import os
import re
import sys
from typing import Final

import pandas as pd
from tableauhyperapi import (
    escape_string_literal,
    HyperProcess,
    Telemetry,
    Connection,
    CreateMode,
    SqlType,
    TableDefinition,
    NOT_NULLABLE,
    NULLABLE,
    HyperException,
)

from benchmarks.timer import timer
from .connector import Connector, THREADS, UseConstraintsTypes, SEC_TO_MS


class Hyper(Connector):

    DATABASE: Final[str] = "db.hyper"

    TYPE_MAPPING: Final[dict[str, SqlType]] = {
        "INTEGER": SqlType.int,
        "CHAR": SqlType.char,
        "VARCHAR": SqlType.varchar,
        "DOUBLE": SqlType.double,
        "DATE": SqlType.date,
    }

    def __enter__(self):
        print(f"Connecting to Hyper")
        parameters = dict(
            log_config="",
            max_query_size="10000000000",
            hard_concurrent_query_thread_limit=str(THREADS),
            initial_compilation_mode="o",
        )
        self.server = HyperProcess(
            Telemetry.DO_NOT_SEND_USAGE_DATA_TO_TABLEAU, "User", None, parameters
        )
        self.conn = Connection(
            self.server.endpoint, self.DATABASE, CreateMode.CREATE_AND_REPLACE
        )

        print(f"Loading TPCH from disk")
        try:
            self.load_tpch(UseConstraintsTypes.Enable)
        except HyperException as e:
            self.__exit__(*sys.exc_info())
            if e.main_message.startswith("too many columns in input file"):
                raise IOError(
                    "Tables shouldn't have EOL `|` character - run fix in README"
                ) from e
            else:
                raise e

        return self

    def __exit__(self, exc_type, exc_val, exc_tb):
        self.conn.__exit__(exc_type, exc_val, exc_tb)
        self.server.__exit__(exc_type, exc_val, exc_tb)
        print(f"Disconnected from Hyper")
        try:
            os.remove(self.DATABASE)
        except FileNotFoundError:
            pass

    def execute(self, query: str) -> pd.DataFrame:
        query = self._revise_query(query)
        hyper_res = self.conn.execute_query(query)
        result_df = pd.DataFrame(list(hyper_res))
        hyper_res.close()
        return result_df

    def time(self, query: str) -> float:
        query = self._revise_query(query)
        with timer() as time:
            hyper_res = self.conn.execute_query(query)
        hyper_res.close()
        return SEC_TO_MS * time()

    def load_table(
        self,
        name: str,
        path: str,
        schema: list[tuple[str, str], ...],
        primary_keys: None | list[str, ...] = None,
        foreign_keys: None | dict[str, tuple[str, str]] = None,
        use_constraints: UseConstraintsTypes = UseConstraintsTypes.Disable,
        header: bool = False,
        delimiter: str = "|",
    ) -> None:
        for idx in range(len(schema)):
            if "(" in schema[idx][1] and ")" in schema[idx][1]:
                lll, rrr = schema[idx][1].index("("), schema[idx][1].index(")")
                schema[idx] = (
                    schema[idx][0],
                    self.TYPE_MAPPING[schema[idx][1][:lll]](
                        int(schema[idx][1][lll + 1 : rrr])
                    ),
                )
            else:
                schema[idx] = (schema[idx][0], self.TYPE_MAPPING[schema[idx][1]]())

        table_def = TableDefinition(
            table_name=name,
            columns=[
                TableDefinition.Column(
                    name=col_n,
                    type=col_t,
                    nullability=(
                        NOT_NULLABLE
                        if use_constraints == UseConstraintsTypes.Enable
                        and primary_keys
                        and col_n in primary_keys
                        else NULLABLE
                    ),
                )
                for col_n, col_t in schema
            ],
        )

        self.conn.catalog.create_table(table_definition=table_def)

        if use_constraints == UseConstraintsTypes.Enable and primary_keys:
            self.conn.execute_command(
                f"ALTER TABLE {name} ADD ASSUMED PRIMARY KEY ({','.join(primary_keys)})"
            )

        if use_constraints == UseConstraintsTypes.Enable and foreign_keys:
            for src_col, dest in foreign_keys.items():
                dest_tbl, dest_col = dest
                self.conn.execute_command(
                    f"ALTER TABLE {name} ADD ASSUMED FOREIGN KEY ({src_col}) REFERENCES {dest_tbl}({dest_col})"
                )

        self.conn.execute_command(
            f"COPY {table_def.table_name} from {escape_string_literal(path)} "
            f"with (format => 'csv', delimiter => '{delimiter}', header => {str(header).lower()})"
        )

    @classmethod
    def _revise_query(cls, query: str):
        query = re.sub(r"MEAN\((\w+|\w+\.\w+)\)", r"AVG(\1)", query)
        query = cls._revise_slice(query)
        return query

    @staticmethod
    def _revise_slice(query: str):
        matches = re.findall(r"(\w+|\w+\.\w+)\[(\d+):(\d+)]", query)
        for match in matches:
            var_name, start_idx, end_idx = match[0], int(match[1]), int(match[2])
            query = query.replace(
                f"{var_name}[{start_idx}:{end_idx}]",
                f"substring({var_name} from {start_idx + 1} for {end_idx - start_idx})",
            )
        return query
