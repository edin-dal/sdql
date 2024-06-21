from typing import Dict, List, Tuple

from tableauhyperapi import *

from benchmarks.enums import UseConstraintsTypes


class HyperDataLoader:
    def __init__(self, db_name="db", threads=1, version="o", **kwargs):
        parameters = {
            "log_config": "",
            "max_query_size": "10000000000",
            "hard_concurrent_query_thread_limit": str(threads),
            "initial_compilation_mode": version,
        }
        self.server = HyperProcess(
            Telemetry.DO_NOT_SEND_USAGE_DATA_TO_TABLEAU, "User", None, parameters
        )
        self.conn = Connection(
            self.server.endpoint, f"{db_name}.hyper", CreateMode.CREATE_AND_REPLACE
        )
        # TODO linux only
        # os.system("echo off > /sys/devices/system/cpu/smt/control")

    @property
    def db_args(self):
        return self.server, self.conn

    def close(self):
        self.conn.close()
        self.server.close()

    def _table_loader(
        self,
        table_name: str,
        schema: List[Tuple[str, str]],
        file_path: str,
        use_constraints: UseConstraintsTypes = UseConstraintsTypes.Disable,
        primary_key: List[str] = None,
        foreign_keys: Dict[str, Tuple[str, str]] = None,
        header: bool = False,
        delimiter: str = "|",
    ):
        type_mapping = {
            "INTEGER": SqlType.int,
            "CHAR": SqlType.char,
            "VARCHAR": SqlType.varchar,
            "DOUBLE": SqlType.double,
            "DATE": SqlType.date,
        }
        for idx in range(len(schema)):
            if "(" in schema[idx][1] and ")" in schema[idx][1]:
                lll, rrr = schema[idx][1].index("("), schema[idx][1].index(")")
                schema[idx] = (
                    schema[idx][0],
                    type_mapping[schema[idx][1][:lll]](
                        int(schema[idx][1][lll + 1 : rrr])
                    ),
                )
            else:
                schema[idx] = (schema[idx][0], type_mapping[schema[idx][1]]())

        table_def = TableDefinition(
            table_name=table_name,
            columns=[
                TableDefinition.Column(
                    name=col_n,
                    type=col_t,
                    nullability=(
                        NOT_NULLABLE
                        if use_constraints == UseConstraintsTypes.Enable
                        and primary_key
                        and col_n in primary_key
                        else NULLABLE
                    ),
                )
                for col_n, col_t in schema
            ],
        )

        self.conn.catalog.create_table(table_definition=table_def)

        if use_constraints == UseConstraintsTypes.Enable and primary_key:
            self.conn.execute_command(
                f"ALTER TABLE {table_name} ADD ASSUMED PRIMARY KEY ({','.join(primary_key)})"
            )

        if use_constraints == UseConstraintsTypes.Enable and foreign_keys:
            for src_col, dest in foreign_keys.items():
                dest_tbl, dest_col = dest
                self.conn.execute_command(
                    f"ALTER TABLE {table_name} ADD ASSUMED FOREIGN KEY ({src_col}) REFERENCES {dest_tbl}({dest_col})"
                )

        self.conn.execute_command(
            f"COPY {table_def.table_name} from {escape_string_literal(file_path)} "
            f"with (format => 'csv', delimiter => '{delimiter}', header => {str(header).lower()})"
        )
