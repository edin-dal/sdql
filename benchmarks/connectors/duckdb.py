import sys
from typing import Any, Self

import duckdb
import pandas as pd

from benchmarks.timer import timer
from .connector import Connector, UseConstraintsTypes


class DuckDb(Connector):

    def __enter__(self) -> Self:
        if self.is_log:
            raise NotImplementedError("logging not handled for DuckDB")

        print(f"Connecting to DuckDB (threads={self.threads})")
        self.conn = duckdb.connect(":memory:", read_only=False)
        self.conn.execute(f"PRAGMA threads={self.threads}")

        print("Loading TPCH from disk")
        try:
            self.load_tpch(UseConstraintsTypes.Enable)
        except Exception as e:
            self.__exit__(*sys.exc_info())
            raise e

        return self

    def __exit__(self, *args, **kwargs) -> None:
        self.conn.__exit__(*args, **kwargs)
        print("Disconnected from DuckDB")

    def execute_to_df(self, query: str) -> pd.DataFrame:
        return self.conn.execute(query).df()

    def time(self, query: str) -> float:
        with timer() as time:
            self.conn.execute(query)
        return time()

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
            schema[idx] = (f'"{schema[idx][0]}"', schema[idx][1])
        schema_with_constraints = ", ".join(
            [f"{col_n} {col_t}" for col_n, col_t in schema]
        )
        if use_constraints == UseConstraintsTypes.Enable and primary_keys:
            schema_with_constraints += f", PRIMARY KEY ({', '.join(primary_keys)})"
        if use_constraints == UseConstraintsTypes.Enable and foreign_keys:
            for src_col, dest in foreign_keys.items():
                dest_tbl, dest_col = dest
                schema_with_constraints += (
                    f", FOREIGN KEY ({src_col}) REFERENCES {dest_tbl}({dest_col})"
                )

        self.conn.execute(f"CREATE TABLE {name}({schema_with_constraints})")
        self.conn.execute(f"COPY {name} FROM '{path}' (AUTO_DETECT true)")
