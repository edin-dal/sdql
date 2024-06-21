import os
from typing import Callable
from typing import Dict, List, Tuple

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
)

import duckdb
from benchmarks.enums import UseConstraintsTypes

THREADS = 1

DATA_PATH = os.path.join(os.path.dirname(__file__), "../datasets/tpch")


class DuckDbTpch:

    def __init__(self):
        self.conn = duckdb.connect(":memory:", read_only=False)
        self.conn.execute(f"PRAGMA threads={THREADS}")
        print(f"Reading TPCH from disk")
        load_tpch(self._table_loader, UseConstraintsTypes.Enable)

    def __enter__(self):
        return self.conn

    def __exit__(self, exc_type, exc_val, exc_tb):
        self.conn.__exit__(exc_type, exc_val, exc_tb)

    def execute(self, *args, **kwargs):
        return self.conn.execute(*args, **kwargs)

    def _table_loader(
        self,
        table_name: str,
        schema: List[Tuple[str, str]],
        file_path: str,
        use_constraints: UseConstraintsTypes = UseConstraintsTypes.Disable,
        primary_key: List[str] = None,
        foreign_keys: Dict[str, Tuple[str, str]] = None,
    ):
        for idx in range(len(schema)):
            schema[idx] = (f'"{schema[idx][0]}"', schema[idx][1])
        schema_with_constraints = ", ".join(
            [f"{col_n} {col_t}" for col_n, col_t in schema]
        )
        if use_constraints == UseConstraintsTypes.Enable and primary_key:
            schema_with_constraints += f", PRIMARY KEY ({', '.join(primary_key)})"
        if use_constraints == UseConstraintsTypes.Enable and foreign_keys:
            for src_col, dest in foreign_keys.items():
                dest_tbl, dest_col = dest
                schema_with_constraints += (
                    f", FOREIGN KEY ({src_col}) REFERENCES {dest_tbl}({dest_col})"
                )

        self.conn.execute(f"CREATE TABLE {table_name}({schema_with_constraints})")
        self.conn.execute(f"COPY {table_name} FROM '{file_path}' (AUTO_DETECT true)")


class HyperTpch:

    def __init__(self, db_name="db", version="o"):
        parameters = {
            "log_config": "",
            "max_query_size": "10000000000",
            "hard_concurrent_query_thread_limit": str(THREADS),
            "initial_compilation_mode": version,
        }
        self.server = HyperProcess(
            Telemetry.DO_NOT_SEND_USAGE_DATA_TO_TABLEAU, "User", None, parameters
        )
        self.conn = Connection(
            self.server.endpoint, f"{db_name}.hyper", CreateMode.CREATE_AND_REPLACE
        )

    def __enter__(self):
        return self.server, self.conn

    def __exit__(self, exc_type, exc_val, exc_tb):
        self.conn.__exit__(exc_type, exc_val, exc_tb)
        self.server.__exit__(exc_type, exc_val, exc_tb)

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


def load_tpch(table_loader: Callable, use_constraints: UseConstraintsTypes):
    table_loader(
        table_name="region",
        file_path=f"{DATA_PATH}/region.tbl",
        schema=[
            ("r_regionkey", "INTEGER"),
            ("r_name", "CHAR(25)"),
            ("r_comment", "VARCHAR(152)"),
        ],
        use_constraints=use_constraints,
        primary_key=["r_regionkey"],
    )
    table_loader(
        table_name="nation",
        file_path=f"{DATA_PATH}/nation.tbl",
        schema=[
            ("n_nationkey", "INTEGER"),
            ("n_name", "CHAR(25)"),
            ("n_regionkey", "INTEGER"),
            ("n_comment", "VARCHAR(152)"),
        ],
        use_constraints=use_constraints,
        primary_key=["n_nationkey"],
        foreign_keys={"n_regionkey": ("region", "r_regionkey")},
    )
    table_loader(
        table_name="supplier",
        file_path=f"{DATA_PATH}/supplier.tbl",
        schema=[
            ("s_suppkey", "INTEGER"),
            ("s_name", "CHAR(25)"),
            ("s_address", "VARCHAR(40)"),
            ("s_nationkey", "INTEGER"),
            ("s_phone", "CHAR(15)"),
            ("s_acctbal", "DOUBLE"),
            ("s_comment", "VARCHAR(101)"),
        ],
        use_constraints=use_constraints,
        primary_key=["s_suppkey"],
        foreign_keys={"s_nationkey": ("nation", "n_nationkey")},
    )
    table_loader(
        table_name="customer",
        file_path=f"{DATA_PATH}/customer.tbl",
        schema=[
            ("c_custkey", "INTEGER"),
            ("c_name", "VARCHAR(25)"),
            ("c_address", "VARCHAR(40)"),
            ("c_nationkey", "INTEGER"),
            ("c_phone", "CHAR(15)"),
            ("c_acctbal", "DOUBLE"),
            ("c_mktsegment", "CHAR(10)"),
            ("c_comment", "VARCHAR(117)"),
        ],
        use_constraints=use_constraints,
        primary_key=["c_custkey"],
        foreign_keys={"c_nationkey": ("nation", "n_nationkey")},
    )
    table_loader(
        table_name="part",
        file_path=f"{DATA_PATH}/part.tbl",
        schema=[
            ("p_partkey", "INTEGER"),
            ("p_name", "VARCHAR(55)"),
            ("p_mfgr", "CHAR(25)"),
            ("p_brand", "CHAR(10)"),
            ("p_type", "VARCHAR(25)"),
            ("p_size", "INTEGER"),
            ("p_container", "CHAR(10)"),
            ("p_retailprice", "DOUBLE"),
            ("p_comment", "VARCHAR(23)"),
        ],
        use_constraints=use_constraints,
        primary_key=["p_partkey"],
    )
    table_loader(
        table_name="partsupp",
        file_path=f"{DATA_PATH}/partsupp.tbl",
        schema=[
            ("ps_partkey", "INTEGER"),
            ("ps_suppkey", "INTEGER"),
            ("ps_availqty", "INTEGER"),
            ("ps_supplycost", "DOUBLE"),
            ("ps_comment", "VARCHAR(199)"),
        ],
        use_constraints=use_constraints,
        primary_key=["ps_partkey", "ps_suppkey"],
        foreign_keys={
            "ps_partkey": ("part", "p_partkey"),
            "ps_suppkey": ("supplier", "s_suppkey"),
        },
    )
    table_loader(
        table_name="orders",
        file_path=f"{DATA_PATH}/orders.tbl",
        schema=[
            ("o_orderkey", "INTEGER"),
            ("o_custkey", "INTEGER"),
            ("o_orderstatus", "CHAR(1)"),
            ("o_totalprice", "DOUBLE"),
            ("o_orderdate", "DATE"),
            ("o_orderpriority", "CHAR(15)"),
            ("o_clerk", "CHAR(15)"),
            ("o_shippriority", "INTEGER"),
            ("o_comment", "VARCHAR(79)"),
        ],
        use_constraints=use_constraints,
        primary_key=["o_orderkey"],
        foreign_keys={"o_custkey": ("customer", "c_custkey")},
    )
    table_loader(
        table_name="lineitem",
        file_path=f"{DATA_PATH}/lineitem.tbl",
        schema=[
            ("l_orderkey", "INTEGER"),
            ("l_partkey", "INTEGER"),
            ("l_suppkey", "INTEGER"),
            ("l_linenumber", "INTEGER"),
            ("l_quantity", "DOUBLE"),
            ("l_extendedprice", "DOUBLE"),
            ("l_discount", "DOUBLE"),
            ("l_tax", "DOUBLE"),
            ("l_returnflag", "CHAR(1)"),
            ("l_linestatus", "CHAR(1)"),
            ("l_shipdate", "DATE"),
            ("l_commitdate", "DATE"),
            ("l_receiptdate", "DATE"),
            ("l_shipinstruct", "CHAR(25)"),
            ("l_shipmode", "CHAR(10)"),
            ("l_comment", "VARCHAR(44)"),
        ],
        use_constraints=use_constraints,
        primary_key=["l_orderkey", "l_linenumber"],
        foreign_keys={
            "l_orderkey": ("orders", "o_orderkey"),
            "l_partkey, l_suppkey": ("partsupp", "ps_partkey, ps_suppkey"),
        },
    )
