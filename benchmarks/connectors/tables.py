import os
from dataclasses import dataclass, field
from typing import Final


TPCH_DATASETS: Final[str] = os.path.normpath(
    os.path.join(os.path.dirname(__file__), "../../src/test/tpch/data/SF_1")
)


@dataclass(frozen=True)
class Table:
    name: str
    path: str
    schema: list[tuple[str, str]]
    primary_keys: list[str] = field(default_factory=list)
    foreign_keys: dict[str, tuple[str, str]] = field(default_factory=dict)


REGION: Final[Table] = Table(
    name="region",
    path=f"{TPCH_DATASETS}/region.tbl",
    schema=[
        ("r_regionkey", "INTEGER"),
        ("r_name", "CHAR(25)"),
        ("r_comment", "VARCHAR(152)"),
    ],
    primary_keys=["r_regionkey"],
)

NATION: Final[Table] = Table(
    name="nation",
    path=f"{TPCH_DATASETS}/nation.tbl",
    schema=[
        ("n_nationkey", "INTEGER"),
        ("n_name", "CHAR(25)"),
        ("n_regionkey", "INTEGER"),
        ("n_comment", "VARCHAR(152)"),
    ],
    primary_keys=["n_nationkey"],
    foreign_keys={"n_regionkey": ("region", "r_regionkey")},
)

SUPPLIER: Final[Table] = Table(
    name="supplier",
    path=f"{TPCH_DATASETS}/supplier.tbl",
    schema=[
        ("s_suppkey", "INTEGER"),
        ("s_name", "CHAR(25)"),
        ("s_address", "VARCHAR(40)"),
        ("s_nationkey", "INTEGER"),
        ("s_phone", "CHAR(15)"),
        ("s_acctbal", "DOUBLE"),
        ("s_comment", "VARCHAR(101)"),
    ],
    primary_keys=["s_suppkey"],
    foreign_keys={"s_nationkey": ("nation", "n_nationkey")},
)

CUSTOMER: Final[Table] = Table(
    name="customer",
    path=f"{TPCH_DATASETS}/customer.tbl",
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
    primary_keys=["c_custkey"],
    foreign_keys={"c_nationkey": ("nation", "n_nationkey")},
)

PART: Final[Table] = Table(
    name="part",
    path=f"{TPCH_DATASETS}/part.tbl",
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
    primary_keys=["p_partkey"],
)

PARTSUPP: Final[Table] = Table(
    name="partsupp",
    path=f"{TPCH_DATASETS}/partsupp.tbl",
    schema=[
        ("ps_partkey", "INTEGER"),
        ("ps_suppkey", "INTEGER"),
        ("ps_availqty", "INTEGER"),
        ("ps_supplycost", "DOUBLE"),
        ("ps_comment", "VARCHAR(199)"),
    ],
    primary_keys=["ps_partkey", "ps_suppkey"],
    foreign_keys={
        "ps_partkey": ("part", "p_partkey"),
        "ps_suppkey": ("supplier", "s_suppkey"),
    },
)

ORDERS: Final[Table] = Table(
    name="orders",
    path=f"{TPCH_DATASETS}/orders.tbl",
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
    primary_keys=["o_orderkey"],
    foreign_keys={"o_custkey": ("customer", "c_custkey")},
)

LINEITEM: Final[Table] = Table(
    name="lineitem",
    path=f"{TPCH_DATASETS}/lineitem.tbl",
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
    primary_keys=["l_orderkey", "l_linenumber"],
    foreign_keys={
        "l_orderkey": ("orders", "o_orderkey"),
        "l_partkey, l_suppkey": ("partsupp", "ps_partkey, ps_suppkey"),
    },
)
