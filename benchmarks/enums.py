import enum
import os

data_path = os.path.join(os.path.dirname(__file__), "data")


class DataTypes(enum.Enum):
    TPCH = ("tpch", "../datasets")


class DBTypes(enum.Enum):
    DuckDB = "duckdb"
    Hyper = "hyper"


class UseConstraintsTypes(enum.Enum):
    Enable = "enable"
    Disable = "disable"


class IDGenerationTypes(enum.Enum):
    Smart = "smart"
    All = "all"
