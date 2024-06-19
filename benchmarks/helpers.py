import io
import os
import re
from pathlib import Path

import pandas as pd
from pandas.api.types import is_numeric_dtype
from pandas.testing import assert_series_equal

import duckdb

SCALING_FACTOR = 1
CONCURRENCY = 1

SDQL_RESULTS_DIR = "../src/test/tpch"
SDQL_CSVS_DIR = "sdql"
DUCKDB_CSVS_DIR = "duckdb"

RE_RELATIONAL = re.compile("^<([^>]*)>:1$")


def assert_df_equal(df1: pd.DataFrame, df2: pd.DataFrame):
    if df1.shape != df2.shape:
        raise ValueError(f"shapes: {df1.shape} != {df2.shape}")

    for (_, s1), (_, s2) in zip(df1.items(), df2.items()):
        if is_numeric_dtype(s1) or is_numeric_dtype(s2):
            try:
                s1 = s1.astype(pd.Float64Dtype())
                s2 = s2.astype(pd.Float64Dtype())
                assert_series_equal(s1, s2, check_exact=False, rtol=1.0e-12)
            except (AssertionError, ValueError):
                raise TypeError(f"dtypes: {s1.dtype} != {s2.dtype}")
        else:
            assert_series_equal(s1, s2, check_dtype=True, check_exact=True)


def read_sdql_csvs(indexes):
    is_file = lambda i: Path(os.path.join(SDQL_CSVS_DIR, f"q{i}.csv")).is_file()
    if not all(map(is_file, indexes)):
        write_sdql_csvs(indexes)

    dfs = []
    for i in indexes:
        path = os.path.join(SDQL_CSVS_DIR, f"q{i}.csv")
        df = pd.read_csv(path, header=None)
        dfs.append(df)

    return dfs


def write_sdql_csvs(indexes):
    for i in indexes:
        in_path = os.path.join(SDQL_RESULTS_DIR, f"q{i}.result")
        lines = []
        with open(in_path) as file:
            while line := file.readline():
                line = relational_to_csv(line.rstrip())
                lines.append(line)
        csv = "\n".join(lines)
        csv_io = io.StringIO(csv)
        df = pd.read_csv(csv_io, header=None)
        # SDQL results are unordered - so we order all dataframes for comparison
        sort_no_ties(df)
        out_path = os.path.join(SDQL_CSVS_DIR, f"q{i}.csv")
        df.to_csv(out_path, header=False, index=False)


def read_duckdb_csvs(indexes, queries):
    is_file = lambda i: Path(os.path.join(DUCKDB_CSVS_DIR, f"q{i}.csv")).is_file()
    if not all(map(is_file, indexes)):
        write_duckdb_csvs(indexes, queries)

    dfs = []
    for i in indexes:
        path = os.path.join(DUCKDB_CSVS_DIR, f"q{i}.csv")
        df = pd.read_csv(path, header=None)
        dfs.append(df)

    return dfs


def write_duckdb_csvs(indexes, queries):
    with duckdb.connect(":memory:") as conn:
        conn.execute(f"PRAGMA threads={CONCURRENCY}")
        print(f"Reading TPCH SF={SCALING_FACTOR}...")
        conn.execute(f"CALL dbgen(sf={SCALING_FACTOR})")
        for i, q in zip(indexes, queries):
            df = conn.execute(q).df()
            # SDQL results are unordered - so we order all dataframes for comparison
            sort_no_ties(df)
            path = os.path.join(DUCKDB_CSVS_DIR, f"q{i}.csv")
            df.to_csv(path, header=False, index=False)


def relational_to_csv(line):
    return RE_RELATIONAL.match(line).group(1)


def sort_no_ties(df):
    df.sort_values(df.columns.tolist(), kind="stable", inplace=True)
