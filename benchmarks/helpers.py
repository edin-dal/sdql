import io
import os
import re
from contextlib import contextmanager
from itertools import repeat
from statistics import mean, pstdev
from time import process_time

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

SEC_TO_MS = 1_000

N_ITERS = 5


def round_ms(secs):
    return round(secs * SEC_TO_MS)


@contextmanager
def Time():
    t1 = t2 = process_time()
    yield lambda: t2 - t1
    t2 = process_time()


def benchmark_duckdb(indexes, queries):
    db_times = []
    with DuckDbTpch() as db:
        for i, q in zip(indexes, queries):
            q_times = []
            for _ in repeat(None, N_ITERS):
                with Time() as time:
                    db.execute(q)
                q_times.append(time())
            mean_ms = round_ms(mean(q_times))
            std_ms = round_ms(pstdev(q_times))
            print(f"DuckDb q{i}: mean {mean_ms} ms (std {std_ms} ms - {N_ITERS} runs)")
            db_times.append(mean_ms)

    return db_times


def assert_df_equal(df1: pd.DataFrame, df2: pd.DataFrame):
    if df1.shape != df2.shape:
        raise ValueError(f"shapes: {df1.shape} != {df2.shape}")

    for (_, s1), (_, s2) in zip(df1.items(), df2.items()):
        if is_numeric_dtype(s1) or is_numeric_dtype(s2):
            try:
                promo1 = s1.astype(pd.Float64Dtype())
                promo2 = s2.astype(pd.Float64Dtype())
                assert_series_equal(promo1, promo2, check_exact=False, rtol=1.0e-12)
            except (AssertionError, ValueError):
                raise TypeError(f"dtypes: {s1.dtype} != {s2.dtype}")
        else:
            assert_series_equal(s1, s2, check_dtype=True, check_exact=True)


def read_sdql_csvs(indexes):
    # TODO safer to always do this?
    # from pathlib import Path
    # is_file = lambda i: Path(os.path.join(SDQL_CSVS_DIR, f"q{i}.csv")).is_file()
    # if not all(map(is_file, indexes)):
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
    # TODO safer to always do this?
    # from pathlib import Path
    # is_file = lambda i: Path(os.path.join(DUCKDB_CSVS_DIR, f"q{i}.csv")).is_file()
    # if not all(map(is_file, indexes)):
    write_duckdb_csvs(indexes, queries)

    dfs = []
    for i in indexes:
        path = os.path.join(DUCKDB_CSVS_DIR, f"q{i}.csv")
        df = pd.read_csv(path, header=None)
        dfs.append(df)

    return dfs


def write_duckdb_csvs(indexes, queries):
    with DuckDbTpch() as db:
        for i, q in zip(indexes, queries):
            df = db.execute(q).df()
            # SDQL results are unordered - so we order all dataframes for comparison
            sort_no_ties(df)
            path = os.path.join(DUCKDB_CSVS_DIR, f"q{i}.csv")
            df.to_csv(path, header=False, index=False)


class DuckDbTpch:

    def __init__(self):
        self.conn = duckdb.connect(":memory:")
        self.conn.execute(f"PRAGMA threads={CONCURRENCY}")
        print(f"Reading TPCH SF={SCALING_FACTOR}...")
        self.conn.execute(f"CALL dbgen(sf={SCALING_FACTOR})")

    def __enter__(self):
        return self.conn

    def __exit__(self, exc_type, exc_val, exc_tb):
        self.conn.__exit__(exc_type, exc_val, exc_tb)

    # TODO __call__ triggers __exit__ ?
    def execute(self, *args, **kwargs):
        return self.conn.execute(*args, **kwargs)


def relational_to_csv(line):
    return RE_RELATIONAL.match(line).group(1)


def sort_no_ties(df):
    df.sort_values(df.columns.tolist(), kind="stable", inplace=True)
