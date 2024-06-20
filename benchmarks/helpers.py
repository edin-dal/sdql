import io
import os
import re
from collections import defaultdict
from contextlib import contextmanager
from functools import wraps
from itertools import repeat
from statistics import mean, pstdev
from time import process_time

import pandas as pd
from pandas.api.types import is_numeric_dtype
from pandas.testing import assert_series_equal

import duckdb

# TPCH address and comment are randomised
TPCH_TO_SKIPCOLS = defaultdict(
    set,
    {
        # FIXME s_phone is fine, was caught in escaping quotes
        # s_address, s_phone, s_comment
        2: {5, 6, 7},
        # FIXME c_phone is fine, was caught in escaping quotes
        # c_address, c_phone, c_comment
        10: {5, 6, 7},
        # TODO should match if we DON'T use DuckDb's TPCH generator
        # s_address
        15: {2},
        # s_address
        20: {1},
    },
)

SCALING_FACTOR = 1
CONCURRENCY = 1

SDQL_RESULTS_DIR = "../src/test/tpch"
SDQL_CSVS_DIR = "sdql"
DUCKDB_CSVS_DIR = "duckdb"

RE_RELATIONAL = re.compile("^<([^>]*)>:1$")
RE_Q7 = re.compile("^<<([^>]*)>,<([^>]*)>>:1$")
RE_Q22 = re.compile("^([^<]*):<([^>]*)>$")

SEC_TO_MS = 1_000

N_ITERS = 5


def round_ms(secs):
    return round(secs * SEC_TO_MS)


@contextmanager
def Time():
    t1 = t2 = process_time()
    yield lambda: t2 - t1
    t2 = process_time()


def benchmark_duckdb(indices, queries):
    db_times = []
    with DuckDbTpch() as db:
        for i, q in zip(indices, queries):
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


def assert_df_equal(df1: pd.DataFrame, df2: pd.DataFrame, tpch_i: int):
    if df1.shape != df2.shape:
        raise ValueError(f"shapes: {df1.shape} != {df2.shape}")

    skip_cols = TPCH_TO_SKIPCOLS[tpch_i]

    for i, ((_, s1), (_, s2)) in enumerate(zip(df1.items(), df2.items())):
        try:
            if is_numeric_dtype(s1) or is_numeric_dtype(s2):
                assert i not in skip_cols
                s1 = s1.astype(pd.Float64Dtype())
                s2 = s2.astype(pd.Float64Dtype())
                assert_series_equal(s1, s2, check_exact=False, rtol=1.0e-13)
            else:
                if i in skip_cols:
                    continue

                assert_series_equal(s1, s2, check_dtype=True, check_exact=True)
        except AssertionError as e:
            print(f"Failed on TPCH Q{tpch_i} column {i}")
            raise e


def read_sdql_csvs(indices):
    # TODO safer to always do this?
    # from pathlib import Path
    # is_file = lambda i: Path(os.path.join(SDQL_CSVS_DIR, f"q{i}.csv")).is_file()
    # if not all(map(is_file, indices)):
    write_sdql_csvs(indices)

    dfs = []
    for i in indices:
        path = os.path.join(SDQL_CSVS_DIR, f"q{i}.csv")
        df = pd.read_csv(path, header=None)
        dfs.append(df)

    return dfs


def write_sdql_csvs(indices):
    for i in indices:
        in_path = os.path.join(SDQL_RESULTS_DIR, f"q{i}.result")
        lines = []
        with open(in_path) as file:
            while line := file.readline():
                line = sdql_to_csv(i)(line.rstrip())
                lines.append(line)
        csv = "\n".join(lines)
        csv_io = io.StringIO(csv)
        df = pd.read_csv(csv_io, header=None)
        # SDQL results are unordered - so we order all dataframes for comparison
        sort_no_ties(df)
        out_path = os.path.join(SDQL_CSVS_DIR, f"q{i}.csv")
        df.to_csv(out_path, header=False, index=False)


def read_duckdb_csvs(indices, queries):
    # TODO safer to always do this?
    # from pathlib import Path
    # is_file = lambda i: Path(os.path.join(DUCKDB_CSVS_DIR, f"q{i}.csv")).is_file()
    # if not all(map(is_file, indices)):
    write_duckdb_csvs(indices, queries)

    dfs = []
    for i in indices:
        path = os.path.join(DUCKDB_CSVS_DIR, f"q{i}.csv")
        df = pd.read_csv(path, header=None)
        dfs.append(df)

    return dfs


def write_duckdb_csvs(indices, queries):
    with DuckDbTpch() as db:
        for i, q in zip(indices, queries):
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


def sdql_to_csv(i):
    match i:
        case 2 | 10:
            return unescaped_to_csv(n_cols=8)
        case 6 | 14 | 17:
            return scalar_to_csv
        case 7:
            return q7_to_csv
        case 20:
            return unescaped_to_csv(n_cols=2)
        case 22:
            return q22_to_csv
        case _:
            return relational_to_csv


def unescaped_to_csv(n_cols):
    @wraps(unescaped_to_csv)
    def inner(line):
        # assumes the column with commas to escape is the last one
        *cols, escape = relational_to_csv(line).split(",", n_cols - 1)
        escape = f'"{escape}"' if ("," in escape) else escape
        return ",".join(cols + [escape])

    return inner


def q7_to_csv(line):
    return re.sub(RE_Q7, r"\1,\2", line)


def q22_to_csv(line):
    return re.sub(RE_Q22, r"\1,\2", line)


def scalar_to_csv(line):
    return line


def relational_to_csv(line):
    return RE_RELATIONAL.match(line).group(1)


def sort_no_ties(df):
    df.sort_values(df.columns.tolist(), kind="stable", inplace=True)
