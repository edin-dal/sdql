import io
import os
import re
from functools import wraps

import pandas as pd
from pandas.api.types import is_string_dtype

from connectors import DuckDbTpch, HyperTpch

SDQL_RESULTS_DIR = "../src/test/tpch"
SDQL_CSVS_DIR = "sdql"
DUCKDB_CSVS_DIR = "duckdb"
HYPER_CSVS_DIR = "hyper"

RE_RELATIONAL = re.compile("^<([^>]*)>:1$")
RE_Q7 = re.compile("^<<([^>]*)>,<([^>]*)>>:1$")
RE_Q22 = re.compile("^([^<]*):<([^>]*)>$")


def read_sdql_csvs(indices):
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
            df = db.query_with_result(q)
            # SDQL results are unordered - so we order all dataframes for comparison
            sort_no_ties(df)
            path = os.path.join(DUCKDB_CSVS_DIR, f"q{i}.csv")
            df.to_csv(path, header=False, index=False)


def read_hyper_csvs(indices, queries):
    write_hyper_csvs(indices, queries)

    dfs = []
    for i in indices:
        path = os.path.join(HYPER_CSVS_DIR, f"q{i}.csv")
        df = pd.read_csv(path, header=None)
        dfs.append(df)

    return dfs


def write_hyper_csvs(indices, queries):
    with HyperTpch() as db:
        for i, q in zip(indices, queries):
            df = db.query_with_result(q)
            # SDQL results are unordered - so we order all dataframes for comparison
            sort_no_ties(df)
            # strip trailing whitespaces - inserted by Hyper
            df = df.apply(lambda s: s.str.rstrip() if is_string_dtype(s) else s)
            path = os.path.join(HYPER_CSVS_DIR, f"q{i}.csv")
            df.to_csv(path, header=False, index=False)


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
