import io
import os
import re
from functools import wraps
from typing import Callable, Final, Iterable

import pandas as pd
from pandas.api.types import is_string_dtype

from connectors import HyperTpch, DuckDbTpch

SDQL_RESULTS_DIR: Final[str] = "../src/test/tpch"
SDQL_CSVS_DIR: Final[str] = "sdql"
DUCKDB_CSVS_DIR: Final[str] = "duckdb"
HYPER_CSVS_DIR: Final[str] = "hyper"


def read_sdql_csvs(indices: Iterable[int]) -> list[pd.DataFrame]:
    return read_csvs(
        DUCKDB_CSVS_DIR,
        indices,
        lambda: write_sdql_csvs(indices),
    )


def read_duckdb_csvs(
    indices: Iterable[int], queries: Iterable[str]
) -> list[pd.DataFrame]:
    return read_csvs(
        DUCKDB_CSVS_DIR,
        indices,
        lambda: write_db_csvs(DuckDbTpch, indices, queries, is_hyper=False),
    )


def read_hyper_csvs(
    indices: Iterable[int], queries: Iterable[str]
) -> list[pd.DataFrame]:
    return read_csvs(
        HYPER_CSVS_DIR,
        indices,
        lambda: write_db_csvs(HyperTpch, indices, queries, is_hyper=True),
    )


def read_csvs(
    dir: str,
    indices: Iterable[int],
    write_csvs: Callable[[], None],
) -> list[pd.DataFrame]:
    # always rewrite results to make sure we are not checking stale data
    write_csvs()

    dfs = []
    for i in indices:
        path = os.path.join(dir, f"q{i}.csv")
        df = pd.read_csv(path, header=None)
        dfs.append(df)

    return dfs


def write_db_csvs(
    conn, indices: Iterable[int], queries: Iterable[str], *, is_hyper: bool
) -> None:
    with conn() as db:
        for i, q in zip(indices, queries):
            df = db.query_with_result(q)
            # SDQL results are unordered - so we order all dataframes for comparison
            stable_sort_no_ties(df)
            # Hyper results include trailing whitespaces - so we strip them everywhere
            if is_hyper:
                df = df.apply(lambda s: s.str.rstrip() if is_string_dtype(s) else s)
            path = os.path.join(DUCKDB_CSVS_DIR, f"q{i}.csv")
            df.to_csv(path, header=False, index=False)


def write_sdql_csvs(indices: Iterable[int]) -> None:
    dfs = read_sdql_results(indices)
    for i, df in enumerate(dfs):
        # SDQL results are unordered - so we order all dataframes for comparison
        stable_sort_no_ties(df)
        out_path = os.path.join(SDQL_CSVS_DIR, f"q{i + 1}.csv")
        df.to_csv(out_path, header=False, index=False)


def stable_sort_no_ties(df: pd.DataFrame) -> None:
    df.sort_values(df.columns.tolist(), kind="stable", inplace=True)


def read_sdql_results(indices: Iterable[int]) -> list[pd.DataFrame]:
    dfs = []
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
        dfs.append(df)

    return dfs


def sdql_to_csv(i: int) -> Callable[[str], str]:
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


def unescaped_to_csv(n_cols: int) -> Callable[[str], str]:
    @wraps(unescaped_to_csv)
    def inner(line: str) -> str:
        # assumes the column with commas to escape is the last one
        *cols, escape = relational_to_csv(line).split(",", n_cols - 1)
        escape = f'"{escape}"' if ("," in escape) else escape
        return ",".join(cols + [escape])

    return inner


def scalar_to_csv(line: str) -> str:
    return line


def relational_to_csv(line: str) -> str:
    return RE_RELATIONAL.match(line).group(1)


def q7_to_csv(line: str) -> str:
    return re.sub(RE_Q7, r"\1,\2", line)


def q22_to_csv(line: str) -> str:
    return re.sub(RE_Q22, r"\1,\2", line)


RE_RELATIONAL: Final[re.Pattern] = re.compile("^<([^>]*)>:1$")
RE_Q7: Final[re.Pattern] = re.compile("^<<([^>]*)>,<([^>]*)>>:1$")
RE_Q22: Final[re.Pattern] = re.compile("^([^<]*):<([^>]*)>$")
