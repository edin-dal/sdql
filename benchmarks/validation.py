from collections import defaultdict
from typing import Callable, DefaultDict, Final, Iterable

import pandas as pd
from pandas.api.types import is_numeric_dtype
from pandas.testing import assert_series_equal

from readers import read_sdql_csvs, read_duckdb_csvs, read_hyper_csvs

RTOL: Final[float] = 1.0e-13

# TPCH address and comment are randomised
TPCH_TO_SKIPCOLS: Final[DefaultDict[int, set[int]]] = defaultdict(
    set,
    {
        # s_address, s_comment
        2: {6, 7},
        # c_address, c_comment
        10: {6, 7},
        # s_address
        20: {1},
    },
)


def validate_vs_duckdb(indices: Iterable[int], queries: Iterable[str]) -> None:
    validate_vs("DuckDB", read_duckdb_csvs, indices, queries)


def validate_vs_hyper(indices: Iterable[int], queries: Iterable[str]) -> None:
    validate_vs("Hyper", read_hyper_csvs, indices, queries)


def validate_vs(
    db_name: str,
    read_csvs: Callable[[Iterable[int], Iterable[str]], Iterable[pd.DataFrame]],
    indices: Iterable[int],
    queries: Iterable[str],
) -> None:
    sdql_dfs = read_sdql_csvs(indices)
    dfs = read_csvs(indices, queries)

    for i, (sdql_df, df) in zip(indices, (zip(sdql_dfs, dfs))):
        print(f"TPCH {i} - validating vs {db_name}")
        assert_frames_equal(sdql_df, df, skipcols=TPCH_TO_SKIPCOLS[i])


def assert_frames_equal(
    df1: pd.DataFrame, df2: pd.DataFrame, skipcols: set[int]
) -> None:
    if df1.shape != df2.shape:
        raise ValueError(f"shapes: {df1.shape} != {df2.shape}")

    for i, ((_, s1), (_, s2)) in enumerate(zip(df1.items(), df2.items())):
        if i in skipcols:
            continue

        try:
            if is_numeric_dtype(s1) or is_numeric_dtype(s2):
                s1 = s1.astype(pd.Float64Dtype())
                s2 = s2.astype(pd.Float64Dtype())
                assert_series_equal(s1, s2, check_exact=False, rtol=RTOL)
            else:
                assert_series_equal(s1, s2, check_dtype=True)
        except AssertionError as e:
            print(f"Failed on column {i}")
            raise e
