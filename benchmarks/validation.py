import os
from collections import defaultdict

import numpy as np
import pandas as pd
from pandas.api.types import is_numeric_dtype
from pandas.testing import assert_series_equal

from readers import read_sdql_csvs, read_duckdb_csvs, read_hyper_csvs

RTOL = 1.0e-13

# TPCH address and comment are randomised
TPCH_TO_SKIPCOLS = defaultdict(
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


def assert_correctness_duckdb(indices, queries):
    duckdb_dfs = read_duckdb_csvs(indices, queries)
    sdql_dfs = read_sdql_csvs(indices)

    for i, (sdql_df, duckdb_df) in zip(indices, (zip(duckdb_dfs, sdql_dfs))):
        print(f"TPCH {i} - checking correctness vs DuckDB")
        assert_df_equal(sdql_df, duckdb_df, i)

    # double-check results on the old sdqlpy validator
    invalid_queries, unknown_queries = validate_results("duckdb", "sdql")
    assert not unknown_queries, f"DuckDB unknown queries: {unknown_queries}"
    assert not unknown_queries, f"DuckDB invalid queries: {invalid_queries}"


def assert_correctness_hyper(indices, queries):
    hyper_dfs = read_hyper_csvs(indices, queries)
    sdql_dfs = read_sdql_csvs(indices)

    for i, (sdql_df, hyper_df) in zip(indices, (zip(hyper_dfs, sdql_dfs))):
        print(f"TPCH {i} - checking correctness vs Hyper")
        assert_df_equal(sdql_df, hyper_df, i)

    # double-check results on the old sdqlpy validator
    invalid_queries, unknown_queries = validate_results("hyper", "sdql")
    assert not unknown_queries, f"unknown_queries: {unknown_queries}"
    assert not unknown_queries, f"Hyper unknown queries: {unknown_queries}"
    assert not unknown_queries, f"Hyper invalid queries: {invalid_queries}"


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
                assert_series_equal(s1, s2, check_exact=False, rtol=RTOL)
            else:
                if i in skip_cols:
                    continue

                assert_series_equal(s1, s2, check_dtype=True, check_exact=True)
        except AssertionError as e:
            print(f"Failed on TPCH Q{tpch_i} column {i}")
            raise e


# this is the old sdqlpy validator
def validate_results(true_results_path, exp_results_path):
    def are_dfs_equal(df1: pd.DataFrame, df2: pd.DataFrame) -> bool:
        np1, np2 = df1.to_numpy(), df2.to_numpy()
        if np1.shape != np2.shape:
            return False

        flag = True
        for idx in range(np1.shape[1]):
            col1, col2 = np1[:, idx], np2[:, idx]

            if isinstance(col1[0], float) or isinstance(col2[0], float):
                flag &= (
                    np.isclose(
                        np.array(col1, dtype=float),
                        np.array(col2, dtype=float),
                        rtol=RTOL,
                    )
                ).all()
            elif isinstance(col1[0], str):
                col1, col2 = np.char.strip(np.array(col1, dtype=str)), np.char.strip(
                    np.array(col2, dtype=str)
                )
                if (np.char.str_len(col1) > 35).any() or (
                    np.char.str_len(col2) > 35
                ).any:
                    continue
                flag &= (col1 == col2).all()
            else:
                flag &= (col1 == col2).all()
        return flag

    invalid_queries = list()
    unknown_queries = list()

    true_results = {
        filename[:-4]: pd.read_csv(
            os.path.join(true_results_path, filename), header=None, index_col=None
        )
        for filename in os.listdir(true_results_path)
    }

    for filename in sorted(os.listdir(exp_results_path)):
        query_name = filename[:-4]
        try:
            exp_res = pd.read_csv(
                os.path.join(exp_results_path, filename), header=None, index_col=None
            )
            if query_name not in true_results.keys():
                unknown_queries.append(query_name)
            elif not are_dfs_equal(true_results[query_name], exp_res):
                invalid_queries.append(query_name)
        except:
            invalid_queries.append(query_name)

    return invalid_queries, unknown_queries
