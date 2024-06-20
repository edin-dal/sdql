from helpers import (
    read_sdql_csvs,
    read_duckdb_csvs,
    assert_df_equal,
    benchmark_duckdb,
)
from queries import *

INDICES_AND_QUERIES = (
    (1, q1),
    (2, q2),
    (3, q3),
    (4, q4),
    (5, q5),
    (6, q6),
    (7, q7),
    (8, q8),
    (9, q9),
    (10, q10),
    (11, q11),
    (12, q12),
    # TODO should match if we DON'T use DuckDb's TPCH generator
    # (13, q13),  # TODO validate manually
    (14, q14),
    (15, q15),
    (16, q16),
    (17, q17),
    (18, q18),
    (19, q19),
    (20, q20),
    # TODO investigate remaining discrepancies in Q21
    # TODO should match if we DON'T use DuckDb's TPCH generator
    # (21, q21),  # TODO validate manually
    (22, q22),
)

# TODO reconcile diffs in sdqlpy_private SQL queries / TPCH originals / sdqlpy *.sdql


def assert_correctness(indices, queries):
    duckdb_dfs = read_duckdb_csvs(indices, queries)
    sdql_dfs = read_sdql_csvs(indices)

    for i, (sdql_df, duckdb_df) in zip(indices, (zip(duckdb_dfs, sdql_dfs))):
        print(f"TPCH {i} - checking correctness")
        assert_df_equal(sdql_df, duckdb_df, i)


if __name__ == "__main__":
    indices = [i for i, _ in INDICES_AND_QUERIES]
    queries = [q for _, q in INDICES_AND_QUERIES]

    assert_correctness(indices, queries)
    # benchmark_duckdb(indices, queries)
