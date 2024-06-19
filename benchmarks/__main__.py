from helpers import read_sdql_csvs, read_duckdb_csvs, assert_df_equal
from queries import *

INDICES_AND_QUERIES = (
    (1, q1),
    # (2, q2), // last column has commas
    (3, q3),
    (4, q4),
    (5, q5),
    # (6, q6),  // scalar
    # (7, q7),  // nested tuple
    (8, q8),
    (9, q9),
    # (10, q10), // last column has commas
    (11, q11),
    (12, q12),
    (13, q13),
    # (14, q14), // scalar
    (15, q15),
    (16, q16),
    # (17, q17), // scalar
    (18, q18),
    (19, q19),
    # (20, q20), // last column has commas
    (21, q21),
    # (22, q22),  // map
)

# TODO reconcile diffs in sdqlpy_private SQL queries / TPCH originals / sdqlpy *.sdql

if __name__ == "__main__":
    # # checks SDQL results can't be parsed into csv
    # for i, q in INDICES_AND_QUERIES:
    #     try:
    #         write_sdql_csvs([i])
    #     except:
    #         print(f"ignore: {i}")

    indexes = (
        1,
        3,
        5,
        9,
        18,
    )
    queries = [eval(f"q{i}") for i in indexes]

    # # forces a rewrite
    # from helpers import write_duckdb_csvs, write_sdql_csvs
    #
    # write_duckdb_csvs(indexes, queries)
    # write_sdql_csvs(indexes)

    duckdb_dfs = read_duckdb_csvs(indexes, queries)
    sdql_dfs = read_sdql_csvs(indexes)

    for i, (sdql_df, duckdb_df) in zip(indexes, (zip(duckdb_dfs, sdql_dfs))):
        print(f"TPCH {i}")
        assert_df_equal(sdql_df, duckdb_df)
