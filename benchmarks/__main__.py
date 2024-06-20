from benches import benchmark_duckdb
from queries import *
from validation import assert_correctness

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
    # (13, q13),
    (14, q14),
    (15, q15),
    (16, q16),
    (17, q17),
    (18, q18),
    (19, q19),
    (20, q20),
    # TODO investigate remaining discrepancies in Q21
    #  (should match if we DON'T use DuckDb's TPCH generator)
    # (21, q21),
    (22, q22),
)

if __name__ == "__main__":
    indices = [i for i, _ in INDICES_AND_QUERIES]
    queries = [q for _, q in INDICES_AND_QUERIES]

    assert_correctness(indices, queries)
    benchmark_duckdb(indices, queries)
