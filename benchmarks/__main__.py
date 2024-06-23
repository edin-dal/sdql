import pandas as pd

from benches import benchmark_duckdb, benchmark_hyper, benchmark_sdql
from queries import *
from validation import validate_vs_duckdb, validate_vs_hyper

RESULTS_FILE = "benchmarks.csv"

THREADS = 1

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
    (13, q13),
    (14, q14),
    (15, q15),
    (16, q16),
    (17, q17),
    (18, q18),
    (19, q19),
    (20, q20),
    # FIXME
    # (21, q21),
    (22, q22),
)

# TODO Hyper benchmark numbers are unbelievably low

if __name__ == "__main__":
    indices = [i for i, _ in INDICES_AND_QUERIES]
    queries = [q for _, q in INDICES_AND_QUERIES]

    results = pd.Series(indices, name="Query").to_frame()

    validate_vs_duckdb(indices, queries, THREADS)
    results["Validated (DuckDB)"] = pd.Series([True for _ in INDICES_AND_QUERIES])

    validate_vs_hyper(indices, queries, THREADS)
    results["Validated (Hyper)"] = pd.Series([True for _ in INDICES_AND_QUERIES])

    results["SDQL (ms)"] = pd.Series(benchmark_sdql(indices))
    results["DuckDB (ms)"] = pd.Series(benchmark_duckdb(indices, queries, THREADS))
    results["Hyper (ms)"] = pd.Series(benchmark_hyper(indices, queries, THREADS))

    results.to_csv(RESULTS_FILE, index=False)
