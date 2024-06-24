import pandas as pd

from benches import benchmark_duckdb, benchmark_hyper, benchmark_sdql
from extractor import extract_hyper_log_times
from queries import *
from validation import validate_vs_duckdb, validate_vs_hyper

# Disable hyper-threading on relevant systems
SMT_FILE = "/sys/devices/system/cpu/smt/control"
os.system(f"if [ -f {SMT_FILE} ]; then echo off > {SMT_FILE}; fi")

# for DuckDB and Hyper - SDQL is single-threaded
THREADS = 1

RUNS = 5

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

if __name__ == "__main__":
    indices = [i for i, _ in INDICES_AND_QUERIES]
    queries = [q for _, q in INDICES_AND_QUERIES]

    res = pd.Series(indices, name="Query").to_frame()

    validate_vs_duckdb(indices, queries, THREADS)
    res["Validated (DuckDB)"] = pd.Series([True for _ in INDICES_AND_QUERIES])

    validate_vs_hyper(indices, queries, THREADS)
    res["Validated (Hyper)"] = pd.Series([True for _ in INDICES_AND_QUERIES])

    res["SDQL (ms)"] = pd.Series(benchmark_sdql(indices, RUNS))
    res["DuckDB (ms)"] = pd.Series(benchmark_duckdb(indices, queries, THREADS, RUNS))
    res["Hyper (ms)"] = pd.Series(benchmark_hyper(indices, queries, THREADS, RUNS))
    # we use perf_counter rather than process_time
    # accordingly elapsed times are more appropriate than execution times
    elapsed_times, _execution_times = extract_hyper_log_times(indices, queries, THREADS)
    res["Hyper log (ms)"] = pd.Series(elapsed_times)
    res.to_csv("benchmarks.csv", index=False)
