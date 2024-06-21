from itertools import repeat
from statistics import mean, pstdev

from connectors import DuckDbTpch, HyperTpch

N_ITERS = 5


def benchmark_duckdb(indices, queries):
    db_times = []
    with DuckDbTpch() as db:
        for i, q in zip(indices, queries):
            q_times = []
            for _ in repeat(None, N_ITERS):
                t = db.query_with_time(q)
                q_times.append(t)
            mean_ms = round(mean(q_times))
            std_ms = round(pstdev(q_times))
            print(f"DuckDb q{i}: mean {mean_ms} ms (std {std_ms} ms - {N_ITERS} runs)")
            db_times.append(mean_ms)

    return db_times


def benchmark_hyper(indices, queries):
    db_times = []
    with HyperTpch() as db:
        for i, q in zip(indices, queries):
            q_times = []
            for _ in repeat(None, N_ITERS):
                t = db.query_with_time(q)
                q_times.append(t)
            mean_ms = round(mean(q_times))
            std_ms = round(pstdev(q_times))
            print(f"Hyper q{i}: mean {mean_ms} ms (std {std_ms} ms - {N_ITERS} runs)")
            db_times.append(mean_ms)

    return db_times
