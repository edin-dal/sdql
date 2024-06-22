from itertools import repeat
from statistics import mean, pstdev
from typing import Iterable

from connectors import DuckDb, Hyper

N_ITERS = 5


def benchmark_duckdb(indices: Iterable[int], queries: Iterable[str]) -> list[float]:
    return benchmark("DuckDB", DuckDb, indices, queries)


def benchmark_hyper(indices: Iterable[int], queries: Iterable[str]) -> list[float]:
    return benchmark("Hyper", Hyper, indices, queries)


def benchmark(
    name: str, connector, indices: Iterable[int], queries: Iterable[str]
) -> list[float]:
    db_times = []
    with connector() as db:
        for i, q in zip(indices, queries):
            q_times = []
            for _ in repeat(None, N_ITERS):
                t = db.time(q)
                q_times.append(t)
            mean_ms = round(mean(q_times))
            std_ms = round(pstdev(q_times))
            print(f"{name} q{i}: mean {mean_ms} ms (std {std_ms} ms - {N_ITERS} runs)")
            db_times.append(mean_ms)

    return db_times
