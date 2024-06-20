from contextlib import contextmanager
from itertools import repeat
from statistics import mean, pstdev
from time import process_time

from connectors import DuckDbTpch

SEC_TO_MS = 1_000

N_ITERS = 5


def round_ms(secs):
    return round(secs * SEC_TO_MS)


@contextmanager
def Time():
    t1 = t2 = process_time()
    yield lambda: t2 - t1
    t2 = process_time()


def benchmark_duckdb(indices, queries):
    db_times = []
    with DuckDbTpch() as db:
        for i, q in zip(indices, queries):
            q_times = []
            for _ in repeat(None, N_ITERS):
                with Time() as time:
                    db.execute(q)
                q_times.append(time())
            mean_ms = round_ms(mean(q_times))
            std_ms = round_ms(pstdev(q_times))
            print(f"DuckDb q{i}: mean {mean_ms} ms (std {std_ms} ms - {N_ITERS} runs)")
            db_times.append(mean_ms)

    return db_times
