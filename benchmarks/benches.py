import os
import re
import subprocess
from itertools import repeat
from statistics import mean, pstdev
from typing import Iterable

from connectors import Connector, DuckDb, Hyper

RUNS = 5

SEC_TO_MS = 1_000
RE_RUNTIME = re.compile(r"^Runtime \(ms\): ([\d]+)$")
REPO_ROOT = os.path.normpath(
    os.path.join(os.path.dirname(os.path.realpath(__file__)), "../")
)


def benchmark_sdql(indices: Iterable[int]) -> list[int]:
    runs = []
    for i in range(RUNS):
        print(f"Running SDQL (run {i + 1})")
        times = run_sdql(indices)
        runs.append(times)
    print("SDQL finished")

    # transpose - inner lists are now question runtimes
    runs = list(map(list, zip(*runs)))

    times = []
    for tpch_i, q_times in zip(indices, runs):
        mean_ms = round(mean(q_times))
        std_ms = round(pstdev(q_times))
        print(f"SDQL q{tpch_i}: mean {mean_ms} ms (std {std_ms} ms - {RUNS} runs)")
        times.append(mean_ms)

    return times


def run_sdql(indices: Iterable[int]) -> list[int]:
    files = " ".join(f"q{i}.sdql" for i in indices)
    args = f"run benchmark progs/tpch {files}"
    print(f"Launching SBT")
    res = subprocess.run(["sbt", args], cwd=REPO_ROOT, stdout=subprocess.PIPE)
    print(f"SBT finished")

    times = []
    for line in res.stdout.decode().splitlines():
        if m := RE_RUNTIME.match(line):
            time_ms = int(m.group(1))
            times.append(time_ms)

    return times


def benchmark_duckdb(
    indices: Iterable[int], queries: Iterable[str], threads: int
) -> list[float]:
    return benchmark(
        "DuckDB", DuckDb(threads=threads), indices, queries, round_digits=None
    )


def benchmark_hyper(
    indices: Iterable[int], queries: Iterable[str], threads: int
) -> list[float]:
    return benchmark("Hyper", Hyper(threads=threads), indices, queries, round_digits=3)


def benchmark(
    name: str,
    connector: Connector,
    indices: Iterable[int],
    queries: Iterable[str],
    round_digits: None | int,
) -> list[float]:
    db_times = []
    with connector as db:
        for i, q in zip(indices, queries):
            q_times = []
            for _ in repeat(None, RUNS):
                time_ms = db.time(q) * SEC_TO_MS
                q_times.append(time_ms)
            mean_ms = mean(q_times)
            std_ms = pstdev(q_times)
            if round_digits is None:
                mean_ms = round(mean_ms)
                std_ms = round(std_ms)
            else:
                mean_ms = round(mean_ms, round_digits)
                std_ms = round(std_ms, round_digits)
            print(f"{name} q{i}: mean {mean_ms} ms (std {std_ms} ms - {RUNS} runs)")
            db_times.append(mean_ms)

    return db_times
