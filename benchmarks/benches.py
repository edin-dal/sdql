import os
import re
import subprocess
from itertools import repeat
from statistics import mean, pstdev
from typing import Iterable

from connectors import Connector, DuckDb, Hyper

SEC_TO_MS = 1_000
RE_RUNTIME = re.compile(r"^Runtime \(ms\): ([\d]+)$")
REPO_ROOT = os.path.normpath(
    os.path.join(os.path.dirname(os.path.realpath(__file__)), "../")
)


def benchmark_sdql(indices: Iterable[int], runs: int) -> list[int]:
    individual_runs = []
    for i in range(runs):
        print(f"Running SDQL (run {i + 1})")
        times = run_sdql(indices)
        individual_runs.append(times)
    print("SDQL finished")

    # transpose - inner lists are now question runtimes
    individual_runs = list(map(list, zip(*individual_runs)))

    times = []
    for tpch_i, q_times in zip(indices, individual_runs):
        mean_ms = round(mean(q_times))
        std_ms = round(pstdev(q_times))
        print(f"SDQL q{tpch_i}: mean {mean_ms} ms (std {std_ms} ms - {runs} runs)")
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
    indices: Iterable[int], queries: Iterable[str], threads: int, runs: int
) -> list[float]:
    return benchmark("DuckDB", DuckDb(threads=threads), indices, queries, runs)


def benchmark_hyper(
    indices: Iterable[int], queries: Iterable[str], threads: int, runs: int
) -> list[float]:
    return benchmark("Hyper", Hyper(threads=threads), indices, queries, runs)


def benchmark(
    name: str,
    connector: Connector,
    indices: Iterable[int],
    queries: Iterable[str],
    runs: int,
) -> list[float]:
    db_times = []
    with connector as db:
        for i, q in zip(indices, queries):
            q_times = []
            for _ in repeat(None, runs):
                time_ms = db.time(q) * SEC_TO_MS
                q_times.append(time_ms)
            mean_ms = mean(q_times)
            std_ms = pstdev(q_times)
            mean_ms = round(mean_ms)
            std_ms = round(std_ms)
            print(f"{name} q{i}: mean {mean_ms} ms (std {std_ms} ms - {runs} runs)")
            db_times.append(mean_ms)

    return db_times
