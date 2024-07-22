import os
import re
import subprocess
from itertools import repeat
from statistics import mean, pstdev
from typing import Final, Iterable

from connectors import Connector, DuckDb, Hyper

SEC_TO_MS = 1_000
RE_QUERY = re.compile(r"^q(\d?\d)$")
RE_RUNTIME: Final[re.Pattern] = re.compile(r"(\d+\.?\d*]?) ms")
REPO_ROOT = os.path.normpath(
    os.path.join(os.path.dirname(os.path.realpath(__file__)), "../")
)
SCRIPTS_DIR = os.path.join(os.path.dirname(os.path.realpath(__file__)), "scripts")


def benchmark_sdql(indices: list[int], runs: int) -> list[int]:
    subprocess.call(f"./codegen_tpch.sh {runs}", shell=True, cwd=SCRIPTS_DIR)
    subprocess.call("./compile_tpch.sh", shell=True, cwd=SCRIPTS_DIR)
    print("SDQL running...")
    output = subprocess.check_output(
        "./run_tpch.sh", shell=True, text=True, cwd=SCRIPTS_DIR
    )
    lines = iter(output.splitlines())

    times = []
    for tpch_i in indices:
        i = int(RE_QUERY.match(next(lines)).group(1))
        assert i == tpch_i
        mean_ms = round(float(RE_RUNTIME.match(next(lines)).group(1)))
        times.append(mean_ms)

    return times


def benchmark_duckdb(
    indices: Iterable[int],
    queries: Iterable[str],
    threads: int,
    runs: int,
) -> list[int]:
    return benchmark("DuckDB", DuckDb(threads=threads), indices, queries, runs)


def benchmark_hyper(
    indices: Iterable[int],
    queries: Iterable[str],
    threads: int,
    runs: int,
) -> list[int]:
    return benchmark("Hyper", Hyper(threads=threads), indices, queries, runs)


def benchmark(
    name: str,
    connector: Connector,
    indices: Iterable[int],
    queries: Iterable[str],
    runs: int,
) -> list[int]:
    db_times = []
    with connector as db:
        for i, q in zip(indices, queries):
            q_times = []
            for _ in repeat(None, runs):
                time_ms = db.time(q) * SEC_TO_MS
                q_times.append(time_ms)
            mean_ms = average_times(q_times, i, name)
            db_times.append(mean_ms)

    return db_times


def average_times(times: list[float], i: int, name: str) -> int:
    mean_ms = round(mean(times))
    std_ms = round(pstdev(times))
    print(f"{name} q{i}: mean {mean_ms} ms (std {std_ms} ms - {len(times)} runs)")
    return mean_ms
