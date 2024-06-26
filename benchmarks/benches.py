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


def benchmark_sdql(indices: list[int], runs: int, batch: bool) -> list[int]:
    if batch:
        return benchmark_sdql_batch(indices, runs)
    else:
        return benchmark_sdql_individual(indices, runs)


# run separate SBT command for each query, slow but queries are isolated from each other
def benchmark_sdql_individual(indices: list[int], runs: int) -> list[int]:
    times = []
    for i in indices:
        args = f"run benchmark progs/tpch q{i}.sdql"

        q_times = []
        for _ in repeat(None, runs):
            print(f"SBT launching")
            res = subprocess.run(["sbt", args], cwd=REPO_ROOT, stdout=subprocess.PIPE)
            print(f"SBT finished")

            for line in res.stdout.decode().splitlines():
                if m := RE_RUNTIME.match(line):
                    time_ms = int(m.group(1))
                    print(f"SDQL q{i} runtime: {time_ms} ms")
                    q_times.append(time_ms)
                    break

            agg_ms = aggregate_times(q_times, i, "SDQL")
            times.append(agg_ms)

    return times


# runs SBT command for a batch of queries
# this is faster because it loads the datasets once for all queries in a batch
# however, the stdev shows queries might occasionally run much more slowly when in batch
# these outliers skew the mean - so use the minium to aggregate batches (or don't batch)
def benchmark_sdql_batch(indices: list[int], runs: int) -> list[int]:
    individual_runs = []
    for i in range(runs):
        print(f"SDQL running (run {i + 1}/{runs})")
        times = run_sdql_batch(indices)
        individual_runs.append(times)
        print(f"SDQL finished (run {i + 1}/{runs})")

    # transpose - inner lists are now question runtimes
    individual_runs = list(map(list, zip(*individual_runs)))

    times = []
    for tpch_i, q_times in zip(indices, individual_runs):
        agg_ms = aggregate_times(q_times, tpch_i, "SDQL")
        times.append(agg_ms)

    return times


def run_sdql_batch(indices: list[int]) -> list[float]:
    files = " ".join(f"q{i}.sdql" for i in indices)
    args = f"run benchmark progs/tpch {files}"
    print(f"SBT launching")
    res = subprocess.run(["sbt", args], cwd=REPO_ROOT, stdout=subprocess.PIPE)
    print(f"SBT finished")
    return extract_sbt_output(res.stdout.decode(), indices)


def extract_sbt_output(sbt_ouput: str, indices: list[int]) -> list[float]:
    times = []
    for line in sbt_ouput.splitlines():
        if m := RE_RUNTIME.match(line):
            time_ms = int(m.group(1))
            i = indices[len(times)]
            print(f"SDQL q{i} runtime: {time_ms} ms")
            times.append(time_ms)

    return times


def benchmark_duckdb(
    indices: Iterable[int], queries: Iterable[str], threads: int, runs: int
) -> list[int]:
    return benchmark("DuckDB", DuckDb(threads=threads), indices, queries, runs)


def benchmark_hyper(
    indices: Iterable[int], queries: Iterable[str], threads: int, runs: int
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
            agg_ms = aggregate_times(q_times, i, name)
            db_times.append(agg_ms)

    return db_times


def aggregate_times(times: list[float], i: int, name: str) -> int:
    mean_ms = round(mean(times))
    std_ms = round(pstdev(times))
    print(f"{name} q{i}: mean {mean_ms} ms (std {std_ms} ms - {len(times)} runs)")
    return mean_ms
