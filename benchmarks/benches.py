import os
import re
import subprocess
from enum import Enum
from itertools import islice, repeat
from statistics import mean, pstdev
from typing import Callable, Dict, Final, Iterable

from connectors import Connector, DuckDb, Hyper

SEC_TO_MS = 1_000
RE_QUERY = re.compile(r"^q(\d?\d)$")
RE_RUNTIME = re.compile(r"^Runtime \(ms\): ([\d]+)$")
REPO_ROOT = os.path.normpath(
    os.path.join(os.path.dirname(os.path.realpath(__file__)), "../")
)


class Aggregation(Enum):
    min = 1
    mean = 2


def benchmark_sdql(indices: list[int], runs: int, agg: Aggregation) -> list[int]:
    subprocess.call("./codegen.sh", shell=True)
    subprocess.call("./compile.sh", shell=True)
    print("SDQL running...")
    output = subprocess.check_output(f"./run.sh {runs}", shell=True, text=True)
    lines = iter(output.splitlines())

    individual_runs = []
    while (line := next(lines, None)) is not None:
        i = int(RE_QUERY.match(line).group(1))
        times = [int(RE_RUNTIME.match(x).group(1)) for x in islice(lines, 0, runs)]
        if i in indices:
            individual_runs.append(times)

    times = []
    for tpch_i, q_times in zip(indices, individual_runs):
        agg_ms = aggregate_times(q_times, tpch_i, "SDQL", agg)
        times.append(agg_ms)

    return times


def benchmark_duckdb(
    indices: Iterable[int],
    queries: Iterable[str],
    threads: int,
    runs: int,
    agg: Aggregation,
) -> list[int]:
    return benchmark("DuckDB", DuckDb(threads=threads), indices, queries, runs, agg)


def benchmark_hyper(
    indices: Iterable[int],
    queries: Iterable[str],
    threads: int,
    runs: int,
    agg: Aggregation,
) -> list[int]:
    return benchmark("Hyper", Hyper(threads=threads), indices, queries, runs, agg)


def benchmark(
    name: str,
    connector: Connector,
    indices: Iterable[int],
    queries: Iterable[str],
    runs: int,
    agg: Aggregation,
) -> list[int]:
    db_times = []
    with connector as db:
        for i, q in zip(indices, queries):
            q_times = []
            for _ in repeat(None, runs):
                time_ms = db.time(q) * SEC_TO_MS
                q_times.append(time_ms)
            agg_ms = aggregate_times(q_times, i, name, agg)
            db_times.append(agg_ms)

    return db_times


def aggregate_times(times: list[float], i: int, name: str, agg: Aggregation) -> int:
    agg_ms = round(AGG_TO_FUNC[agg](times))
    std_ms = round(pstdev(times))
    print(f"{name} q{i}: {agg.name} {agg_ms} ms (std {std_ms} ms - {len(times)} runs)")
    return agg_ms


AGG_TO_FUNC: Final[Dict[Aggregation, Callable[[Iterable[float]], int]]] = {
    Aggregation.min: min,
    Aggregation.mean: mean,
}
