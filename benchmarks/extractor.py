import json
import os
import re
import subprocess
from collections import defaultdict
from statistics import mean, pstdev
from typing import Final

from connectors.hyper import LOG_PATH

REPO_ROOT = os.path.join(os.path.dirname(os.path.realpath(__file__)))

RE_TPCH: Final[re.Pattern] = re.compile(r"TPC-H Query ([1|2]?\d)")


def extract_hyper_log_times(round_digits: int = 3) -> (list[float], list[float]):
    print("Processing Hyper log")
    res = subprocess.run(
        ["grep", '"execution-time"', LOG_PATH],
        cwd=REPO_ROOT,
        stdout=subprocess.PIPE,
    ).stdout.decode()

    i_to_elapsed_times = defaultdict(list)
    i_to_execution_times = defaultdict(list)
    for line in res.splitlines():
        if m := RE_TPCH.search(line):
            i = int(m.group(1))
            as_json = json.loads(line)
            elapsed = as_json["v"]["elapsed"]
            execution_time = as_json["v"]["execution-time"]
            i_to_elapsed_times[i].append(elapsed)
            i_to_execution_times[i].append(execution_time)

    elapsed_times = []
    for i in sorted(i_to_elapsed_times):
        q_times = i_to_elapsed_times[i]
        mean_ms = round(mean(q_times), round_digits)
        std_ms = round(pstdev(q_times), round_digits)
        print(
            f"Elapsed q{i}: mean {mean_ms} ms (std {std_ms} ms - {len(q_times)} runs)"
        )
        elapsed_times.append(mean_ms)

    execution_times = []
    for i in sorted(i_to_execution_times):
        q_times = i_to_execution_times[i]
        mean_ms = round(mean(q_times), round_digits)
        std_ms = round(pstdev(q_times), round_digits)
        print(
            f"Execution q{i}: mean {mean_ms} ms (std {std_ms} ms - {len(q_times)} runs)"
        )
        execution_times.append(mean_ms)

    print("Hyper log processed")
    return elapsed_times, execution_times
