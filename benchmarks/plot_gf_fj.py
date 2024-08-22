import json
import os
import re
import subprocess
from enum import Enum
from pathlib import Path
from statistics import mean
from typing import Any, Final

import matplotlib.pyplot as plt
import pandas as pd

from benches import RE_RUNTIME

QUERY_COL = "Query"
RUNTIME_COL = "Runtime (ms)"

SECS_TO_MS: Final[int] = 1_000
DROP_COLS: set[str] = {"vectorize", "optimize", "strategy"}

FILE_DIR: Final[str] = os.path.dirname(os.path.realpath(__file__))
SCRIPTS_DIR: Final[str] = os.path.join(FILE_DIR, "scripts")
DTYPES: Final[dict[str, Any]] = {QUERY_COL: "string", RUNTIME_COL: int}


class Algo(Enum):
    FJ = "fj"
    GJ = "gj"


class Compare(Enum):
    FREE_JOIN = "free-join"
    WCOJ = "WCOJ"


def read_job_results(algo: Algo) -> pd.DataFrame:
    job_data_dir: Final[str] = os.path.join(FILE_DIR, f"{algo.value}_results")
    job_results: Final[str] = os.path.join(FILE_DIR, f"{algo.value}_results.csv")

    if not Path(job_data_dir).is_dir():
        subprocess.call(f"./codegen_job.sh {algo.value} 5", shell=True, cwd=SCRIPTS_DIR)
        subprocess.call(f"./compile_job.sh {algo.value}", shell=True, cwd=SCRIPTS_DIR)
        subprocess.call(f"./run_job.sh {algo.value}", shell=True, cwd=SCRIPTS_DIR)

    if not Path(job_results).is_file():
        write_results_frame(job_data_dir, job_results)

    return pd.read_csv(job_results, dtype=DTYPES)


# FJ: 5 iterations were ran for https://github.com/edin-dal/wcoj
# GJ: 5 iterations were ran for https://github.com/edin-dal/sdql/tree/wcoj
def read_wcoj_results(algo: Algo) -> pd.DataFrame:
    wcoj_data_dir: Final[str] = os.path.join(FILE_DIR, f"wcoj_{algo.value}_results")
    wcoj_results: Final[str] = os.path.join(FILE_DIR, f"wcoj_{algo.value}_results.csv")

    if not Path(wcoj_results).is_file():
        write_results_frame(wcoj_data_dir, wcoj_results)

    return pd.read_csv(wcoj_results, dtype=DTYPES)


def write_results_frame(data_dir: str, output_csv: str) -> None:
    df = pd.DataFrame(
        get_query_names_and_times(data_dir), columns=[QUERY_COL, RUNTIME_COL]
    )
    df.to_csv(output_csv, index=False)


def get_query_names_and_times(data_dir: str) -> list[tuple[str, int]]:
    files = get_files(data_dir)
    query_names = get_query_names(files)
    times = [
        get_ms(Path(os.path.join(data_dir, f)).read_text(), RE_RUNTIME) for f in files
    ]
    return list(zip(query_names, times))


def get_query_names(files: list[str]) -> list[str]:
    return [f.split(".", 1)[0] for f in files]


def get_files(data_dir: str) -> list[str]:
    return sorted(
        (f for f in next(os.walk(data_dir))[2] if f.endswith(".result")),
        key=lambda f: "0" + f if len(f) == len("__.result") else f,
    )


def get_ms(s: str, regex: re.Pattern) -> int:
    return round(float(regex.search(s).group(1)))


# Both generic join and free join were ran for 5 iterations:
# https://github.com/SIGMOD23p561/free-join/blob/c020bbc7964ba17594299a1910ad6b65eebdf0e0/Makefile#L51
# For generic join we ran this code:
# https://github.com/SIGMOD23p561/free-join/blob/c020bbc7964ba17594299a1910ad6b65eebdf0e0/gj/src/main.rs#L106-L114
# As per https://arxiv.org/abs/2301.10841 – 5.1 Setup:
# "We therefore implement a Generic Join baseline ourselves,
#  by modifying Free Join to fully construct all tries, and removing vectorization."
def read_free_join_results(algo: Algo) -> pd.DataFrame:
    with open(os.path.join(FILE_DIR, f"{algo.value.lower()}.json")) as f:
        data = json.load(f)
    avg = lambda v: round(SECS_TO_MS * mean(v)) if isinstance(v, list) else v
    avg_dict = lambda d: {k: avg(v) for k, v in d.items() if k not in DROP_COLS}
    df = pd.DataFrame(avg_dict(d) for d in data["gj"])
    df["time"] = df["time"].astype(int)
    df.rename(columns={"query": QUERY_COL, "time": RUNTIME_COL}, inplace=True)
    return df


def plot(df: pd.DataFrame, algo: Algo, compare: Compare) -> None:
    plt.plot(df[df.columns[0]], df[df.columns[0]], color="gray")
    plt.scatter(df[df.columns[0]], df[df.columns[1]], color="orange", s=10)
    plt.xscale("log")
    plt.yscale("log")
    plt.xlabel(f"{compare.value} - {algo.value} (ms)")
    plt.ylabel(f"SDQL - {algo.value} (ms)")
    plt.show()


# change the parameters here
ALGO: Algo = Algo.FJ
COMPARE: Compare = Compare.FREE_JOIN


if __name__ == "__main__":
    job_df = read_job_results(ALGO)
    read = read_free_join_results if COMPARE == Compare.FREE_JOIN else read_wcoj_results
    compare_df = read(ALGO)
    df = pd.merge(
        compare_df, job_df, how="outer", on="Query", suffixes=(" expected", " actual")
    ).set_index("Query")
    df["Diff (ms)"] = df[df.columns[1]] - df[df.columns[0]]
    df["Diff (%)"] = (100 * df["Diff (ms)"] / df[df.columns[0]]).round().astype(int)
    pct_diff_summary = pd.DataFrame(df[df.columns[3]].describe().round().astype(int)).T
    print(pct_diff_summary)
    print()
    print(df.sort_values(by=df.columns[3], ascending=False))
    plot(df, ALGO, COMPARE)
