import enum
import os
import re
import subprocess
from pathlib import Path
from typing import Any, Final

import matplotlib.pyplot as plt
import pandas as pd

from benches import RE_RUNTIME

FILE_DIR: Final[str] = os.path.dirname(os.path.realpath(__file__))
SCRIPTS_DIR: Final[str] = os.path.join(FILE_DIR, "scripts")
DTYPES: Final[dict[str, Any]] = {"Query": "string", "Runtime (ms)": int}


class Mode(enum.Enum):
    FJ = "fj"
    GJ = "gj"


def read_job_results(mode: Mode) -> pd.DataFrame:
    JOBS_DATA_DIR: Final[str] = os.path.join(FILE_DIR, f"{mode.value}_results")
    JOB_RESULTS: Final[str] = os.path.join(FILE_DIR, f"{mode.value}_results.csv")

    if not Path(JOBS_DATA_DIR).is_dir():
        subprocess.call(f"./codegen_job.sh {mode.value} 5", shell=True, cwd=SCRIPTS_DIR)
        subprocess.call(f"./compile_job.sh {mode.value}", shell=True, cwd=SCRIPTS_DIR)
        subprocess.call(f"./run_job.sh {mode.value}", shell=True, cwd=SCRIPTS_DIR)

    if not Path(JOB_RESULTS).is_file():
        write_results_frame(JOBS_DATA_DIR, JOB_RESULTS)

    return pd.read_csv(JOB_RESULTS, dtype=DTYPES)


# FJ: 5 iterations were ran for https://github.com/edin-dal/wcoj
# GJ: 5 iterations were ran for https://github.com/edin-dal/sdql/tree/wcoj
def read_wcoj_results(mode: Mode) -> pd.DataFrame:
    WCOJ_DATA_DIR: Final[str] = os.path.join(FILE_DIR, f"wcoj_{mode.value}_results")
    WCOJ_RESULTS: Final[str] = os.path.join(FILE_DIR, f"wcoj_{mode.value}_results.csv")

    if not Path(WCOJ_RESULTS).is_file():
        write_results_frame(WCOJ_DATA_DIR, WCOJ_RESULTS)

    return pd.read_csv(WCOJ_RESULTS, dtype=DTYPES)


def write_results_frame(data_dir: str, output_csv: str) -> None:
    df = pd.DataFrame(
        get_query_names_and_times(data_dir), columns=["Query", "Runtime (ms)"]
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


def plot(df: pd.DataFrame) -> None:
    plt.plot(df[df.columns[0]], df[df.columns[0]], color="gray")
    plt.scatter(df[df.columns[0]], df[df.columns[1]], color="orange", s=10)
    plt.xscale("log")
    plt.yscale("log")
    plt.xlabel("Expected (ms)")
    plt.ylabel("Actual (ms)")
    plt.show()


# switch mode here
MODE: Mode = Mode.FJ

if __name__ == "__main__":
    job_df = read_job_results(MODE)
    wcoj_df = read_wcoj_results(MODE)
    df = pd.merge(
        wcoj_df, job_df, how="outer", on="Query", suffixes=(" expected", " actual")
    ).set_index("Query")
    df["Diff (ms)"] = df[df.columns[1]] - df[df.columns[0]]
    df["Diff (%)"] = (100 * df["Diff (ms)"] / df[df.columns[0]]).round().astype(int)
    pct_diff_summary = pd.DataFrame(df[df.columns[3]].describe().round().astype(int)).T
    print(pct_diff_summary)
    print()
    print(df.sort_values(by=df.columns[3], ascending=False))
    plot(df)
