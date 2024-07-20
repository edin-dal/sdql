import os
import re
import subprocess
from pathlib import Path
from typing import Any, Final

import matplotlib.pyplot as plt
import pandas as pd

FILE_DIR: Final[str] = os.path.dirname(os.path.realpath(__file__))
SCRIPTS_DIR: Final[str] = os.path.join(FILE_DIR, "scripts")
DTYPES: Final[dict[str, Any]] = {"Query": "string", "Runtime (ms)": int}


def read_job_results() -> pd.DataFrame:
    JOBS_DATA_DIR: Final[str] = os.path.join(FILE_DIR, "job_results")
    JOB_RESULTS: Final[str] = os.path.join(FILE_DIR, f"job_results.csv")
    from benches import RE_RUNTIME as RE_JOB

    if not Path(JOBS_DATA_DIR).is_dir():
        subprocess.call("./codegen_job.sh", shell=True, cwd=SCRIPTS_DIR)
        subprocess.call("./compile_job.sh", shell=True, cwd=SCRIPTS_DIR)
        # note: only runs a single iteration
        subprocess.call("./run_job.sh", shell=True, cwd=SCRIPTS_DIR)

    if not Path(JOB_RESULTS).is_file():
        write_results_frame(JOBS_DATA_DIR, RE_JOB, JOB_RESULTS)

    return pd.read_csv(JOB_RESULTS, dtype=DTYPES)


# 5 iterations ran locally for https://github.com/edin-dal/sdql/tree/wcoj
def read_wcoj_results() -> pd.DataFrame:
    WCOJ_DATA_DIR: Final[str] = os.path.join(FILE_DIR, "wcoj_results")
    WCOJ_RESULTS: Final[str] = os.path.join(FILE_DIR, f"wcoj_results.csv")
    RE_WCOJ: Final[re.Pattern] = re.compile(r"(\d+\.?\d*]?) ms")

    if not Path(WCOJ_RESULTS).is_file():
        write_results_frame(WCOJ_DATA_DIR, RE_WCOJ, WCOJ_RESULTS)

    return pd.read_csv(WCOJ_RESULTS, dtype=DTYPES)


def write_results_frame(data_dir: str, regex: re.Pattern, output_csv: str) -> None:
    df = pd.DataFrame(
        get_query_names_and_times(data_dir, regex),
        columns=["Query", "Runtime (ms)"],
    )
    df.to_csv(output_csv, index=False)


def get_query_names_and_times(
    data_dir: str, regex: re.Pattern
) -> list[tuple[str, int]]:
    files = get_files(data_dir)
    query_names = get_query_names(files)
    times = [get_ms(Path(os.path.join(data_dir, f)).read_text(), regex) for f in files]
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


if __name__ == "__main__":
    job_df = read_job_results()
    wcoj_df = read_wcoj_results()
    df = pd.merge(
        wcoj_df, job_df, how="outer", on="Query", suffixes=(" expected", " actual")
    ).set_index("Query")
    df["Diff (ms)"] = df[df.columns[1]] - df[df.columns[0]]
    df["Diff (%)"] = (100 * df["Diff (ms)"] / df[df.columns[0]]).round().astype(int)
    print(df.sort_values(by=df.columns[3], ascending=False))
    print()
    print(f"{df.columns[3]} summary")
    pct_diff_summary = df[df.columns[3]].describe().round().astype(int)
    print(pct_diff_summary)
    plot(df)
