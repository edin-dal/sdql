import os
import re
import subprocess
from pathlib import Path
from typing import Final

import pandas as pd

FILE_DIR: Final[str] = os.path.dirname(os.path.realpath(__file__))
SCRIPTS_DIR = os.path.join(FILE_DIR, "scripts")


def read_job_results() -> pd.DataFrame:
    JOB_RESULTS: Final[str] = os.path.join(FILE_DIR, f"job_results.csv")

    def write_job_results() -> None:
        subprocess.call("./codegen_job.sh", shell=True, cwd=SCRIPTS_DIR)
        subprocess.call("./compile_job.sh", shell=True, cwd=SCRIPTS_DIR)
        # note: only runs a single iteration
        subprocess.call("./run_job.sh", shell=True, cwd=SCRIPTS_DIR)

    if not Path(JOB_RESULTS).is_file():
        write_job_results()

    return "TODO"


# 5 iterations ran locally for https://github.com/edin-dal/sdql/tree/wcoj
def read_wcoj_results() -> pd.DataFrame:
    DATA_DIR: Final[str] = os.path.join(FILE_DIR, "wcoj_results")
    WCOJ_RESULTS: Final[str] = os.path.join(FILE_DIR, f"wcoj_results.csv")
    RE_WCOJ: Final[re.Pattern] = re.compile(r"(\d+\.?\d*]?) ms")

    def write_wcoj_results() -> None:
        df = pd.DataFrame(
            get_query_names_and_times(), columns=["Query", "Runtime (ms)"]
        )
        df.to_csv(WCOJ_RESULTS, index=False)

    def get_query_names_and_times() -> list[tuple[str, int]]:
        files = get_files()
        query_names = get_query_names(files)
        times = [get_ms(Path(os.path.join(DATA_DIR, f)).read_text()) for f in files]
        return list(zip(query_names, times))

    def get_query_names(files: list[str]) -> list[str]:
        return [f.split(".", 1)[0] for f in files]

    def get_files() -> list[str]:
        return sorted(
            (f for f in next(os.walk(DATA_DIR))[2] if f.endswith(".result")),
            key=lambda f: "0" + f if len(f) == len("__.result") else f,
        )

    def get_ms(s: str) -> int:
        return round(float(RE_WCOJ.search(s).group(1)))

    if not Path(WCOJ_RESULTS).is_file():
        write_wcoj_results()

    return pd.read_csv(WCOJ_RESULTS)


if __name__ == "__main__":
    print(read_job_results())
    print(read_wcoj_results())
