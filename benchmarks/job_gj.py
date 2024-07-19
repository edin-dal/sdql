import os
import re
from pathlib import Path
from typing import Final

import pandas as pd

FILE_DIR: Final[str] = os.path.dirname(__file__)
DATA_DIR: Final[str] = os.path.join(FILE_DIR, "results")
WCOJ_RESULTS: Final[str] = os.path.join(FILE_DIR, f"wcoj_results.csv")

RE_WCOJ: Final[re.Pattern] = re.compile(r"(\d+\.?\d*]?) ms")


def read_wcoj_results() -> pd.DataFrame:
    if not Path(WCOJ_RESULTS).is_file():
        write_wcoj_results()

    return pd.read_csv(WCOJ_RESULTS)


def write_wcoj_results() -> None:
    df = pd.DataFrame(get_query_names_and_times(), columns=["Query", "Runtime (ms)"])
    df.to_csv(WCOJ_RESULTS, index=False)


def get_query_names_and_times() -> list[tuple[str, int]]:
    files = get_files()
    queries = [f.split(".", 1)[0] for f in files]
    times = [get_ms(Path(os.path.join(DATA_DIR, f)).read_text()) for f in files]
    return list(zip(queries, times))


def get_files() -> list[str]:
    return sorted(
        (f for f in next(os.walk(DATA_DIR))[2] if f.endswith(".result")),
        key=lambda f: "0" + f if len(f) == len("__.result") else f,
    )


def get_ms(s: str) -> int:
    return round(float(RE_WCOJ.search(s).group(1)))


if __name__ == "__main__":
    df = read_wcoj_results()
    print(df)
