import os
from pathlib import Path
from typing import Final

TPCH_QUERIES: Final[str] = os.path.join(os.path.dirname(__file__), "tpch")

q1: Final[str] = Path(os.path.join(TPCH_QUERIES, "q01.sql")).read_text()
q2: Final[str] = Path(os.path.join(TPCH_QUERIES, "q02.sql")).read_text()
q3: Final[str] = Path(os.path.join(TPCH_QUERIES, "q03.sql")).read_text()
q4: Final[str] = Path(os.path.join(TPCH_QUERIES, "q04.sql")).read_text()
q5: Final[str] = Path(os.path.join(TPCH_QUERIES, "q05.sql")).read_text()
q6: Final[str] = Path(os.path.join(TPCH_QUERIES, "q06.sql")).read_text()
q7: Final[str] = Path(os.path.join(TPCH_QUERIES, "q07.sql")).read_text()
q8: Final[str] = Path(os.path.join(TPCH_QUERIES, "q08.sql")).read_text()
q9: Final[str] = Path(os.path.join(TPCH_QUERIES, "q09.sql")).read_text()
q10: Final[str] = Path(os.path.join(TPCH_QUERIES, "q10.sql")).read_text()
q11: Final[str] = Path(os.path.join(TPCH_QUERIES, "q11.sql")).read_text()
q12: Final[str] = Path(os.path.join(TPCH_QUERIES, "q12.sql")).read_text()
q13: Final[str] = Path(os.path.join(TPCH_QUERIES, "q13.sql")).read_text()
q14: Final[str] = Path(os.path.join(TPCH_QUERIES, "q14.sql")).read_text()
q15: Final[str] = Path(os.path.join(TPCH_QUERIES, "q15.sql")).read_text()
q16: Final[str] = Path(os.path.join(TPCH_QUERIES, "q16.sql")).read_text()
q17: Final[str] = Path(os.path.join(TPCH_QUERIES, "q17.sql")).read_text()
q18: Final[str] = Path(os.path.join(TPCH_QUERIES, "q18.sql")).read_text()
q19: Final[str] = Path(os.path.join(TPCH_QUERIES, "q19.sql")).read_text()
q20: Final[str] = Path(os.path.join(TPCH_QUERIES, "q20.sql")).read_text()
q21: Final[str] = Path(os.path.join(TPCH_QUERIES, "q21.sql")).read_text()
q22: Final[str] = Path(os.path.join(TPCH_QUERIES, "q22.sql")).read_text()
