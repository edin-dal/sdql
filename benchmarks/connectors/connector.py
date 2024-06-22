import enum
from contextlib import AbstractContextManager
from dataclasses import asdict
from typing import Protocol, Self

import pandas as pd

from .tables import *


class UseConstraintsTypes(enum.Enum):
    Enable = "enable"
    Disable = "disable"


THREADS = 1

SEC_TO_MS = 1_000


class Connector(AbstractContextManager[Self], Protocol):

    def execute(self, query: str) -> pd.DataFrame: ...

    def time(self, query: str) -> float: ...

    def load_table(
        self,
        name: str,
        path: str,
        schema: list[tuple[str, str], ...],
        primary_keys: None | list[str, ...] = None,
        foreign_keys: None | dict[str, tuple[str, str]] = None,
        use_constraints: UseConstraintsTypes = UseConstraintsTypes.Disable,
        header: bool = False,
        delimiter: str = "|",
    ) -> None: ...

    def load_tpch(
        self,
        use_constraints: UseConstraintsTypes,
        header: bool = False,
        delimiter: str = "|",
    ) -> None:
        for table in (
            REGION,
            NATION,
            SUPPLIER,
            CUSTOMER,
            PART,
            PARTSUPP,
            ORDERS,
            LINEITEM,
        ):
            self.load_table(
                **asdict(table),
                use_constraints=use_constraints,
                header=header,
                delimiter=delimiter,
            )
