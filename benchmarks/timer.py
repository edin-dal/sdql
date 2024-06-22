from contextlib import contextmanager
from time import process_time


@contextmanager
def timer():
    t1 = t2 = process_time()
    yield lambda: t2 - t1
    t2 = process_time()
