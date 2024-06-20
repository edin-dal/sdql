import duckdb

SCALING_FACTOR = 1
CONCURRENCY = 1


class DuckDbTpch:

    def __init__(self):
        self.conn = duckdb.connect(":memory:")
        self.conn.execute(f"PRAGMA threads={CONCURRENCY}")
        print(f"Reading TPCH SF={SCALING_FACTOR}...")
        self.conn.execute(f"CALL dbgen(sf={SCALING_FACTOR})")

    def __enter__(self):
        return self.conn

    def __exit__(self, exc_type, exc_val, exc_tb):
        self.conn.__exit__(exc_type, exc_val, exc_tb)

    def execute(self, *args, **kwargs):
        return self.conn.execute(*args, **kwargs)
