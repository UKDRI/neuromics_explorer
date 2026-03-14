"""
This module sets up FastAPI and initialises pools of connections to handle concurrent reads internally.
It also includes startup and shutdown event handlers to manage the lifecycle of these connections.
"""

import duckdb
import os
from queue import Queue, Empty
from contextlib import contextmanager
import threading

# Connection pool class to manage multiple read-only duckdb connections for concurrent access in FastAPI
class DuckDBPool:
    def __init__(self, db_path: str, pool_size: int = 4,
            attached_dbs: dict | None = None): # | None avoids conflicts
        self.pool = Queue(maxsize=pool_size)
        self._lock = threading.Lock()

        for _ in range(pool_size):
            con = duckdb.connect(db_path, read_only=True)
            # Attach source databases to every connection in the pool
            if attached_dbs:
                for alias, path in attached_dbs.items():
                    if not os.path.exists(path):
                        print(f"   WARNING: source DB not found, skipping: {path}")
                        continue
                    try:
                        con.execute(f"ATTACH '{path}' AS {alias} (READ_ONLY)")
                    except duckdb.BinderException as e:
                        if "already exists" in str(e).lower():
                            pass
                        else:
                            raise
            self.pool.put(con)
    
    # Acquire connection from pool/ queue
    def acquire(self, timeout: float = 5.0):
        try:
            return self.pool.get(timeout=timeout)
        except Empty as e:
            raise RuntimeError(
                "DuckDB pool exhausted — all connections busy. "
                "Consider increasing pool_size. Error:", str(e)
            )

    # Release connection back to pool/ queue
    def release(self, con):
        self.pool.put(con)

    # Close all connections for shutdown cleanup
    def close_all(self):
        while not self.pool.empty():
            con = self.pool.get_nowait()
            con.close()

# Context manager ensure connections are opened/closed automatically, cleanly and handling errors, especially for multi-threaded async environments like FastAPI.
@contextmanager
def get_conn(pool: DuckDBPool):
    con = pool.acquire()
    try:
        yield con
    finally:
        pool.release(con)






# # Initialise once at FastAPI startup
# duckdb_conn = None

# @app.on_event("startup")
# def startup():
#     global duckdb_conn
#     duckdb_conn = duckdb.connect("data/neuromics_registry.duckdb", read_only=True)
#     duckdb_conn.execute("PRAGMA threads=8;")
#     # Attach source databases to every connection in the pool
#     attached_dbs = {
#         'diaz': 'data/test_diaz_castro.duckdb',
#         'hong': 'data/test_hong.duckdb'
#     }
#     for alias, path in attached_dbs.items():
#         duckdb_conn.execute(f"ATTACH '{path}' AS {alias} (READ_ONLY)")

#  TBC
# @contextmanager
# def get_duckdb_connection():
#     global duckdb_conn
#     if duckdb_conn is None:
#         raise RuntimeError("DuckDB connection not initialised.")
#     try:
#         yield duckdb_conn
#     except Exception as e:
#         print(f"Error using DuckDB connection: {e}")
#         raise
