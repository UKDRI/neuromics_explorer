"""
This module sets up FastAPI and initialises pools of connections for use in API endpoints to handle concurrent reads internally.
It also includes startup and shutdown event handlers to manage the lifecycle of these connections.
"""

from fastapi import FastAPI # HTTPException, BackgroundTasks, Query
import duckdb
from contextlib import contextmanager

app = FastAPI(
    debug=True,
    title="Neuromics Explorer",
    version="1.1.2",
    description="Neuromics Explorer visualisation dashboard for UK DRI datasets")

# Initialise once at FastAPI startup
duckdb_conn = None

@app.on_event("startup")
def startup():
    global duckdb_conn
    duckdb_conn = duckdb.connect("data/neuromics_registry.duckdb", read_only=True)
    duckdb_conn.execute("PRAGMA threads=8;")
    # Attach source databases to every connection in the pool
    attached_dbs = {
        'diaz': 'data/test_diaz_castro.duckdb',
        'hong': 'data/test_hong.duckdb'
    }
    for alias, path in attached_dbs.items():
        duckdb_conn.execute(f"ATTACH '{path}' AS {alias} (READ_ONLY)")

# Context manager ensure connections are opened/closed cleanly and handling errors, especially for multi-threaded async environments like FastAPI.
@contextmanager
def get_duckdb_connection():
    global duckdb_conn
    if duckdb_conn is None:
        raise RuntimeError("DuckDB connection not initialised.")
    try:
        yield duckdb_conn
    except Exception as e:
        print(f"Error using DuckDB connection: {e}")
        raise




# startup/db_pool.py
# import duckdb
# from queue import Queue, Empty
# from contextlib import contextmanager
# import threading

# class DuckDBPool:
#     def __init__(self, db_path: str, pool_size: int = 8,
#                  attached_dbs: dict = None):
#         self._pool = Queue(maxsize=pool_size)
#         self._lock = threading.Lock()

#         for _ in range(pool_size):
#             con = duckdb.connect(db_path, read_only=True)
#             # Attach source databases to every connection in the pool
#             if attached_dbs:
#                 for alias, path in attached_dbs.items():
#                     con.execute(f"ATTACH '{path}' AS {alias} (READ_ONLY)")
#             self._pool.put(con)

#     @contextmanager
#     def acquire(self, timeout: float = 5.0):
#         try:
#             con = self._pool.get(timeout=timeout)
#         except Empty:
#             raise RuntimeError("DuckDB pool exhausted — all connections busy")    #raise HTTPException(503, "DB pool exhausted")
#         try:
#             yield con
#         finally:
#             self._pool.put(con)  # always return, even on exception


# Initialised once at FastAPI startup
duckdb_pool = DuckDBPool("data/neuromics_registry.duckdb", attached_dbs={
    'diaz': 'data/test_diaz_castro.duckdb',
    'hong': 'data/test_hong.duckdb'
})
# _pool: DuckDBPool = None

# def init_pool(registry_db: str, source_dbs: dict, size: int = 8):
#     global _pool
#     _pool = DuckDBPool(registry_db, pool_size=size, attached_dbs=source_dbs)

# def get_pool() -> DuckDBPool:
#     return _pool