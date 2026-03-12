# main_setup.py (FastAPI entrypoint)

"""
Module defines the FASTAPI app and startup sequence.
It orchestrates initialisation of the registry, building semantic views,
and computing/ aggregating dataset-level stats for quick retrieval.
"""

from contextlib import asynccontextmanager
from fastapi import FastAPI
from registry_parser import parse_and_load_registry, build_registry_index
from data_summaries import build_dataset_stats
from db_pool import DuckDBPool, get_conn
import uvicorn


@asynccontextmanager
async def lifespan(app: FastAPI):
    # -- STARTUP -- 
    duckdb_pool = None # initialise
    try:
        print("1. Parsing registry YAML and importing into duckdb...")
        parse_and_load_registry("data/dataset_registry.yml", "data/neuromics_registry.duckdb")

        print("2. Build registry index and semantic views...")
        build_registry_index("data/neuromics_registry.duckdb")

        print("3. Computing dataset stats...")
        build_dataset_stats("data/neuromics_registry.duckdb")

        print("4. Initialising connection pool to create new instance...")
        duckdb_pool = DuckDBPool(
            db_path="data/neuromics_registry.duckdb", 
            attached_dbs={
                'src_diaz': 'data/diaz_castro.duckdb',  # key must match view alias name in registry_parser.py
                'src_hong': 'data/hong.duckdb',
            }
        )

        # Make pool available to endpoint modules via app state
        # app.state.db_pool = duckdb_pool

        print("API ready.")
        yield   # application runs here; all requests are handled after this point

    except Exception as e:
        print(f"Startup failed: {e}")
        raise   # re-raise so uvicorn reports the failure clearly

    # --- CLEANUP & SHUTDOWN ---
    finally:
        if duckdb_pool is not None:
            get_conn(duckdb_pool) #.close_all()
            duckdb_pool.close_all()
            print("Connection pool closed.")


# Lifespan executes code once during startup, before application starts receiving requests
app = FastAPI(
    lifespan=lifespan,
    debug=True,
    title="Neuromics Explorer",
    version="1.2.0",
    description="Neuromics Explorer visualisation dashboard for UK DRI datasets")


# Health check to ensure API is running
@app.get("/health")
def health_check():
    return {"status": "healthy"}

# Run server and main_setup to load/use app instance
if __name__ == "__main__":
    # Run the FastAPI app
    uvicorn.run(
        "app.logic.startup.main_setup:app",
        host="0.0.0.0",
        port=7000,
        log_level="info",
        reload=True)       # uvicorn app.logic.startup.main_setup:app --host 0.0.0.0 --port 8000 --reload # reload automatically restarts server when code changes are detected
    