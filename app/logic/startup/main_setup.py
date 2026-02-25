# main_setup.py (FastAPI entrypoint)

"""
This module defines the FASTAPI app and startup sequence.
It orchestrates the initialisation of the registry, building semantic views and computing/ aggregating dataset-level stats for quick retrieval.
"""

from contextlib import asynccontextmanager
from fastapi import FastAPI
from .registry_parser import parse_and_load_registry, build_registry_index
from .data_summaries import build_dataset_stats
# resolve_column_mappings, _infer_organism

@asynccontextmanager
async def lifespan(app: FastAPI):
    # -- STARTUP --
    print("1. Parsing registry YAML and importing into duckdb...")
    parse_and_load_registry("config/dataset_registry.yml", "data/neuromics_registry.duckdb")

    print("2. Build registry index and semantic views...")
    build_registry_index("data/neuromics_registry.duckdb")

#     print("2. Building semantic views...")
#     build_semantic_views("data/registry.duckdb", {
#         "diaz": "data/diaz.duckdb",
#         "hong": "data/hong.duckdb",
#     })

    print("3. Computing dataset stats...")
    build_dataset_stats("data/neuromics_registry.duckdb")

#     print("5. Initialising connection pool...")
#     init_pool("data/registry.duckdb", {
#         "diaz": "data/diaz.duckdb",
#         "hong": "data/hong.duckdb",
#     }, size=8)

    print("API ready.")
#     yield

#     # --- SHUTDOWN ---
#     # pool cleanup if needed

app = FastAPI(lifespan=lifespan)


# Health check to ensure API is running
@app.get("/health")
def health_check():
    return {"status": "healthy"}

# Run server - go to init_backend.py and load app instance
import uvicorn
if __name__ == "__main__":
    # Run the FastAPI app
    uvicorn.run(
        "app.logic.startup.main_setup:app",
        host="0.0.0.0",
        port=8000,
        log_level="info",
        reload=True)       # uvicorn app.logic.startup.main_setup:app --host 0.0.0.0 --port 8000 --reload # reload automatically restarts server when code changes are detected
    