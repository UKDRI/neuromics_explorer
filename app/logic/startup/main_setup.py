# main_setup.py (FastAPI entrypoint)

"""
Module defines the FASTAPI app and startup sequence.
It orchestrates initialisation of the registry, building semantic views,
and computing/ aggregating dataset-level stats for quick retrieval.
"""

from contextlib import asynccontextmanager
from datetime import datetime, timezone
from fastapi import FastAPI
from pathlib import Path
import asyncio
import os
import sys
import threading
import uvicorn
from uuid import uuid4

# __file__ uses absolute path to main_setup.py (sets working directory separate from main.R & run_startup.R)
_SCRIPT_DIR   = Path(__file__).resolve().parent   # ./app/logic/startup/
_APP_DIR      = _SCRIPT_DIR.parent.parent         # ./app/
_PROJECT_DIR  = _APP_DIR.parent                   # ./
os.chdir(_PROJECT_DIR)
if str(_PROJECT_DIR) not in sys.path:
    sys.path.insert(0, str(_PROJECT_DIR))   # include project root in sys.path before uvicorn starts
_DATA_DIR     = _PROJECT_DIR / "data"             # ./data/

from app.logic.startup.registry_parser import parse_and_load_registry, build_registry_index
from app.logic.startup.data_summaries import build_dataset_stats
from app.logic.startup.db_pool import DuckDBPool
from app.logic.startup.usage_metrics import (
    get_client_ip,
    initialise_usage_metrics,
    log_request_metrics,
)
from app.logic.api.endpoints import router as api_router
from fastapi.staticfiles import StaticFiles

REGISTRY_YAML = str(_DATA_DIR / "dataset_registry.yml")
DB_PATH       = str(_DATA_DIR / "neuromics_registry.duckdb")
METRICS_DB_PATH = str(_DATA_DIR / "usage_metrics.sqlite3")
DIAZ_DB       = str(_DATA_DIR / "diaz_castro.duckdb")
HONG_DB       = str(_DATA_DIR / "hong.duckdb")
DATA_DIR      = str(_DATA_DIR)
POOL_SIZE     = int(os.getenv("NEX_DUCKDB_POOL_SIZE", "8"))
DUCKDB_THREADS = int(os.getenv("NEX_DUCKDB_THREADS", str(max(2, (os.cpu_count() or 4) // 2))))



@asynccontextmanager
async def lifespan(app: FastAPI):
    # -- STARTUP -- 
    duckdb_pool = None # initialise
    try:
        print("1. Parsing registry YAML and importing into duckdb...")
        parse_and_load_registry(REGISTRY_YAML, DB_PATH)

        print("2. Build registry index and semantic views...")
        build_registry_index(DB_PATH)

        print("3. Computing dataset stats...")
        build_dataset_stats(DB_PATH)

        print("4. Initialising usage metrics tables...")
        initialise_usage_metrics(METRICS_DB_PATH)
        app.state.metrics_db_path = METRICS_DB_PATH
        app.state.metrics_write_lock = threading.Lock()
        app.state.metrics_recent_requests = {}

        print("5. Initialising connection pool to create new instance...")
        duckdb_pool = DuckDBPool(
            db_path = DB_PATH, 
            pool_size = POOL_SIZE,
            attached_dbs = {
                'src_diaz': DIAZ_DB,  # key must match view alias name in registry_parser.py
                'src_hong': HONG_DB,
            },
            threads_per_connection = DUCKDB_THREADS
        )

        # Make a db connection from the pool available to endpoint modules via app state
        app.state.db_pool = duckdb_pool
        api_paths = sorted(route.path for route in app.routes if route.path.startswith("/api"))
        print("Registered API routes:", ", ".join(api_paths))

        print("API ready.")
        yield   # application runs here; all requests are handled after this point

    except Exception as e:
        print(f"Startup failed: {e}")
        raise   # re-raise so uvicorn reports the failure clearly

    # --- CLEANUP & SHUTDOWN ---
    finally:
        if duckdb_pool is not None:
            # get_conn(duckdb_pool) #.close_all()
            duckdb_pool.close_all()
            print("Connection pool closed.")


# Lifespan executes code once during startup, before application starts receiving requests
app = FastAPI(
    lifespan=lifespan,
    debug=True,
    title="Neuromics Explorer",
    version="0.0.0",
    description="Neuromics Explorer visualisation dashboard for UK DRI datasets")
app.mount("/static", StaticFiles(directory="app/static"), name="static")    # attach static assets (ie JS helper, CSS)
app.include_router(api_router)


@app.middleware("http")
async def track_usage_metrics(request, call_next):
    session_id = request.headers.get("x-nex-session-id") or request.cookies.get("nex_session_id") or uuid4().hex
    request.state.session_id = session_id
    started_at = datetime.now(timezone.utc)
    response = await call_next(request)
    if "nex_session_id" not in request.cookies:
        response.set_cookie("nex_session_id", session_id, httponly=True, samesite="lax")

    # Get db_path and write lock if they exist in app state (i.e. if startup successful), otherwise skip tracking to avoid errors during startup
    db_path = getattr(request.app.state, "metrics_db_path", None)
    write_lock = getattr(request.app.state, "metrics_write_lock", None)
    recent_requests = getattr(request.app.state, "metrics_recent_requests", None)
    if db_path is None or write_lock is None or recent_requests is None:
        return response

    duration_ms = (datetime.now(timezone.utc) - started_at).total_seconds() * 1000.0
    try:
        await asyncio.to_thread(
            log_request_metrics,
            db_path,
            write_lock,
            recent_requests,
            session_id=session_id,
            user_ip=get_client_ip(request),
            method=request.method,
            path=request.url.path,
            status_code=response.status_code,
            duration_ms=duration_ms,
            user_agent=request.headers.get("user-agent"),
            referer=request.headers.get("referer"),
        )
    except Exception as exc:
        print(f"Usage metrics logging skipped: {exc}")
    return response

@app.get("/")
def root():
    return {
        "status": "ready",
        "service": "Neuromics Explorer API",
        "docs": "/docs" #"http://0.0.0.0:7000/docs"
    }

# Health check to ensure API is running
@app.get("/health")
def health_check():
    return {"status": "healthy"}

# Run server and main_setup to load/use app instance
if __name__ == "__main__":
    # Run the FastAPI app
    uvicorn.run(
        "app.logic.startup.main_setup:app", #"main_setup:app",
        host="0.0.0.0",
        port=7000,
        log_level="info",
        reload=False)       # uvicorn app.logic.startup.main_setup:app --host 0.0.0.0 --port 8000 --reload # reload automatically restarts server when code changes are detected
    
