"""
Helpers for lightweight API usage tracking stored in a dedicated SQLite file.
Tables request_log logs per-request events while visitor_stats and visitor_sessions logs per-IP and session usage.
"""

from __future__ import annotations

import fnmatch
import sqlite3
import threading
from datetime import datetime, timezone

TRACKING_EXCLUDED_PATHS = {
    "/api/metrics/usage",
    "/api/health",
    "/health",
    "/docs",
    "/openapi.json",
    "/favicon.ico",
    "/api/datasets/all",
    "/api/genes/index",
    "/api/proteins/index",
}
TRACKING_EXCLUDED_PATTERNS = (
    "/api/datasets/*/*/metadata/options",
    "/api/datasets/*/*/expression/volcano",
    "/api/datasets/*/*/expression/histogram",
    "/api/datasets/*/*/expression/groups",
    "/api/datasets/*/*/expression/goi",
    "/api/datasets/*/*/expression/feature-values",
    "/api/datasets/*/*/embeddings",
)
SEARCH_EVENT_PATHS = {"/api/datasets/search"}
# Time window to suppress duplicated or inflated entries for the same user IP, path, method, and status code
DEDUP_WINDOW_SECONDS = 3.0
RECENT_REQUEST_CACHE_LIMIT = 512


def _connect_metrics_db(metrics_db_path: str) -> sqlite3.Connection:
    con = sqlite3.connect(metrics_db_path, timeout=5.0)
    con.row_factory = sqlite3.Row
    con.execute("PRAGMA journal_mode=WAL")
    con.execute("PRAGMA synchronous=NORMAL")
    return con


def initialise_usage_metrics(metrics_db_path: str) -> None:
    """Ensure usage-tracking tables exist in the dedicated metrics database."""
    con = _connect_metrics_db(metrics_db_path)
    try:
        con.execute(
            """
            CREATE TABLE IF NOT EXISTS visitor_stats (
                user_ip TEXT PRIMARY KEY,
                first_visit TEXT,
                last_visit TEXT,
                total_visits INTEGER DEFAULT 0,
                total_requests INTEGER DEFAULT 0,
                last_session_id TEXT,
                last_path TEXT,
                last_method TEXT,
                last_user_agent TEXT
            )
            """
        )
        con.execute(
            """
            CREATE TABLE IF NOT EXISTS visitor_sessions (
                session_id TEXT PRIMARY KEY,
                user_ip TEXT,
                first_visit TEXT,
                last_visit TEXT,
                first_path TEXT,
                last_path TEXT,
                user_agent TEXT
            )
            """
        )
        con.execute(
            """
            CREATE TABLE IF NOT EXISTS request_log (
                visited_at TEXT,
                event_date TEXT,
                session_id TEXT,
                user_ip TEXT,
                method TEXT,
                path TEXT,
                search_event INTEGER DEFAULT 0,
                status_code INTEGER,
                duration_ms REAL,
                user_agent TEXT,
                referer TEXT
            )
            """
        )
        con.execute(
            "CREATE INDEX IF NOT EXISTS idx_request_log_visited_at ON request_log (visited_at)"
        )
        con.execute(
            "CREATE INDEX IF NOT EXISTS idx_request_log_path_method ON request_log (path, method)"
        )
        con.execute(
            "CREATE INDEX IF NOT EXISTS idx_request_log_session_id ON request_log (session_id)"
        )
        con.execute(
            "CREATE INDEX IF NOT EXISTS idx_request_log_search_event ON request_log (search_event)"
        )
        con.commit()
    finally:
        con.close()


def get_client_ip(request) -> str | None:
    """Prefer proxy-forwarded client IPs when present."""
    forwarded_for = request.headers.get("x-forwarded-for")
    if forwarded_for:
        return forwarded_for.split(",")[0].strip()

    if request.client:
        return request.client.host

    return None


def should_track_path(path: str) -> bool:
    """Skip noisy, low-value events or infrastructure-only, non-user-facing routes."""
    if path in TRACKING_EXCLUDED_PATHS:
        return False
    return not any(fnmatch.fnmatch(path, pattern) for pattern in TRACKING_EXCLUDED_PATTERNS)


def _should_skip_duplicate(
    recent_requests: dict[tuple[str, str, str, int], float],
    key: tuple[str, str, str, int],
    now_ts: float,
) -> bool:
    """
    Return True when the same request key is seen very recently
    and needs to be suppressed to avoid inflated logs.
    """
    # If same request occurs within the dedupe window, refresh timestamp and 
    # skip logging, otherwise record it as a recent request
    last_seen = recent_requests.get(key)
    if last_seen is not None and now_ts - last_seen < DEDUP_WINDOW_SECONDS:
        recent_requests[key] = now_ts
        return True

    recent_requests[key] = now_ts

    # If cache is getting large, prune old entries that are now outside dedupe window
    if len(recent_requests) > RECENT_REQUEST_CACHE_LIMIT:
        stale_keys = [
            stale_key
            for stale_key, stale_ts in recent_requests.items()
            if now_ts - stale_ts >= DEDUP_WINDOW_SECONDS
        ]
        for stale_key in stale_keys:
            recent_requests.pop(stale_key, None)
    return False


def log_request_metrics(
    metrics_db_path: str,
    write_lock: threading.Lock,
    recent_requests: dict[tuple[str, str, str, int], float],
    *,
    session_id: str | None,
    user_ip: str | None,
    method: str,
    path: str,
    status_code: int,
    duration_ms: float,
    user_agent: str | None,
    referer: str | None,
) -> None:
    """Persist one request and update the per-visitor logs."""
    if not user_ip or not session_id or not should_track_path(path):
        return

    now = datetime.now(timezone.utc)
    now_iso = now.replace(tzinfo=None).isoformat(timespec="seconds")
    event_date = now.date().isoformat()
    dedupe_key = (session_id, method, path, status_code)
    now_ts = now.timestamp()
    search_event = 1 if path in SEARCH_EVENT_PATHS else 0

    with write_lock:
        if _should_skip_duplicate(recent_requests, dedupe_key, now_ts):
            return

        con = _connect_metrics_db(metrics_db_path)
        try:
            existing_session = con.execute(
                "SELECT 1 FROM visitor_sessions WHERE session_id = ? LIMIT 1",
                [session_id],
            ).fetchone()
            is_new_session = existing_session is None

            if is_new_session:
                con.execute(
                    """
                    INSERT INTO visitor_sessions (
                        session_id, user_ip, first_visit, last_visit,
                        first_path, last_path, user_agent
                    )
                    VALUES (?, ?, ?, ?, ?, ?, ?)
                    """,
                    [session_id, user_ip, now_iso, now_iso, path, path, user_agent],
                )
            else:
                con.execute(
                    """
                    UPDATE visitor_sessions
                    SET last_visit = ?, last_path = ?, user_agent = ?
                    WHERE session_id = ?
                    """,
                    [now_iso, path, user_agent, session_id],
                )

            con.execute(
                """
                INSERT INTO request_log (
                    visited_at, event_date, session_id, user_ip, method, path,
                    search_event, status_code, duration_ms, user_agent, referer
                )
                VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)
                """,
                [
                    now_iso,
                    event_date,
                    session_id,
                    user_ip,
                    method,     
                    path,
                    search_event, 
                    status_code,
                    duration_ms,
                    user_agent,
                    referer,
                ],
            )
            con.execute(
                """
                INSERT INTO visitor_stats (
                    user_ip, first_visit, last_visit, total_visits, total_requests,
                    last_session_id, last_path, last_method, last_user_agent
                )
                VALUES (?, ?, ?, ?, 1, ?, ?, ?, ?)
                ON CONFLICT(user_ip) DO UPDATE SET
                    last_visit = excluded.last_visit,
                    total_visits = visitor_stats.total_visits + excluded.total_visits,
                    total_requests = visitor_stats.total_requests + 1,
                    last_session_id = excluded.last_session_id,
                    last_path = excluded.last_path,
                    last_method = excluded.last_method,
                    last_user_agent = excluded.last_user_agent
                """,
                [
                    user_ip,
                    now_iso,
                    now_iso,
                    1 if is_new_session else 0,
                    session_id,
                    path,
                    method,
                    user_agent,
                ],
            )
            con.commit()
        finally:
            con.close()
