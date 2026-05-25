"""
Helpers for lightweight API usage tracking stored in a dedicated DuckDB file.
Tables request_log logs per-request events and visitor_stats logs per-IP usage.
"""

from __future__ import annotations

import threading
from datetime import datetime, timezone

import duckdb

TRACKING_EXCLUDED_PATHS = {
    "/api/metrics/usage",
    "/api/health",
    "/health",
    "/docs",
    "/openapi.json",
    "/favicon.ico",
    "/api/datasets/all",
}


def initialise_usage_metrics(metrics_db_path: str) -> None:
    """Ensure usage-tracking tables exist in the dedicated metrics database."""
    con = duckdb.connect(metrics_db_path, read_only=False)
    try:
        con.execute(
            """
            CREATE TABLE IF NOT EXISTS visitor_stats (
                user_ip VARCHAR PRIMARY KEY,
                first_visit TIMESTAMP,
                last_visit TIMESTAMP,
                total_visits BIGINT,
                last_path VARCHAR,
                last_method VARCHAR,
                last_user_agent VARCHAR
            )
            """
        )
        con.execute(
            """
            CREATE TABLE IF NOT EXISTS request_log (
                visited_at TIMESTAMP,
                event_date DATE,
                user_ip VARCHAR,
                method VARCHAR,
                path VARCHAR,
                status_code INTEGER,
                duration_ms DOUBLE,
                user_agent VARCHAR,
                referer VARCHAR
            )
            """
        )
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
    return path not in TRACKING_EXCLUDED_PATHS


def log_request_metrics(
    metrics_db_path: str,
    write_lock: threading.Lock,
    *,
    user_ip: str | None,
    method: str,
    path: str,
    status_code: int,
    duration_ms: float,
    user_agent: str | None,
    referer: str | None,
) -> None:
    """Persist one request and update the per-visitor logs."""
    if not user_ip or not should_track_path(path):
        return

    now = datetime.now(timezone.utc).replace(tzinfo=None)
    event_date = now.date()

    with write_lock:
        con = duckdb.connect(metrics_db_path, read_only=False)
        try:
            con.execute(
                """
                INSERT INTO request_log (
                    visited_at, event_date, user_ip, method, path,
                    status_code, duration_ms, user_agent, referer
                )
                VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?)
                """,
                [
                    now,
                    event_date,
                    user_ip,
                    method,
                    path,
                    status_code,
                    duration_ms,
                    user_agent,
                    referer,
                ],
            )
            con.execute(
                """
                INSERT INTO visitor_stats (
                    user_ip, first_visit, last_visit, total_visits,
                    last_path, last_method, last_user_agent
                )
                VALUES (?, ?, ?, 1, ?, ?, ?)
                ON CONFLICT (user_ip) DO UPDATE SET
                    last_visit = EXCLUDED.last_visit,
                    total_visits = visitor_stats.total_visits + 1,
                    last_path = EXCLUDED.last_path,
                    last_method = EXCLUDED.last_method,
                    last_user_agent = EXCLUDED.last_user_agent
                """,
                [user_ip, now, now, path, method, user_agent],
            )
        finally:
            con.close()
