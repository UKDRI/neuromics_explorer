#!/bin/bash
set -e

# Ensure your Python modules are discoverable
export PYTHONPATH="${PYTHONPATH}:$(pwd)"

# Start FastAPI backend in background
echo "Starting FastAPI backend..."
python3 /app/logic/startup/main_setup.py &
API_PID=$!

# Wait for FastAPI to be ready (start polling health checks to prevent race condition)
echo "Waiting for FastAPI to be ready..."
MAX_WAIT=30
ELAPSED=0
while ! curl -s http://localhost:7000/health >/dev/null 2>&1; do
  if [ $ELAPSED -ge $MAX_WAIT ]; then
    echo "ERROR: FastAPI failed to start within ${MAX_WAIT}s"
    kill $API_PID 2>/dev/null || true
    exit 1
  fi
  sleep 2
  ELAPSED=$((ELAPSED + 2))
done

echo "FastAPI is ready! Starting Shiny..."

# Start Shiny app
Rscript -e "shiny::runApp('app/main.R', port = 3838, host = '0.0.0.0')"
