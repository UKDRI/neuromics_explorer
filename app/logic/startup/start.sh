#!/bin/bash
set -e

# Ensure your Python modules are discoverable
export PYTHONPATH="${PYTHONPATH}:$(pwd)"

# Start FastAPI backend in background
echo "Starting FastAPI backend..."
python3 /app/logic/startup/main_setup.py &

# Start Shiny app
echo "Starting Neuromics Explorer..."
Rscript -e "shiny::runApp('app/main.R', port = 3838, host = '0.0.0.0')"
