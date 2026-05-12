# ─────────────────────────────────────────────
# Build stage for R dependencies
# ─────────────────────────────────────────────
FROM rocker/r-ver:4.5.3 AS r-builder

LABEL maintainer="UK DRI Core Informatics"
LABEL description="NeurOmicsExplorer — Multi-omic dashboard" version="0.0.1"

RUN apt-get update && apt-get install -y --no-install-recommends \
    build-essential \
    curl ca-certificates \
    libcurl4-openssl-dev libssl-dev libxml2-dev \
    libfontconfig1-dev libfreetype6-dev libpng-dev libjpeg-dev libtiff5-dev \
    libharfbuzz-dev libfribidi-dev \
    && apt-get clean \
    && rm -rf /var/lib/apt/lists/*

WORKDIR /app
COPY renv.lock renv.lock
COPY renv/ renv/
COPY .Rprofile .Rprofile

ENV RENV_PATHS_LIBRARY=/app/renv/library \
    R_LIBS_USER=/app/renv/library \
    RENV_CONFIG_CACHE_SYMLINKS=FALSE

RUN R -e "install.packages('renv', repos='https://cloud.r-project.org')" && \
    R -e "install.packages('S7', repos='https://cloud.r-project.org')" && \
    R -e "install.packages('httr2', repos='https://cloud.r-project.org')" && \
    R -e "renv::restore(prompt = FALSE, exclude='S7')"

COPY app/ app/
COPY app.R dependencies.R .Rprofile rhino.yml config.yml _quarto.yml ./


# ─────────────────────────────────────────────
# Shiny R frontend - runtime-only libs
# ─────────────────────────────────────────────
FROM rocker/r-ver:4.5.3 AS shiny-frontend

RUN apt-get update && apt-get install -y --no-install-recommends \
    curl ca-certificates \
    libcurl4 libssl3 libxml2 \
    libfontconfig1 libfreetype6 libpng16-16 libjpeg-dev libtiff6 \
    libharfbuzz0b libfribidi0 \
    && apt-get clean \
    && rm -rf /var/lib/apt/lists/*

# Create a non-root user for runtime security as port is exposed on the network
RUN useradd --system --create-home --uid 1001 shinyuser

WORKDIR /app
COPY --from=r-builder /usr/local/lib/R/site-library /usr/local/lib/R/site-library
COPY --from=r-builder /app /app

# Mirror renv env vars so R finds the library
ENV RENV_PATHS_LIBRARY=/app/renv/library \
    R_LIBS_USER=/app/renv/library \
    NEX_API_BASE_URL=http://nex-backend:7000/api


RUN chown -R shinyuser:shinyuser /app
USER shinyuser

EXPOSE 4848
# HEALTHCHECK --interval=30s --timeout=5s --start-period=15s --retries=3 \
#   CMD curl -f http://localhost:4848/ || exit 1
CMD ["R", "-e", "shiny::runApp('/app/main.R', host = '0.0.0.0', port = 4848)"]


# ─────────────────────────────────────────────
# FastAPI / Python backend
# ─────────────────────────────────────────────
FROM python:3.13-slim AS fastapi-backend

RUN apt-get update && apt-get install -y --no-install-recommends \
    ca-certificates curl \
    && apt-get clean \
    && rm -rf /var/lib/apt/lists/*

RUN useradd --system --create-home --uid 1001 apiuser

WORKDIR /app
COPY requirements.txt .
RUN pip install --no-cache-dir --upgrade pip && \
    pip install --no-cache-dir -r requirements.txt

COPY app/ app/
COPY data/dataset_registry.yml data/dataset_registry.yml

RUN chown -R apiuser:apiuser /app

USER apiuser

ENV NEX_DUCKDB_POOL_SIZE=8 \
    NEX_DUCKDB_THREADS=4

EXPOSE 7000
# HEALTHCHECK --interval=30s --timeout=5s --start-period=20s --retries=3 \
#   CMD curl -f http://localhost:7000/health || exit 1

CMD ["uvicorn", "app.logic.startup.main_setup:app", \
    "--host", "0.0.0.0", "--port", "7000", "--workers", "1"]
