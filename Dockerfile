# ── Install dependencies (for system, Shiny, DuckDB, Arrow, R etc) ────────────────────
FROM rocker/shiny:4.5

LABEL maintainer="UK DRI Core Informatics"
LABEL description="NeurOmicsExplorer — Multi-omic dashboard" version="0.0.0"

RUN apt-get update && apt-get install -y \
    python3 python3-pip python3-venv build-essential wget ca-certificates \
    libsqlite3-dev libssl-dev libxml2-dev libcurl4-openssl-dev \
    libharfbuzz-dev libfribidi-dev \
    libfreetype6-dev libpng-dev libjpeg-dev libtiff5-dev zlib1g-dev && \
    apt-get clean && rm -rf /var/lib/apt/lists/*
#     python3.11 python3.11-venv python3-pip \

# Set working directory
WORKDIR /app

# Copy application code, envs, any databases and additional files
COPY renv.lock renv.lock
COPY renv/ renv/
COPY .Rprofile .Rprofile

ENV RENV_PATHS_LIBRARY=renv/library
ENV RENV_CONFIG_CACHE_SYMLINKS=FALSE

RUN R -e "install.packages('renv', repos='https://cloud.r-project.org')" && \
    R -e "renv::restore(prompt=FALSE, library='renv/library')"

COPY . .
COPY data/ data/
COPY requirements.txt requirements.txt
# NB: .duckdb and Parquet source files can later be mounted at runtime, 
# instead of being baked into image to allow flexibility and dataset updates

# Install Python deps in venv
RUN python3 -m venv /opt/venv
ENV PATH="/opt/venv/bin:$PATH"

RUN python3 -m pip install --upgrade pip setuptools wheel && \
    python3 -m pip install -r requirements.txt


# ── Make entrypoint for Shiny ────────────────────────────────────────
RUN chmod +x app/logic/startup/start.sh

# Expose port
EXPOSE 4848

# Run Shiny app (activates venv to use Python packages)
CMD ["bash", "app/logic/startup/start.sh"]
