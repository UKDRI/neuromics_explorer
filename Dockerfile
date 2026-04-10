# ── Install dependencies (for system, Shiny, DuckDB, Arrow, R etc) ────────────────────
FROM rocker/shiny:4.5 AS builder

LABEL maintainer="UK DRI Core Informatics" 
LABEL description="NeurOmicsExplorer — Multi-omic dashboard" version="0.0.0"

RUN apt-get update && apt-get install -y \
    python3 python3-pip python3-venv build-essential wget ca-certificates \
    libsqlite3-dev libssl-dev libxml2-dev libcurl4-openssl-dev \
    libharfbuzz-dev libfribidi-dev \
    libfreetype6-dev libpng-dev libjpeg-dev libtiff5-dev zlib1g-dev && \
    apt-get clean && rm -rf /var/lib/apt/lists/*
#     python3.11 python3.11-venv python3-pip \

RUN R -e "update.packages(ask = FALSE, repos = 'https://cloud.r-project.org')"

RUN R -e "install.packages('devtools', repos = 'https://cloud.r-project.org')"

COPY renv.lock renv.lock
RUN R -e "install.packages('renv', repos='https://cloud.r-project.org')" && \
    R -e "renv::restore(prompt=FALSE)"
# # Install required CRAN and Bioconductor packages
# RUN R -e "install.packages(c('shiny', 'plotly', 'purrr', 'tidyverse', 'dplyr', 'shinypanel', 'shinycssloaders', 'ggplot2', 'DT', 'readr', 'bsicons', 'bslib', 'shinyWidgets', 'shinydashboard', 'stringr', 'data.table', 'tidyr', 'RSQLite', 'DBI', 'glue', 'fastDummies', 'htmltools', '', 'jsonlite'), repos = 'https://cloud.r-project.org')"
# RUN R -e "if (!requireNamespace('BiocManager', quietly = TRUE)) install.packages('BiocManager'); BiocManager::install(c('Biostrings', 'bluster', 'dittoSeq', 'SingleCellExperiment', 'scuttle', 'scater', 'scran', 'ComplexHeatmap', 'InteractiveComplexHeatmap'))"
# RUN R -e "install.packages('remotes')"
# RUN R -e "require(devtools)"
# RUN R -e "remotes::install_version(\"httr2\", version = \"1.1.2\", repos = \"http://cran.us.r-project.org/\")"

# Set working directory
WORKDIR /app
# # Copy all project files
# COPY . .


# Copy application code, any databases and additional files
COPY . .
COPY data/ data/
# COPY app/   app/
# NB: .duckdb and Parquet source files can later be mounted at runtime, 
# instead of being baked into image to allow flexibility and dataset updates

# Make entrypoint executable
RUN chmod +x app/logic/startup/start.sh

# Expose port
EXPOSE 4848

# Run Shiny app (activate venv to use Python packages)
CMD ["bash", "-c", "source /app/.venv/bin/activate && bash /app/logic/startup/start.sh"]
# CMD ["bash", "/app/logic/startup/start.sh"]
