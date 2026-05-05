# NEx_alpha

NEx_alpha is the working codebase for NeurOmicsExplorer, a Shiny-based data explorer for neuromics datasets. The project is split across two runtimes:

-   a Python service that prepares and logs the dataset registry, processes DuckDB and Parquet storage files, runs SQL, and streams Arrow responses
-   an R/Shiny app that handles the UI, interactivity, and visualisation

## What this repository contains

### Rhino App & Directory Structure

```
data/
| ├── dataset_registry.yml
| ├── hong.duckdb
| ├── diaz_castro.duckdb
| ├── neuromics_registry.duckdb   # Data register, canonical name mapping, indexing, and stats (auto-generated)
│ └── lab={lab_name}/modality={omic}/.../conversions/       # Converted RDS / h5ad datasets to Parquet
│              ├── expression.parquet               # (DE) expression data
│              ├── counts.parquet
│              ├── logcounts.parquet
│              ├── feature_annotations.parquet
│              ├── obs_metadata.parquet             # sample/ cell metadata
│              ├── tSNE.parquet
│              ├── UMAP.parquet
│              └── PCA.parquet
app/
├── main.R                          # Entry point — main Shiny application module ties UI + server
├── logic/                          # Code independent from Shiny
│   ├── startup/                    # Runs once at container start
│   │   ├── db_pool.py              # DuckDB read-only connection pool (pool package)
│   │   ├── registry_parser.py      # Parse YAML → maps SQL columns and tables, build/query gene_study_index, import, harmonises, and registers current datasets
│   │   ├── db_views.py             # Shared helpers for DuckDB and semantic view management - creates views (virtual tables) to collate data across datasets for fast, dynamic querying
│   │   ├── data_summaries.py       # Computes and stores aggregates dataset-level stats
│   │   └── main_setup.py           # Defines FASTAPI app and startup sequence, i.e. initialise registry, build semantic views, compute dataset-level stats
|   |
│   ├── api/
│   │   ├── api_client.py       # HTTP client wrappers for requests - calls API helpers to fetch data as Arrow IPC streams
│   │   └── endpoints.py        # FastAPI routers for querying storage files (DuckDB, Parquet, etc) via DuckDB 
|   |
│   ├── conversions/                        # run R scripts locally
│   │   ├── rds_to_parquet_containers.R     # SCE/Seurat RDS → Parquet
│   │   └── rds_to_parquet_indep.R          # Standalone files (e.g. csv) → Parquet
|   |
│   └── query_data/                 # Per user requests - R queries only db views
│       ├── expression.R            # Fetch expression data for a gene+dataset
│       ├── metadata.R              # API client wrappers to fetch sample/cell metadata
│       └── reductions.R            # Fetch UMAP/tSNE coordinates
│
├── view/                               # Shiny modules for pages and UI components such as tables, plots
│   ├── pages/
│   │   ├── landing_page.R              # Main app page
│   │   ├── gene_dataset_selector.R     # Gene-centric modal popup for "Explore Data" page
│   │   ├── data_explore.R              # "Explore Data" page for gene/ dataset selection
│   │   └── profiles.R                  # Profiles about Core Informatics members, link to associated website(s)
|   |
│   └── components/
│       ├── explore_sidebar.R  # Side panel for filtering datases (ie gene + protein search, omic type, organism, cell type, condition filters)
│       ├── dataset_table.R    # Dataset(s) containing searched gene
│       ├── results_table.R    # DE results for selected datasets
│       ├── volcano_plot.R
│       ├── violin_plot.R
│       ├── dots_plot.R
│       ├── histogram_plot.R
│       ├── feature_scatter_plot.R
│       ├── expression_heatmap.R
│       └── umap_plot.R     # Embeddings
├── static/
├── styles/ 
docs/                       # Project notes on production architecture and current direction
tests/
  ├── testthat/             # Unit tests (R)
  └── cypress/              # UI tests via Rhino's Cypress integration
```

## How the app works

1.  Python builds or refreshes the registry database once a new dataset is ingested and added to the YAML file.
2.  DuckDB exposes canonical views over the source data.
3.  FastAPI serves query endpoints that return the data in the format of Arrow IPC.
4.  Shiny calls those endpoints and renders the results as tables and plots.

```         
┌─────────────────────────────────────────────────────┐
│  DuckDB + Parquet files                             │
└─────────────────────────────────────────────────────┘
                       │  read-only file access
                       ▼
┌─────────────────────────────────────────────────────┐
│  Python / FastAPI                                   │
│                                                     │
│  • All DuckDB connections (pool)                    │
│  • All SQL query execution                          │
│  • Arrow IPC serialization                          │
│  • Gene index lookups                               │
│  • Dataset summary aggregation                      │
│  • Usage event logging                              │
│  • Views management                                 │
└──────────────────────┬──────────────────────────────┘
                       │  HTTP + Arrow IPC
                       ▼
┌────────────────────────────────────────────────────────────────┐
│  R / Rhino / Shiny                                             │
│                                                                │
│  • All UI rendering (shiny, bslib, etc.)                       │
│  • All plot generation (plotly)                                │
│  • Session state (selected genes, active datasets)             │
│  • HTTP calls to FastAPI (httr2)                               │
│  • Arrow IPC deserialisation with no direct DuckDB connections │
│                                                                │
└────────────────────────────────────────────────────────────────┘
```

## Running locally

You need both the Python API and the Shiny app running.

### 1. Start the API

From the repository root:

``` bash
python app/logic/startup/main_setup.py
```

If you prefer `uvicorn` directly:

``` bash
uvicorn app.logic.startup.main_setup:app --host 0.0.0.0 --port 8000
```

When the API starts cleanly, it should log the mounted `/api/...` routes.

### 2. Start the Shiny app

From the repository root:

``` r
shiny::runApp('app/main.R', port = 3838, host = "0.0.0.0")
```

## Useful project docs

-   [Architecture summary](docs/architecture_summary.md)

