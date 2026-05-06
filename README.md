# NEx_alpha

NEx_alpha is the working codebase for NeurOmicsExplorer, a Shiny-based data explorer for neuromics datasets. The project is split across two runtimes:

-   a Python service that prepares and logs the dataset registry, processes DuckDB and Parquet storage files, runs SQL, and streams Arrow responses
-   an R/Shiny app that handles the UI, interactivity, and visualisation

## What this repository contains

### Rhino App & Directory Structure

```
data/
├── dataset_registry.yml
├── neuromics_registry.duckdb        # Registry DB built at API startup; contains canonical mappings, indexing, and stats (auto-generated)
├── diaz_castro.duckdb               # DuckDB-backed source dataset
├── hong.duckdb                      # DuckDB-backed source dataset
└── lab={lab_name}/modality={omic}/.../
    └── .../conversions/             # Converted datasets as Parquet
        ├── expression.parquet       # DE / expression data
        ├── counts.parquet
        ├── logcounts.parquet
        ├── feature_annotations.parquet
        ├── obs_metadata.parquet     # sample / cell metadata
        ├── UMAP.parquet
        ├── PCA.parquet
        └── tSNE.parquet
app/
├── main.R                              # Shiny entry point - module ties UI + server
├── js/
├── logic/                              # Code independent from Shiny
│   ├── api/
│   │   ├── api_client.R                # R HTTP client wrappers for API requests - fetches data as Arrow IPC streams
│   │   └── endpoints.py                # FastAPI router mounted at `/api` for querying storage files (DuckDB, Parquet, etc) via DuckDB 
│   ├── conversions/                    # Offline conversion scripts
│   │   ├── bowles_tsv_to_parquet.R     # DE .tsv workbook → .parquet (specific to Bowles)
│   │   ├── rds_to_parquet_containers.R # SingleCellExperiment / SummarizedExperiment RDS → Parquet #TODO: add Seurat conversion
│   │   ├── rds_to_parquet_indep.R      # Multiple independent RDS files → Parquet
│   │   └── williams_xlsx_to_parquet.R  # DE excel workbook → expression.parquet (specific to Williams)
│   ├── query_data/                     # Per user requests - R queries only db views
│   │   ├── expression.R                # API client wrappers to fetch expression data for a gene+dataset
│   │   └── metadata.R                  # API client wrappers to fetch sample/cell metadata
│   └── startup/                        # Runs once when the API starts
│       ├── db_pool.py                  # Read-only DuckDB connection pool
│       ├── db_views.py                 # Shared helpers for semantic view management for fast, dynamic querying
│       ├── data_summaries.py           # Computes and stores dataset-level stats summaries
│       ├── main_setup.py               # FastAPI app + spins startup sequence
│       └── registry_parser.py          # Registry ingestion and canonical mappings to import, harmonise, and register current datasets
├── static/
├── styles/
└── view/                               # Shiny pages and plot/ table components
│   ├── pages/
│   │   ├── data_explorer.R             # "Explore Data" page for gene/ dataset selection
│   │   ├── data_submit.R               # Data submissions page
│   │   ├── explore_sidebar.R           # Side panel for filtering datasets
│   │   ├── gene_dataset_selector.R     # Gene-centric modal popup for "Explore Data" page
│   │   ├── landing_page.R              # "Homepage"
│   │   └── profiles.R                  # Profiles about Core Informatics members, link to associated website(s)
│   └── components/
│       ├── dataset_table.R             # Datatables containing searched terms for `Dataset Listings` module
│       ├── dots_plot.R
│       ├── expression_heatmap.R
│       ├── feature_scatter_plot.R
│       ├── histogram_plot.R
│       ├── results_table.R             # DE results for selected datasets
│       ├── umap_plot.R                 # Reductions/ embeddings
│       ├── violin_plot.R
│       └── volcano_plot.R
docs/                       # Project notes on production architecture and current direction
tests/
├── cypress/                # UI / browser tests via Rhino's Cypress integration
├── python/                 # Python tests
└── testthat/               # Unit tests (R)
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
uvicorn app.logic.startup.main_setup:app --host 0.0.0.0 --port 7000
```

When the API starts cleanly, it should log the mounted `/api/...` routes.
Development examples in this repo use port `7000`.
Production can, however, still run on `8000` via uvicorn and setting `NEX_API_BASE_URL`.

### 2. Start the Shiny app

From the repository root:

``` r
shiny::runApp('app/main.R', port = 4848, host = "0.0.0.0")
```

## Useful project docs

-   [Architecture summary](docs/architecture_summary.md)
<!-- -   [API endpoints overview](docs/api-endpoints-overview.md) -->

