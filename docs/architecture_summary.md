# Neuromics Explorer Summary

## Project Overview

Neuromics Explorer is a gene-centric, multi-omics visualisation dashboard for UK DRI labs.
Users search for a gene or protein and discover which datasets contain it, then interactively explore differential expression, cell-type distributions, and dimensionality reductions.
Stack: Rhino + Shiny (UI) · FastAPI (API) · DuckDB + Parquet (storage) · Arrow IPC (R↔Python data transfer).


### User journey

- Type or select one or more genes and/or proteins.
- Click 'Search' button to list datasets containing those terms.
- Click to select one or more datasets from the generated results table.
- Review the metadata preview shown.
- Click 'Explore selected dataset(s)' to open them in the main explorer page.


### Currently Hosted Datasets

- Díaz-Castro (proteomics, bulk) : CSV files converted to DuckDB
- Hong (scrna) : CSV files converted to RDS (SCE object) converted to Parquet
- Hong (proteomics): CSV files converted to DuckDB
- Williams (snrna) : RDS converted to Parquet via rds_to_parquet_containers.R; DE garnered from '40478_2023_1568_MOESM4_ESM.xlsx' and converted to Parquet via williams_xlsx_to_parquet.R
- Webber (bulk) : RDS converted to Parquet via rds_to_parquet_indep.R


## Technical Methods

The frontend Shiny app acts as a thinly layered client.
When a user searches for a gene, protein, or opens a dataset, Shiny sends HTTP requests to the FastAPI service which validates the request.

The request resolves the selected dataset(s) against the registry catalogue, and constructs the appropriate SQL query via DuckDB. The DuckDB engine then reads only the required Parquet files and columns, then applies filters such as dataset, gene symbol, cell type, or thresholds.

For reusable metadata, the backend keeps a small DuckDB registry database containing dataset definitions, column mappings, logical table mappings, gene indexes, and summary tables. That registry is cheap to query and lets the backend normalise heterogeneous datasets into one semantic layer.

Once DuckDB produces a result, the backend converts it to Arrow and returns it over HTTP as an Arrow IPC stream, preserving types, reducing serialisation overhead, and maps naturally to plotting and table rendering in R. The Shiny app deserialises the Arrow response, converts it to an R data frame, and feeds it into Plotly or datatables.


### Arrow IPC streaming

Expression, metadata, and reduction endpoints return Arrow IPC streams (`application/vnd.apache.arrow.stream`). In R:
`httr2::req_perform(req) |> httr2::resp_body_raw() |> arrow::read_ipc_stream()`
This avoids JSON serialisation overhead for large cell matrices (though for small datasets, e.g. proteomics, < 5000 rows, plain JSON can be acceptable).


## Deployment
Deployment is currently done using Docker.

#### Optional environment variables

- `NEX_API_BASE_URL`
  Defaults to `http://127.0.0.1:7000/api`
- `NEX_DUCKDB_POOL_SIZE`
  Controls the size of the read-only DuckDB connection pool used by FastAPI
- `NEX_DUCKDB_THREADS`
  Controls DuckDB thread usage per pooled connection

Notes:
The FastAPI app itself defaults to port `7000` in `app/logic/startup/main_setup.py`.
Production can still use `8000` by setting `NEX_API_BASE_URL`, which points at the mounted `/api` router.

#### API Endpoints & Client

- `app/logic/startup/main_setup.py`
  Starts FastAPI, runs registry/setup tasks, configures pooled DuckDB connections, and mounts the router at `/api`.
- `app/logic/startup/db_pool.py`
  Provides a pool of reusable read-only DuckDB connections for concurrent requests.
- `app/logic/api/endpoints.py`
  Handles SQL and Arrow/JSON responses for dataset search, dataset stats, metadata, expression, embeddings, and comparison queries.
- `app/main.R`
  Sets the API base URL and launches the UI.
- `app/logic/api/api_client.R`
  Calls the FastAPI endpoints and reads Arrow IPC responses into R.

#### Query Contract

- `gene_study_index`
  Used for fast gene-to-dataset discovery prior to joining with `dataset_stats`.
- `dataset_stats`
  Used for compact dataset summaries.
- `v_{lab}_{study_id}`
  Canonical expression/DE view for datasets.
- `vm_{lab}_{study_id}`
  Canonical metadata view for datasets when richer metadata exists separately.


## Schemas

#### neuromics_registry.duckdb

The registry DB is write-locked only during startup. The pool opens it as `read_only=True` to prevent concurrent users hitting a lock error.
`study_description` will be populated from the source DB's study_info.Metadata_Text column during index build.

Table     |	Key columns	    |   Purpose
dataset_registry	study_id, lab_source, dataset_name, omic_type, source_type, data_path, study_description, registered_at	One row per dataset. PK: (study_id, lab_source, omic_type)
column_mappings	study_id, lab_source, canonical_name, original_name, col_category, is_default_gene	Maps canonical names → actual column names per dataset
table_mappings	study_id, lab_source, logical_table, actual_table	Maps logical roles (expression, cell_metadata) to actual table/file paths
gene_study_index	gene_symbol, protein_id, study_id, lab_source, dataset_name, omic_type, organism	Inverted index: gene → datasets. Indexed on (gene_symbol, study_id)
dataset_stats	study_id, lab_source, total_features, n_sig_features, total_cells, cell_types_json, tissues_json, age_range_json…	Pre-computed aggregates for fast filter population
index_build_log	index_name, built_at, row_count	Startup skip-check: rebuilds only if registry newer than index
registry_load_issues / index_build_issues	logged_at, study_id, lab_source, issue	Structured error log for startup issues

#### Parquet storage

File	Canonical-columns	Description
expression.parquet	gene_symbol, protein_id, log2fc, …padj  	    Canonical DE / expression data
counts.parquet	feature_id, counts/values                         Raw counts (sparse long format); shape depends on source and converter script
logcounts.parquet	feature_id, logcounts                          	Normalised counts (sparse long format)
obs_metadata.parquet	obs, cell_type, cluster_id, tissue, …sex	  Per-cell / per-sample metadata often derived from `colData`
feature_annotations.parquet	feature_id, biotype, …chromosome	    Per-feature annotations often derived from `rowData`
PCA.parquet	obs, Dim1, …DimN                                    	PCA coordinates
TSNE.parquet	obs, Dim1, Dim2	                                    t-SNE coordinates
UMAP.parquet	obs, Dim1, Dim2	                                    UMAP coordinates

#### Endpoints

Method | Endpoint | Description | Response
GET | `/health` | Liveness check | JSON
GET | `/api/genes/index` | Gene autocomplete index | Arrow IPC
GET | `/api/proteins/index` | Protein autocomplete index | Arrow IPC
GET | `/api/datasets/all` | List all registered datasets | Arrow IPC
GET | `/api/datasets/search` | Search datasets by gene and/or protein terms | Arrow IPC
GET | `/api/datasets/stats` | Dataset summary stats | Arrow IPC
GET | `/api/datasets/{lab}/{study_id}/expression` | Threshold-filtered semantic expression / DE rows | Arrow IPC
GET | `/api/datasets/{lab}/{study_id}/expression/table` | Paginated expression table | Arrow IPC
GET | `/api/datasets/{lab}/{study_id}/expression/volcano` | Lightweight volcano payload | Arrow IPC
GET | `/api/datasets/{lab}/{study_id}/expression/summary` | One-row expression summary | Arrow IPC
GET | `/api/datasets/{lab}/{study_id}/expression/histogram` | Histogram bins | Arrow IPC
GET | `/api/datasets/{lab}/{study_id}/expression/groups` | Grouped expression for plots such as violin, bar etc. | Arrow IPC
GET | `/api/datasets/{lab}/{study_id}/expression/goi` | Full rows for genes/proteins of interest | Arrow IPC
GET | `/api/datasets/{lab}/{study_id}/expression/feature-values` | Feature values joined to metadata | Arrow IPC
GET | `/api/datasets/{lab}/{study_id}/embeddings` | UMAP / PCA / tSNE coordinates with optional overlays | Arrow IPC
GET | `/api/datasets/{lab}/{study_id}/top-de` | Top-N DE rows used by heatmaps and similarly ranked plots | Arrow IPC
GET | `/api/datasets/{lab}/{study_id}/metadata/options` | Distinct metadata values for filters | Arrow IPC
GET | `/api/datasets/{lab}/{study_id}/metadata` | Full metadata table | Arrow IPC
GET | `/api/compare/expression` | Cross-dataset (expression) comparison | Arrow IPC
