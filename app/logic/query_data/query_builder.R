# Dynamic SQL construction using canonical views (column_mappings) from neuromics_registry.duckdb.
# All downstream data-fetch functions use these helpers — never hardcode column names.
# All queries hit pre-built views (v_{lab}_{id} for expression data, vm_{lab}_{id} for obs/metadata) so they work
# regardless of the heterogeneous source schema underneath.

# Key design:
#  - padj_thresh and lfc_thresh parameters → adjustable with UI sliders without re-fetching all data
#  - fetch_datasets_for_gene()  → drives the gene_dataset_selector modal
#  - fetch_de_results()         → drives plots (eg volcano) and results table
#  - fetch_dataset_stats()      → drives explorer page summaries

box::use(
  DBI[dbExecute, dbIsValid],
  glue[glue, glue_sql],
  dplyr[bind_rows, mutate, filter, arrange, desc],
  purrr[map, keep, compact],
)


# ── DuckDB check ─────────────────────────────────────────────────────────

#' @noRd
check_con <- function(con) {
  if (is.null(con) || !DBI::dbIsValid(con))
    stop("Registry DuckDB connection is not valid.")
}


# ── Arrow querying ───────────────────────────────────────────────────────
#' Use Arrow fetch to execute SQL for zero-copy data transfers
#' DuckDB columnar → Arrow buffer/ memory
#'
#' @param con   DuckDB connection
#' @param sql   SQL string
#' @param collect logical — if TRUE returns data.frame (can be used for small lookups)
#' @return Arrow Table (default) or data.frame
query_arrow <- function(con, sql, collect = FALSE) {
  result <- duckdb::dbSendQuery(con, sql)
  tbl    <- duckdb::duckdb_fetch_arrow(result)
  duckdb::dbClearResult(result)
  if (collect) return(dplyr::collect(tbl))
  tbl
}


# ── All Datasets ───────────────────────────────────────────────────────

#' List all registered datasets (for selector dropdowns / dataset_table)
#' @export
fetch_all_datasets <- function(con) {
  check_con(con)
  query_arrow(con, "
    SELECT
      dr.study_id, dr.lab_source, dr.dataset_name, dr.omic_type,
      dr.organism, dr.source_type,
      ds.total_features, ds.n_sig_features,
      ds.total_samples, ds.total_cells,
      ds.n_conditions, ds.n_cell_types,
      ds.conditions_json, ds.cell_types_json,
      ds.computed_at
    FROM dataset_registry dr
    LEFT JOIN dataset_stats ds
      ON dr.study_id = ds.study_id AND dr.lab_source = ds.lab_source
    ORDER BY dr.lab_source, dr.study_id
  ")
}


# ── Gene search ───────────────────────────────────────────────────────────────
#' Search gene_study_index for any datasets containing a gene/ protein.
#' Used by gene_dataset_selector modal to populate hits table.
#'
#' @param con         DBI connection to neuromics_registry.duckdb
#' @param query       Gene symbol or protein ID (partial match supported)
#' @param omic_type   Filter by omic type; NULL = all
#' @param lab_source  Filter by lab; NULL = all
#' @return data.frame with study_id, lab_source, dataset_name, omic_type,
#'         protein_id, total_features, n_sig_features (joined from dataset_stats)
#' @export
fetch_datasets_for_gene <- function(con, query, omic_type = NULL, lab_source = NULL) {
  check_con(con)
  stopifnot(nchar(trimws(query)) > 0)

  # Sanitise query
  q_safe <- gsub("'", "''", trimws(query))

  omic_clause <- if (!is.null(omic_type) && omic_type != "All")
    glue::glue("AND gi.omic_type = '{omic_type}'") else ""

  lab_clause  <- if (!is.null(lab_source) && lab_source != "All")
    glue::glue("AND gi.lab_source = '{lab_source}'") else ""

  query_arrow(con, glue::glue("
    SELECT
      gi.study_id,
      gi.lab_source,
      gi.dataset_name,
      gi.omic_type,
      gi.gene_symbol,
      gi.protein_id,
      -- Pull pre-computed stats so user can see dataset size at a glance
      ds.total_features,
      ds.n_sig_features,
      ds.total_samples,
      ds.total_cells,
      ds.n_cell_types,
      ds.cell_types_json,
      ds.conditions_json
    FROM gene_study_index gi
    LEFT JOIN dataset_stats ds
      ON gi.study_id = ds.study_id AND gi.lab_source = ds.lab_source
    WHERE gi.gene_symbol ILIKE '%{q_safe}%'
      {omic_clause}
      {lab_clause}
    ORDER BY gi.lab_source, gi.study_id
  "))
}


# ── DE results ────────────────────────────────────────────────────────────────

#' Fetch DE rows for one gene across one dataset.
#' padj_thresh and lfc_thresh are applied at query time → slider changes
#' rerun just this query, not the full data fetch.
#'
#' @param con           DBI connection (source DBs must be already ATTACHed)
#' @param gene          Gene symbol string (exact match on canonical gene_symbol)
#' @param lab_source    lab_source key
#' @param study_id      integer
#' @param padj_thresh   numeric, default 0.05; set NULL to skip filter
#' @param lfc_thresh    numeric |log2FC| minimum, default 0; set NULL to skip
#' @param cell_type     optional cell_type filter (scRNA datasets)
#' @return data.frame of DE rows with canonical column names
#' @export
fetch_de_for_gene <- function(con, gene, lab_source, study_id,
                              padj_thresh = 0.05, lfc_thresh = 0,
                              cell_type = NULL) {
  check_con(con)
  view <- glue::glue("v_{lab_source}_{study_id}")

  # Build optional WHERE clauses — all threshold params come from UI reactives such as sliders
  padj_clause <- if (!is.null(padj_thresh))
    glue::glue("AND (padj IS NULL OR padj < {padj_thresh})") else ""
  lfc_clause  <- if (!is.null(lfc_thresh) && lfc_thresh > 0)
    glue::glue("AND (log2fc IS NULL OR ABS(log2fc) >= {lfc_thresh})") else ""
  ct_clause   <- if (!is.null(cell_type))
    glue::glue("AND cell_type = '{gsub(\"'\",\"''\", cell_type)}'") else ""

  query_arrow(con, glue::glue("
    SELECT
      gene_symbol, human_gene, protein_id, organism,
      log2fc, pvalue, padj,
      abundance_a, abundance_b,
      pct_expressed_a, pct_expressed_b, expression_metric,
      sample_a, sample_b, condition_a, condition_b, cell_type,
      study_id
    FROM {view}
    WHERE gene_symbol = '{gsub(\"'\",\"''\", gene)}'
      {padj_clause}
      {lfc_clause}
      {ct_clause}
    ORDER BY padj ASC NULLS LAST, ABS(log2fc) DESC NULLS LAST
  "))
}


#' Fetch top N DE genes from a dataset (eg for heatmaps, ranked gene lists etc.).
#' Threshold params mirror the sidebar sliders so the list updates reactively.
#'
#' @param direction "both" | "up" | "down"
#' @export
fetch_top_de <- function(con, lab_source, study_id, n = 50,
                         padj_thresh = 0.05, lfc_thresh = 0,
                         cell_type = NULL,
                         direction = c("both", "up", "down")) {
    check_con(con)
    direction <- match.arg(direction)
    view      <- glue::glue("v_{lab_source}_{study_id}")
    # direction for after log2fc extraction
    dir_clause <- switch(direction,
      up   = "AND log2fc > 0",
      down = "AND log2fc < 0",
      ""
    )
    ct_clause <- if (!is.null(cell_type))
      glue::glue("AND cell_type = '{gsub(\"'\",\"''\", cell_type)}'") else ""

    query_arrow(con, glue::glue("
      SELECT
        gene_symbol, log2fc, padj, pvalue,
        abundance_a, abundance_b,
        cell_type, condition_a, condition_b
      FROM {view}
      WHERE padj < {padj_thresh}
      AND (log2fc IS NULL OR ABS(log2fc) >= {lfc_thresh})
      {dir_clause}
      {ct_clause}
      ORDER BY ABS(log2fc) DESC NULLS LAST
      LIMIT {n}
    "))
}


#' Fetch all expression rows for a gene across multiple datasets at once.
#' Returns a stacked data.frame with a dataset_label column for faceting.
#'
#' @param dataset_list  list of list(lab_source, study_id, dataset_name)
#' @export
fetch_de_multi_dataset <- function(con, gene, dataset_list,
                                   padj_thresh = 0.05, lfc_thresh = 0, cell_type = NULL) {
  check_con(con)
  results <- purrr::map(dataset_list, function(ds) {
    tryCatch(
      fetch_de_for_gene(con, gene, ds$lab_source, ds$study_id,
                        padj_thresh, lfc_thresh, cell_type) |>
        dplyr::mutate(dataset_label = ds$dataset_name),
      error = function(e) {
        warning(glue::glue("Query failed for v_{ds$lab_source}_{ds$study_id}: {e$message}"))
        NULL
      }
    )
  })
  dplyr::bind_rows(purrr::compact(results))
}


# ── Dataset-level stats ───────────────────────────────────────────────────────

#' Pull pre-computed summary stats from dataset_stats for 'Explore' page and side panel filter.
#'
#' @param lab_source       optional lab_source filter
#' @param study_id         optional study_id filter
#' @param omic_type        optional omic_type filter
#' @export
fetch_dataset_stats <- function(con, lab_source = NULL, study_id = NULL, omic_type = NULL) {
  check_con(con)

  # Guard against dataset_stats not existing yet: return empty dataframe
  tables <- query_arrow(con,
    "SELECT table_name FROM information_schema.tables
     WHERE table_name = 'dataset_stats'"
  )
  if (nrow(tables) == 0) {
    warning("dataset_stats table not found — has data_summaries.py run?")
    return(data.frame())
  }

  clauses <- character(0)
  if (!is.null(lab_source))   clauses <- c(clauses, glue::glue("lab_source = '{lab_source}'"))
  if (!is.null(study_id))     clauses <- c(clauses, glue::glue("study_id = {study_id}"))
  if (!is.null(omic_type) && omic_type != "All")
    clauses <- c(clauses, glue::glue("omic_type = '{omic_type}'"))

  where <- if (length(clauses)) paste("WHERE", paste(clauses, collapse = " AND ")) else ""
  query_arrow(con, glue::glue("
    SELECT * FROM dataset_stats 
    {where} 
    ORDER BY lab_source, study_id
  "))
}


# ── Metadata & Filtering ──────────────────────────────────────────────────────────────────

#' Fetch sample/cell metadata for sidebar filter population
#' (distinct values for cell_type, condition, tissue dropdowns)
#' @export
fetch_metadata_filter_options <- function(con, lab_source, study_id) {
  check_con(con)
  vm_view <- glue::glue("vm_{lab_source}_{study_id}") # obs/ metadata
  v_view  <- glue::glue("v_{lab_source}_{study_id}")  # expression

  # Check which view exists (scrna has vm_, proteomics may not)
  has_meta <- tryCatch({
    query_arrow(con, glue::glue("SELECT 1 FROM {vm_view} LIMIT 1"))
    TRUE
  }, error = function(e) FALSE)

  src_view <- if (has_meta) vm_view else v_view

  query_arrow(con, glue::glue("
    SELECT
      LIST(DISTINCT sample_a    ORDER BY sample_a)     AS sample_a,
      LIST(DISTINCT sample_b    ORDER BY sample_b)     AS sample_b,
      LIST(DISTINCT condition_a ORDER BY condition_a)  AS condition_a,
      LIST(DISTINCT condition_b ORDER BY condition_b)  AS condition_b,
      LIST(DISTINCT cell_type   ORDER BY cell_type)    AS cell_types,
      LIST(DISTINCT tissue      ORDER BY tissue)       AS tissues
    FROM {src_view}
  "))
}

# Used when the actual data, e.g. to join onto UMAP coordinates or pass to a violin plot from views (vm_ or v_)
#' @export
fetch_all_metadata <- function(con, lab_source, study_id) {
  vm_view <- glue::glue("vm_{lab_source}_{study_id}")
  v_view  <- glue::glue("v_{lab_source}_{study_id}")

  has_vm <- tryCatch({
    query_arrow(con, glue::glue("SELECT 1 FROM {vm_view} LIMIT 1"))
    TRUE
  }, error = function(e) FALSE)

  src_view <- if (has_vm) vm_view else v_view
  query_arrow(con, glue::glue("SELECT * FROM {src_view}"))
}