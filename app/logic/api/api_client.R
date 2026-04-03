# Central HTTP client for requests.
# Shiny modules call these API helpers to get Arrow IPC streams instead of
# opening DuckDB directly.

box::use(
  httr2[req_headers, req_perform, request, resp_body_raw],
)

ARROW_MEDIA_TYPE <- "application/vnd.apache.arrow.stream"

# Internal helpers in this file intentionally stay `@noRd` because they are
# wiring for one HTTP client implementation, not user-facing package API.
# Exporting them would lock us into low-level URL and request-shaping contracts
# that the UI should never depend on directly.

#' @export
set_api_base_url <- function(base_url) {
  options(nex.api_base_url = normalise_api_base_url(base_url))
}

#' Resolve the configured API base URL.
#'
#' @return A length-1 character vector without trailing slashes.
#' @noRd
api_base_url <- function() {
  base_url <- getOption(
    "nex.api_base_url",
    Sys.getenv("NEX_API_BASE_URL", "http://127.0.0.1:7000/api")
  )
  normalise_api_base_url(base_url)
}

#' Normalise a configured service base URL to the FastAPI router mount point.
#'
#' The production app mounts query routes under `/api`, so probing both
#' `http://host` and `http://host/api` just creates misleading 404 noise when
#' the bare root is not a valid dataset route.
#'
#' @param base_url Base service URL supplied by env var or setter.
#'
#' @return Canonical API base URL ending in `/api`.
#' @noRd
normalise_api_base_url <- function(base_url) {
  base <- sub("/+$", "", base_url)
  if (grepl("/api$", base)) return(base)
  paste0(base, "/api")
}

#' Return the single canonical API base URL.
#'
#' @return Character vector of length 1.
#' @noRd
candidate_base_urls <- function() {
  api_base_url()
}

#' Drop `NULL`, empty, and zero-length query items before URL construction.
#'
#' @param query Named list of query parameters.
#'
#' @return Filtered query list.
#' @noRd
compact_query <- function(query) {
  keep <- vapply(
    query,
    function(x) !is.null(x) && length(x) > 0,
    logical(1)
  )
  query[keep]
}

#' Build a GET URL from a base URL, route path, and repeated query parameters.
#'
#' @param base_url Base service URL.
#' @param path Route path starting with `/`.
#' @param query Named query list. Vector values become repeated `name=value`
#'   pairs, matching the FastAPI list parameter contract.
#'
#' @return Fully encoded URL.
#' @noRd
build_url <- function(base_url, path, query = list()) {
  query <- compact_query(query)
  base_path <- paste0(base_url, path)
  if (length(query) == 0) return(base_path)

  query_parts <- unlist(lapply(names(query), function(name) {
    values <- query[[name]]
    values <- values[!is.na(values)]
    if (length(values) == 0) return(character(0))

    encoded_name <- utils::URLencode(name, reserved = TRUE)
    vapply(
      as.character(values),
      function(value) paste0(encoded_name, "=", utils::URLencode(value, reserved = TRUE)),
      character(1)
    )
  }), use.names = FALSE)

  if (length(query_parts) == 0) return(base_path)

  paste0(base_path, "?", paste(query_parts, collapse = "&"))
}

# Create new HTTP requests with query params to stream results.
#'
#' @param base_url Base service URL.
#' @param path Route path starting with `/`.
#' @param query Named list of query params.
#' @param accept_arrow Whether to request an Arrow IPC response.
#'
#' @return A configured `httr2_request`.
#' @noRd
build_request <- function(base_url, path, query = list(), accept_arrow = TRUE) {
  req <- httr2::request(build_url(base_url, path, query = query))

  if (accept_arrow) {
    req <- httr2::req_headers(req, Accept = ARROW_MEDIA_TYPE)
  }

  req
}

#' Convert Arrow tables to a plain data frame for Shiny consumers.
#'
#' @param tbl Arrow table or existing data frame.
#'
#' @return A data frame.
#' @noRd
arrow_table_to_df <- function(tbl) {
  if (is.data.frame(tbl)) return(tbl)
  as.data.frame(tbl)
}

#' Perform an API request that returns Arrow IPC data.
#'
#' This helper first fails fast when the `arrow` package is unavailable, then
#' iterates across candidate base URLs until one succeeds. The final `stop()`
#' is therefore conditional on all attempts failing.
#'
#' @param path Route path starting with `/`.
#' @param query Named query list.
#' @param collect When `TRUE`, return a data frame; otherwise return an Arrow
#'   Table for callers that want deferred collection.
#'
#' @return A data frame or Arrow table.
#' @noRd
perform_arrow_request <- function(path, query = list(), collect = TRUE) {
  if (!requireNamespace("arrow", quietly = TRUE)) {
    stop(
      "Package 'arrow' is required for the API client. ",
      "Install in the Shiny image before using Arrow IPC endpoints to fetch and stream data."
    )
  }

  last_error <- NULL
  for (base_url in candidate_base_urls()) {
    req <- build_request(base_url, path, query = query, accept_arrow = TRUE)

    resp <- tryCatch(
      httr2::req_perform(req),
      error = function(e) {
        last_error <<- e
        NULL
      }
    )

    if (!is.null(resp)) {
      tbl <- arrow::read_ipc_stream(httr2::resp_body_raw(resp))
      if (!collect) return(tbl)
      return(arrow_table_to_df(tbl))
    }
  }

  last_error_message <- if (is.null(last_error)) {
    "No response received from any candidate base URL."
  } else {
    conditionMessage(last_error)
  }

  stop(
    "API route unavailable at configured base URL(s): ",
    paste(candidate_base_urls(), collapse = ", "),
    ". Restart the FastAPI service if it is still running an older app instance. ",
    "Last error: ", last_error_message
  )
}

#' Perform an API request that returns JSON instead of Arrow IPC.
#'
#' @param path Route path starting with `/`.
#' @param query Named query list.
#'
#' @return Parsed JSON as an R object.
#' @noRd
perform_json_request <- function(path, query = list()) {
  last_error <- NULL
  for (base_url in candidate_base_urls()) {
    req <- build_request(base_url, path, query = query, accept_arrow = FALSE)
    resp <- tryCatch(
      httr2::req_perform(req),
      error = function(e) {
        last_error <<- e
        NULL
      }
    )

    if (!is.null(resp)) {
      return(jsonlite::fromJSON(rawToChar(httr2::resp_body_raw(resp)))) #resp_body_json(resp)
    }
  }

  last_error_message <- if (is.null(last_error)) {
    "No response received from any candidate base URL."
  } else {
    conditionMessage(last_error)
  }

  stop(
    "API route unavailable at configured base URL(s): ",
    paste(candidate_base_urls(), collapse = ", "),
    ". Restart the FastAPI service if it is still running an older app instance. ",
    "Last error: ", last_error_message
  )
}

#' Suggest gene symbols for the modal selectize input.
#'
#' UI connection: preloads the gene selectize choices when the dataset-search
#' modal opens, and narrows those suggestions as the user types.
#' @export
fetch_gene_index_genes <- function(query = NULL, limit = 50000L) {
  tbl <- perform_arrow_request(
    "/genes/index",
    query = list(q = query, limit = as.integer(limit))
  )

  if (!"gene_symbol" %in% names(tbl)) return(character(0))
  tbl$gene_symbol
}

#' Suggest protein identifiers for the modal selectize input.
#'
#' UI connection: mirrors `fetch_gene_index_genes()` for the protein search box.
#' @export
fetch_protein_index_ids <- function(query = NULL, limit = 500L) {
  tbl <- perform_arrow_request(
    "/proteins/index",
    query = list(q = query, limit = as.integer(limit))
  )

  if (!"protein_id" %in% names(tbl)) return(character(0))
  unique(unlist(lapply(tbl$protein_id, function(value) {
    if (is.null(value) || is.na(value) || !nzchar(value)) return(character(0))
    parsed <- tryCatch(jsonlite::fromJSON(value), error = function(e) value)
    parsed <- as.character(parsed)
    parsed[!is.na(parsed) & nzchar(parsed)]
  }), use.names = FALSE))
}

#' Search registered datasets for one or more genes and/or proteins.
#'
#' UI connection: backs the modal results table in
#' `app/view/pages/gene_dataset_selector.R`.
#' @export
fetch_datasets_for_terms <- function(genes = NULL, proteins = NULL,
                                     omic_type = NULL, lab_source = NULL) {
  genes <- unique(trimws(genes))
  genes <- genes[nzchar(genes)]
  proteins <- unique(trimws(proteins))
  proteins <- proteins[nzchar(proteins)]
  if (length(genes) == 0 && length(proteins) == 0) return(data.frame())

  perform_arrow_request(
    "/datasets/search",
    query = list(
      gene = genes,
      protein = proteins,
      omic_type = omic_type,
      lab_source = lab_source
    )
  )
}

#' Backward-compatible gene-only search wrapper.
#' @export
fetch_datasets_for_gene <- function(genes, omic_type = NULL, lab_source = NULL) {
  fetch_datasets_for_terms(genes = genes, omic_type = omic_type, lab_source = lab_source)
}

#' List all registered datasets with summary stats.
#'
#' UI connection: called as soon as the search modal opens so the UI can build
#' lab filters and show dataset-level preview counts before any term search.
#' @export
fetch_all_datasets <- function() {
  perform_arrow_request("/datasets/all")
}

#' Fetch precomputed dataset stats.
#' @export
fetch_dataset_stats <- function(lab_source = NULL, study_id = NULL, omic_type = NULL) {
  perform_arrow_request(
    "/datasets/stats",
    query = list(
      lab_source = lab_source,
      study_id   = study_id,
      omic_type  = omic_type
    )
  )
}

#' Fetch DE rows for one or more gene and/or protein terms in a single dataset.
#' @export
fetch_de_for_terms <- function(genes = NULL, proteins = NULL,
                               lab_source, study_id,
                               padj_thresh = 0.05, lfc_thresh = 0,
                               cell_type = NULL) {
  genes <- unique(trimws(genes))
  genes <- genes[nzchar(genes)]
  proteins <- unique(trimws(proteins))
  proteins <- proteins[nzchar(proteins)]
  if (length(genes) == 0 && length(proteins) == 0) return(data.frame())

  perform_arrow_request(
    sprintf("/datasets/%s/%s/expression", lab_source, study_id),
    query = list(
      gene      = genes,
      protein   = proteins,
      padj      = padj_thresh,
      lfc       = lfc_thresh,
      cell_type = cell_type
    )
  )
}

#' Fetch the full semantic DE / expression rows for one dataset.
#' The modal search terms are not applied here; this is the dataset-level path
#' used by the Expression and Plots tabs after a dataset has been selected.
#' @export
fetch_dataset_expression <- function(lab_source, study_id,
                                     padj_thresh = 0.05, lfc_thresh = 0,
                                     cell_type = NULL,
                                     genes = NULL,
                                     proteins = NULL) {
  genes <- unique(trimws(genes %||% character(0)))
  genes <- genes[nzchar(genes)]
  proteins <- unique(trimws(proteins %||% character(0)))
  proteins <- proteins[nzchar(proteins)]

  perform_arrow_request(
    sprintf("/datasets/%s/%s/expression", lab_source, study_id),
    query = list(
      gene      = genes,
      protein   = proteins,
      padj      = padj_thresh,
      lfc       = lfc_thresh,
      cell_type = cell_type
    )
  )
}

#' Fetch embedding coordinates and optional selected-term expression overlays.
#'
#' UI connection: powers the single-cell UMAP/PCA/tSNE plot in the explorer
#' Plot tab for parquet-backed sc/snRNA datasets.
#' @export
fetch_dataset_embeddings <- function(lab_source, study_id,
                                     reduction = c("umap", "pca", "tsne"),
                                     assay = c("expression", "counts"),
                                     genes = NULL,
                                     proteins = NULL,
                                     max_points = 75000L) {
  reduction <- match.arg(reduction)
  assay <- match.arg(assay)

  genes <- unique(trimws(genes %||% character(0)))
  genes <- genes[nzchar(genes)]
  proteins <- unique(trimws(proteins %||% character(0)))
  proteins <- proteins[nzchar(proteins)]

  perform_arrow_request(
    sprintf("/datasets/%s/%s/embeddings", lab_source, study_id),
    query = list(
      reduction = reduction,
      assay = assay,
      gene = genes,
      protein = proteins,
      max_points = as.integer(max_points)
    )
  )
}

#' Backward-compatible gene-only DE wrapper.
#' @export
fetch_de_for_gene <- function(gene, lab_source, study_id,
                              padj_thresh = 0.05, lfc_thresh = 0,
                              cell_type = NULL) {
  fetch_de_for_terms(
    genes = gene,
    lab_source = lab_source,
    study_id = study_id,
    padj_thresh = padj_thresh,
    lfc_thresh = lfc_thresh,
    cell_type = cell_type
  )
}

#' Fetch the full semantic DE / expression rows for multiple selected datasets.
#' This is the combined table / Compare tab path after modal selection.
#' @export
fetch_expression_multi_dataset <- function(dataset_list,
                                           padj_thresh = 0.05, lfc_thresh = 0,
                                           cell_type = NULL,
                                           genes = NULL,
                                           proteins = NULL) {
  if (is.null(dataset_list)) return(data.frame())

  if (is.data.frame(dataset_list)) {
    if (nrow(dataset_list) == 0) return(data.frame())
    dataset_keys <- paste(dataset_list$lab_source, dataset_list$study_id, sep = ":")
  } else {
    if (length(dataset_list) == 0) return(data.frame())
    dataset_keys <- vapply(
      dataset_list,
      function(ds) paste(ds$lab_source, ds$study_id, sep = ":"),
      character(1)
    )
    dataset_list <- do.call(rbind, lapply(dataset_list, as.data.frame))
  }

  rows <- perform_arrow_request(
    "/compare/expression",
    query = list(
      dataset = dataset_keys,
      gene = unique(trimws(genes %||% character(0))),
      protein = unique(trimws(proteins %||% character(0))),
      padj = padj_thresh,
      lfc = lfc_thresh,
      cell_type = cell_type
    )
  )

  if (nrow(rows) == 0) return(rows)

  if ("dataset_name" %in% names(dataset_list)) {
    key_map <- unique(dataset_list[, intersect(c("lab_source", "study_id", "dataset_name", "omic_type"), names(dataset_list)), drop = FALSE])
    rows <- merge(rows, key_map, by = c("lab_source", "study_id"), all.x = TRUE, sort = FALSE)
    rows$dataset_label <- rows$dataset_name
  }

  rows
}

#' Fetch top DE rows for one dataset.
#' @export
fetch_top_de <- function(lab_source, study_id, n = 50,
                         padj_thresh = 0.05, lfc_thresh = 0,
                         cell_type = NULL,
                         direction = c("both", "up", "down")) {
  direction <- match.arg(direction)

  perform_arrow_request(
    sprintf("/datasets/%s/%s/top-de", lab_source, study_id),
    query = list(
      n         = as.integer(n),
      padj      = padj_thresh,
      lfc       = lfc_thresh,
      cell_type = cell_type,
      direction = direction
    )
  )
}

#' Fetch DE rows for multiple datasets so comparison views can bind them together.
#' @export
fetch_de_multi_dataset <- function(gene = NULL, proteins = NULL, dataset_list,
                                   padj_thresh = 0.05, lfc_thresh = 0,
                                   cell_type = NULL) {
  if (is.null(dataset_list)) return(data.frame())

  if (is.data.frame(dataset_list)) {
    if (nrow(dataset_list) == 0) return(data.frame())
    dataset_keys <- paste(dataset_list$lab_source, dataset_list$study_id, sep = ":")
  } else {
    if (length(dataset_list) == 0) return(data.frame())
    dataset_keys <- vapply(
      dataset_list,
      function(ds) paste(ds$lab_source, ds$study_id, sep = ":"),
      character(1)
    )
    dataset_list <- do.call(rbind, lapply(dataset_list, as.data.frame))
  }

  genes <- unique(trimws(gene))
  genes <- genes[nzchar(genes)]
  proteins <- unique(trimws(proteins))
  proteins <- proteins[nzchar(proteins)]
  if (length(genes) == 0 && length(proteins) == 0) return(data.frame())

  rows <- perform_arrow_request(
    "/compare/expression",
    query = list(
      gene = genes,
      protein = proteins,
      dataset = dataset_keys,
      padj = padj_thresh,
      lfc = lfc_thresh,
      cell_type = cell_type
    )
  )

  if (nrow(rows) == 0) return(rows)

  if ("dataset_name" %in% names(dataset_list)) {
    key_map <- unique(dataset_list[, intersect(c("lab_source", "study_id", "dataset_name", "omic_type"), names(dataset_list)), drop = FALSE])
    rows <- merge(rows, key_map, by = c("lab_source", "study_id"), all.x = TRUE, sort = FALSE)
    rows$dataset_label <- rows$dataset_name
  }

  rows
}

#' Fetch distinct filter options for one dataset.
#'
#' UI connection: used when the modal or sidebar needs dropdown-ready metadata
#' values without paying for a full metadata table fetch.
#' @export
fetch_metadata_filter_options <- function(lab_source, study_id) {
  perform_arrow_request(
    sprintf("/datasets/%s/%s/metadata/options", lab_source, study_id)
  )
}

#' Fetch full metadata rows for one dataset.
#'
#' UI connection: reserved for views that need all metadata rows, not just
#' unique filter values.
#' @export
fetch_all_metadata <- function(lab_source, study_id) {
  perform_arrow_request(
    sprintf("/datasets/%s/%s/metadata", lab_source, study_id)
  )
}

#' @noRd
`%||%` <- function(a, b) if (!is.null(a)) a else b
