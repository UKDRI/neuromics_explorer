# logic/query_data/metadata.R
# ─────────────────────────────────────────────────────────────────────────────

box::use(
  DBI[dbGetQuery],
  glue[glue],
  app/logic/query_data/query_builder[fetch_dataset_stats, fetch_all_datasets,
                                     fetch_metadata_filter_options, fetch_all_metadata],
)

#' Fetch pre-computed dataset stats for one or all datasets.
#' @export
get_dataset_stats <- function(con, lab_source = NULL, study_id = NULL, omic_type = NULL) {
  fetch_dataset_stats(con, lab_source = lab_source, study_id = study_id, omic_type = omic_type)
}

#' Full dataset registry joined with stats — for dataset_table overview component
#' @export
get_all_datasets <- function(con) {
  fetch_all_datasets(con)
}

#' Fetch all metadata
#' @export
get_all_metadata <- function(con, lab_source, study_id) {
  fetch_all_metadata(con, lab_source, study_id)
}

#' Distinct filter options (cell types, conditions, tissues) for sidebar dropdowns.
#' @export
get_filter_options <- function(con, lab_source, study_id) {
  fetch_metadata_filter_options(con, lab_source, study_id)
}

