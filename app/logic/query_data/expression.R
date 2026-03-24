# ─────────────────────────────────────────────────────────────────────────────
# Thin wrappers around the Shiny API client for expression / DE data.
# Components import helpers so HTTP boundary stays isolated from UI code and so
# threshold params come from UI reactive sliders only.
# ─────────────────────────────────────────────────────────────────────────────

box::use(
  app/logic/api/api_client[
    fetch_de_for_gene,
    fetch_de_multi_dataset,
    fetch_top_de,
  ],
)

#' Fetch DE rows for a gene in one dataset with threshold filtering.
#' @export
get_expression <- function(gene, lab_source, study_id,
                           padj_thresh = 0.05, lfc_thresh = 0,
                           cell_type = NULL) {
  fetch_de_for_gene(gene, lab_source, study_id,
                    padj_thresh = padj_thresh,
                    lfc_thresh  = lfc_thresh,
                    cell_type   = cell_type)
}

# TODO Add direction as reactive for user-inputs??
#' Top N DE genes — for plots like heatmap or ranked list components.
#' @export
get_top_de <- function(lab_source, study_id, n = 50,
                       padj_thresh = 0.05, lfc_thresh = 0,
                       cell_type = NULL, direction = "both") {
  fetch_top_de(lab_source, study_id, n = n,
               padj_thresh = padj_thresh,
               lfc_thresh  = lfc_thresh,
               cell_type   = cell_type,
               direction   = direction)
}

#' Gene expression across multiple datasets — for cross-dataset comparison.
#' @export
get_expression_multi <- function(gene, dataset_list,
                                 padj_thresh = 0.05, lfc_thresh = 0) {
  fetch_de_multi_dataset(gene, dataset_list,
                         padj_thresh = padj_thresh,
                         lfc_thresh  = lfc_thresh)
}
