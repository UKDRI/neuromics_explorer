# helpers/explorer_helpers.R — optional shared utilities
box::use(shiny[tags, actionLink])

#' @export
breadcrumb_ui <- function(ns, label = "Back to Landscape View") {
  actionLink(ns("back_to_landscape"), paste("\u2190", label))
}


#' @param n             numeric
#' @param entity_name   ie drug name for signature drug panel
#' @export
selection_count_text <- function(n, entity_name) {
  paste0("Selected: ", n, " ", entity_name, if (n != 1) "s" else "")
}