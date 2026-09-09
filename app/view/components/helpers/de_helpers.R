# helpers/de_helpers.R — shared differential-expression helpers
#
# Is currently used in data_explorer.R (Compare tab) and expression_heatmap.R (Plot tab), so
# any scrna dataset with an empty de_category column uses "Up"/"Down"/"No change" or a single "All" column

box::use(
  dplyr[case_when],
)

# A column counts as usable only if it holds at least one non-NA, non-blank value.
# A column that is present but empty is treated as absent.
#' @export
has_values <- function(df, col, df_names = names(df)) {
  col %in% df_names &&
    any(!is.na(df[[col]]) & nzchar(trimws(as.character(df[[col]]))))
}

#' Builds differential expression (DE) categories by resolving a label for every row of `df`
#'
#' Order:
#'   1. the dataset's own `de_category` when it actually holds values (labs label their
#'      contrasts in their own vocabulary, e.g. "Vessel enriched", "Vessel depleted");
#'   2. another alias column (comparison / DE / category / variable);
#'   3. "Up" / "Down" / "No change" depending on the sidebar thresholds;
#'   4. "All" - a single column, for data with no stats columns to threshold on.
#'
#' @param df           data.frame of DE rows
#' @param padj_thresh  Numeric thresholds. NULL (default) otherwise falls through to "All" 
#' @param lfc_thresh   Numeric thresholds. NULL (default) otherwise falls through to "All"
#' @param df_names     Pre-computed names(df); passed by hot callers to avoid recomputing
#' @return character vector, length nrow(df)
#' @export
build_de_category <- function(df, padj_thresh = NULL, lfc_thresh = NULL,
                              df_names = names(df)) {
  if (has_values(df, "de_category", df_names)) {
    values <- as.character(df$de_category)
    values[is.na(values) | !nzchar(values)] <- "unlabelled"
    return(values)
  }

  for (candidate in c("comparison", "DE", "category", "variable")) {
    if (has_values(df, candidate, df_names)) {
      values <- as.character(df[[candidate]])
      values[is.na(values) | !nzchar(values)] <- "unlabelled"
      return(values)
    }
  }

  # Build only when there is something to threshold on. Proteomics/ embedding-style df with no
  # usable padj/log2fc stats column gets a single categorisation "All" (e.g. rather than all "No change")
  can_derive <- !is.null(padj_thresh) && !is.null(lfc_thresh) &&
    has_values(df, "padj", df_names) && has_values(df, "log2fc", df_names)
  if (!can_derive) return(rep("All", nrow(df)))

  case_when(
    !is.na(df$padj) & df$padj < padj_thresh & !is.na(df$log2fc) & df$log2fc >  lfc_thresh ~ "Up",
    !is.na(df$padj) & df$padj < padj_thresh & !is.na(df$log2fc) & df$log2fc < -lfc_thresh ~ "Down",
    TRUE ~ "No change"
  )
}

#' Collapse duplicate values for one gene x group variable so the strongest (logfc) effect is selected.
#'
#' The heatmap draws log2FC as colour, so the variable e.g, multiple cell types, has to show a single log2FC.
#' Picking by largest |log2FC| shows biggest effect; it also prevents issues ie empty groups/ categories
#' Picking by the most significant/ smallest padj may have bias towards those with tiny but significant effect
#' Picking by mean(x) is avoided because opposing values have a bias towards 0 (white), which reads as "no change"
#' rather than "mixed"; it also can't handle NAs
#'
#' @param x numeric vector; NAs are dropped. Returns NA_real_ when nothing is left, so
#'          pivot_wider()/aggregate() get a length-1 result rather than numeric(0).
#' @export
pick_strongest <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) == 0) return(NA_real_)
  x[which.max(abs(x))]
}

#' Row indices whose gene symbol matches one searched term. If gene present, returns all matching rows
#'
#' Case-insensitive, and splits composite symbols so a dataset storing "GAPDH;GAPD" still
#' matches a search for GAPDH (i.e. hong prot dataset).
#'
#' Prevents stray NA labels pointing at nothing  on volcano plots
#'
#' TODO: normalise composite gene symbols at conversion time (app/logic/conversions/*) so
#' every layer - dataset search, gene index, and plots - sees one clean symbol per row instead
#' of composites ie "GAPDH;GAPD". This client-side split would then be unnecessary, and the
#' dataset-search table would stop showing matches that the plots cannot label.
#' @param symbols character vector of gene symbols, one per row.
#' @param term    single searched term.
#' @export
match_gene_symbol_rows <- function(symbols, term) {
  if (length(symbols) == 0 || length(term) != 1 || is.na(term) || !nzchar(trimws(term))) {
    return(integer(0))
  }
  target <- toupper(trimws(term))
  parts <- strsplit(toupper(as.character(symbols)), "[;,|]")
  which(vapply(parts, function(p) target %in% trimws(p), logical(1)))
}
