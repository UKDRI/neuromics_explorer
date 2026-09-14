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

#' Choose which rows to label per searched gene, and report what was left out e.g. volcano plot hover
#' tooltip.
#'
#' Volcano payloads guarantees the searched terms are present, while `cap` is no. of most significant rows per term that will be visible.
#'
#' Matching is case-insensitive and splits composite symbols, so "GAPDH;GAPD" (hong proteomics)
#' matches a GAPDH search. Returns integer(0), never NA, so an absent gene draws no stray label.
#'
#' TODO: normalise composite gene symbols at conversion time (app/logic/conversions/*) so every
#' layer - dataset search, gene index, and plots - sees one clean symbol per row. This
#' client-side split would then be unnecessary, and the dataset-search table would stop showing
#' matches that the plots cannot label.
#'
#' @param symbols character vector of gene symbols, one per row (composite "A;B" supported).
#' @param terms   searched terms.
#' @param score   numeric ranking score per row, higher = kept first (-log10(p)).
#' @param cap     maximum labels per term.
#' @return list(idx = row indices to label,
#'              summary = data.frame(term, matched, labelled) - one row per term).
#' @export
pick_label_rows <- function(symbols, terms, score, cap = 4L) {
  empty <- list(
    idx = integer(0),
    summary = data.frame(term = character(0), matched = integer(0),
                         labelled = integer(0), stringsAsFactors = FALSE)
  )
  terms <- unique(trimws(as.character(if (is.null(terms)) character(0) else terms)))
  terms <- terms[!is.na(terms) & nzchar(terms)]
  if (length(symbols) == 0 || length(terms) == 0) return(empty)

  # Matches composite symbols, so a dataset storing "GAPDH;GAPD" is labelled for a GAPDH search.
  parts <- strsplit(toupper(as.character(symbols)), "[;,|]")
  owner <- rep.int(seq_along(parts), lengths(parts))
  flat  <- trimws(unlist(parts, use.names = FALSE))

  idx <- integer(0)
  rows <- vector("list", length(terms))
  for (i in seq_along(terms)) {
    m <- unique(owner[!is.na(flat) & flat == toupper(terms[i])])
    n <- length(m)
    if (n > 1) {
      s <- if (length(score) >= max(m)) score[m] else rep(0, n)
      s[!is.finite(s)] <- -Inf
      m <- m[order(s, decreasing = TRUE)]
    }
    keep <- if (n > cap) m[seq_len(cap)] else m
    idx <- c(idx, keep)
    rows[[i]] <- data.frame(term = terms[i], matched = n, labelled = length(keep),
                            stringsAsFactors = FALSE)
  }
  list(idx = unique(idx), summary = do.call(rbind, rows))
}

#' Human-readable notes for whatever pick_label_rows() left out. Used in hover text.
#'
#' Returns NULL when every match is labelled and nothing is absent.
#' @param summary_df  the `summary` element of pick_label_rows().
#' @return character vector of short phrases, or NULL.
#' @export
label_cap_notes <- function(summary_df) {
  if (is.null(summary_df) || nrow(summary_df) == 0) return(NULL)
  notes <- character(0)
  absent <- summary_df[summary_df$matched == 0, , drop = FALSE]
  capped <- summary_df[summary_df$matched > summary_df$labelled, , drop = FALSE]
  if (nrow(capped) > 0) {
    notes <- c(notes, sprintf("%s: %d of %d labelled",
                              capped$term, capped$labelled, capped$matched))
  }
  if (nrow(absent) > 0) {
    notes <- c(notes, sprintf("%s: not in this dataset", absent$term))
  }
  if (length(notes) == 0) NULL else notes
}
