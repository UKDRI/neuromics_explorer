#!/usr/bin/env Rscript

# Convert Bowles bulk RNA-seq TSV files into registry-friendly parquet assets.
# -----------------------------------------------------------------------------
# Input files:
#   - DEGs.tsv       : wide DE table with one logFC/pvalue/padj block per comparison
#   - Star_counts.tsv: wide raw counts matrix
#
# Output files:
#   - expression.parquet
#   - counts.parquet
#   - feature_annotations.parquet
#   - obs_metadata.parquet

`%||%` <- function(a, b) if (is.null(a)) b else a

#' Split counts matrix's id column into annotation columns.
#'
#' @param x Character vector such as `ENSG...|GENE|protein_coding`.
#'
#' @return A data frame with `feature_id`, `gene_symbol`, and `feature_type`.
parse_feature_col <- function(x) {
  parts <- strsplit(as.character(x), "\\|", fixed = FALSE)

  data.frame(
    feature_id = vapply(parts, function(p) p[1] %||% NA_character_, character(1)),
    gene_symbol = vapply(parts, function(p) p[2] %||% p[1] %||% NA_character_, character(1)),
    feature_type = vapply(parts, function(p) p[3] %||% NA_character_, character(1)),
    stringsAsFactors = FALSE
  )
}

#' Parse sample names into simple metadata (obs) fields.
#'
#' @param meta_col_names Character vector of count-matrix column names.
#'
#' @return A data frame suitable for `obs_metadata.parquet`.
parse_meta_cols <- function(meta_col_names) {
  tokens <- strsplit(as.character(meta_col_names), "_", fixed = TRUE)

  data.frame(
    obs = meta_col_names,
    sample_prefix = vapply(tokens, function(x) x[1] %||% NA_character_, character(1)),
    cell_lineage = vapply(tokens, function(x) x[2] %||% NA_character_, character(1)),
    timepoint = vapply(tokens, function(x) x[3] %||% NA_character_, character(1)),
    batch = vapply(tokens, function(x) x[4] %||% NA_character_, character(1)),
    replicate = vapply(tokens, function(x) x[5] %||% NA_character_, character(1)),
    extra_label = vapply(tokens, function(x) {
      if (length(x) <= 5) return(NA_character_)
      paste(x[6:length(x)], collapse = "_")
    }, character(1)),
    stringsAsFactors = FALSE
  )
}

#' Derive comparison metadata from DE contrast label.
#'
#' @param comparison Comparison suffix taken from a `logFC_*` column name.
#'
#' @return A one-row data frame describing the comparison and timepoint fields.
parse_comparison <- function(comparison) {
  out <- list(
    comparison = comparison,
    comparison_type = if (startsWith(comparison, "(")) "(contrast1)-(contrast2)" else "single_contrast",
    sample_a = NA_character_,
    sample_b = NA_character_,
    condition_a = NA_character_,
    condition_b = NA_character_,
    timepoint_a = NA_character_,
    timepoint_b = NA_character_
  )

  if (grepl("^mutant_[0-9]+mo-wildtype_[0-9]+mo$", comparison)) {
    parts <- strsplit(comparison, "-", fixed = TRUE)[[1]]
    left <- strsplit(parts[1], "_", fixed = TRUE)[[1]]
    right <- strsplit(parts[2], "_", fixed = TRUE)[[1]]

    out$sample_a <- parts[1]
    out$sample_b <- parts[2]
    out$condition_a <- left[1] %||% NA_character_
    out$condition_b <- right[1] %||% NA_character_
    out$timepoint_a <- left[2] %||% NA_character_
    out$timepoint_b <- right[2] %||% NA_character_
    return(as.data.frame(out, stringsAsFactors = FALSE))
  }

  if (grepl("^\\(.+\\)-\\(.+\\)$", comparison)) {
    inner <- regmatches(comparison, gregexpr("\\(([^()]*)\\)", comparison))[[1]]
    inner <- gsub("^\\(|\\)$", "", inner)
    left <- strsplit(inner[1], "-", fixed = TRUE)[[1]]
    right <- strsplit(inner[2], "-", fixed = TRUE)[[1]]
    left_parts <- strsplit(left, "_", fixed = TRUE)
    right_parts <- strsplit(right, "_", fixed = TRUE)

    out$sample_a <- inner[1] %||% NA_character_
    out$sample_b <- inner[2] %||% NA_character_
    out$condition_a <- paste0(left_parts[[1]][1], "-", left_parts[[2]][1]) %||% NA_character_
    out$condition_b <- paste0(right_parts[[1]][1], "-", right_parts[[2]][1]) %||% NA_character_
    out$timepoint_a <- paste0(left_parts[[1]][2], "-", left_parts[[2]][2]) %||% NA_character_
    out$timepoint_b <- paste0(right_parts[[1]][2], "-", right_parts[[2]][2]) %||% NA_character_

    return(as.data.frame(out, stringsAsFactors = FALSE))
  }

  as.data.frame(out, stringsAsFactors = FALSE)
}

#' Reshape wide DE table into a canonical expression map.
#'
#' @param deg_path Path to `DEGs.tsv`.
#'
#' @return A long-form data frame with one row per feature-comparison.
build_expr <- function(deg_path) {
  deg <- utils::read.delim(deg_path, check.names = FALSE, stringsAsFactors = FALSE)
  required_cols <- c("gid", "gname", "gtype")
  missing_cols <- setdiff(required_cols, names(deg))
  if (length(missing_cols) > 0) {
    stop("DEGs.tsv is missing required column(s): ", paste(missing_cols, collapse = ", "))
  }

  logfc_cols <- grep("^logFC_", names(deg), value = TRUE)
  if (length(logfc_cols) == 0) {
    stop("No comparison columns matching 'logFC_*' were found in DEGs.tsv.")
  }

  col_blocks <- lapply(logfc_cols, function(logfc_col) {
    comparison <- sub("^logFC_", "", logfc_col)
    pval_col <- paste0("P.Value_", comparison)
    padj_col <- paste0("adj.P.Val_", comparison)
    z_col <- paste0("z.std_", comparison)

    block <- data.frame(
      feature_id = as.character(deg$gid),
      gene_symbol = as.character(deg$gname),
      feature_type = as.character(deg$gtype),
      log2fc = as.numeric(deg[[logfc_col]]),
      pvalue = as.numeric(deg[[pval_col]] %||% NA),
      padj = as.numeric(deg[[padj_col]] %||% NA),
      z_score = as.numeric(deg[[z_col]] %||% NA),
      stringsAsFactors = FALSE
    )

    cbind(block, parse_comparison(comparison))
  })

  do.call(rbind, col_blocks)
}

#' Build a Bowles counts table from the STAR counts matrix.
#'
#' @param counts_path Path to `Star_counts.tsv`.
#'
#' @return A wide counts data frame with parsed feature annotation columns.
build_counts <- function(counts_path) {
  counts <- utils::read.delim(counts_path, check.names = FALSE, stringsAsFactors = FALSE)
  first_col <- names(counts)[1]
  parsed_features <- parse_feature_col(counts[[first_col]])

  counts[[first_col]] <- NULL

  data.frame(
    gene_symbol = parsed_features$gene_symbol,
    counts,
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
}

#' Create a feature-annotation table.
#'
#' @param expression_df Expression data frame returned by `build_expr()`.
#' @param counts_df Counts data frame returned by `build_counts()`.
#'
#' @return A de-duplicated feature annotation data frame.
build_annot <- function(expression_df, counts_df) {
  source_df <- if (nrow(expression_df) > 0) {
    unique(expression_df[, c("feature_id", "gene_symbol", "feature_type"), drop = FALSE])
  } else {
    unique(counts_df[, c("feature_id", "gene_symbol", "feature_type"), drop = FALSE])
  }

  source_df[!duplicated(source_df$feature_id), , drop = FALSE]
}

#' Write a data frame to Parquet using Arrow.
#'
#' @param df Data frame to convert.
#' @param path Output parquet path.
#'
#' @return The output path.
write_parquet <- function(df, path) {
  if (!requireNamespace("arrow", quietly = TRUE)) {
    stop("Package 'arrow' is required to write parquet outputs.")
  }

  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  arrow::write_parquet(df, path, compression = "snappy")
  path
}

#' Convert Bowles bulk RNA-seq TSV files into parquet assets.
#'
#' @param deg_path Path to the wide DE table.
#' @param counts_path Path to the raw counts matrix.
#' @param output_dir Directory where parquet outputs will be written.
#'
#' @return Invisibly returns the written parquet paths.
convert_tsv_to_parquet <- function(
    deg_path = "data/Bowles/V337M_bulk_RNAseq/DEGs.tsv",
    counts_path = "data/Bowles/V337M_bulk_RNAseq/Star_counts.tsv",
    output_dir = "data/Bowles/V337M_bulk_RNAseq/conversions") {
  expression_df <- build_expr(deg_path)
  counts_df <- build_counts(counts_path)
  feature_df <- build_annot(expression_df, counts_df)
  obs_df <- parse_meta_cols(names(counts_df)[!(names(counts_df) %in% c("feature_id", "gene_symbol", "feature_type"))])

  written <- c(
    write_parquet(expression_df, file.path(output_dir, "expression.parquet")),
    write_parquet(counts_df, file.path(output_dir, "counts.parquet")),
    write_parquet(feature_df, file.path(output_dir, "feature_annotations.parquet")),
    write_parquet(obs_df, file.path(output_dir, "obs_metadata.parquet"))
  )

  message("Wrote parquet file(s):")
  for (path in written) {
    message("  - ", path)
  }

  invisible(written)
}

if (identical(environment(), globalenv()) && !interactive()) {
  args <- commandArgs(trailingOnly = TRUE)
  deg_path <- if (length(args) >= 1) args[[1]] else "data/Bowles/V337M_bulk_RNAseq/DEGs.tsv"
  counts_path <- if (length(args) >= 2) args[[2]] else "data/Bowles/V337M_bulk_RNAseq/Star_counts.tsv"
  output_dir <- if (length(args) >= 3) args[[3]] else "data/Bowles/V337M_bulk_RNAseq/conversions"
  convert_tsv_to_parquet(deg_path = deg_path, counts_path = counts_path, output_dir = output_dir)
}
