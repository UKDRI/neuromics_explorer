#!/usr/bin/env Rscript

# Convert dev nf-core bulk RNA-seq outputs into registry-friendly parquet assets.
# -----------------------------------------------------------------------------
# Expected inputs:
#   - annotation/*.anno.tsv
#   - differential/*.deseq2.results_filtered.tsv
#   - processed_abundance/all.normalised_counts.tsv
#   - optional logcounts matrix when a separate log-scale assay is available
#
# Outputs:
#   - expression.parquet
#   - counts.parquet (raw or normalised, latter preferred)
#   - logcounts.parquet (when available)
#   - feature_annotations.parquet
#   - obs_metadata.parquet

`%||%` <- function(a, b) if (is.null(a)) b else a

#' Build one-row-per-feature annotations from the nf-core annotation table.
#'
#' @param annotation_path Path to `*.anno.tsv`.
#'
#' @return A de-duplicated feature annotation data frame.
build_feature_annotations <- function(annotation_path) {
  annot <- utils::read.delim(annotation_path, check.names = FALSE, stringsAsFactors = FALSE)
  if (!all(c("gene_id", "gene_name") %in% names(annot))) {
    stop("Annotation file must contain at least 'gene_id' and 'gene_name'.")
  }

  annot$feature_id <- as.character(annot$gene_id)
  annot$gene_symbol <- as.character(annot$gene_name)
  if ("gene_biotype" %in% names(annot)) {
    annot$feature_type <- as.character(annot$gene_biotype)
  } else if ("transcript_biotype" %in% names(annot)) {
    annot$feature_type <- as.character(annot$transcript_biotype)
  } else {
    annot$feature_type <- NA_character_
  }

  keep_cols <- unique(c(
    "feature_id",
    "gene_symbol",
    "feature_type",
    intersect(
      c(
        "chromosome", "start", "end", "width",
        "source", "type",
        "gene_version", "transcript_id", "gene_source", "gene_biotype"
      ),
      names(annot)
    )
  ))

  annot <- annot[, keep_cols, drop = FALSE]
  annot <- annot[!duplicated(annot$transcript_id), , drop = FALSE] # changed from annot$feature_id as there are duplicate feature_id, gene_name, or gene_id for different transcripts
  rownames(annot) <- NULL
  annot
}

#' Parse one genotype comparison label from its filename.
#'
#' @param path Path to a `*.deseq2.results_filtered.tsv` file.
#'
#' @return A named list of comparison metadata.
parse_differential_filename <- function(path) {
  filename <- basename(path)
  differential_prefix <- sub("_.*", "", filename)

  comparison <- sub("\\.deseq2\\.results_filtered\\.tsv$", "", filename, ignore.case = TRUE)
  comparison <- sub("^[^_]+_*", "", comparison, ignore.case = TRUE)
  parts <- strsplit(comparison, "__", fixed = TRUE)[[1]]
  left <- parts[1] %||% comparison
  right <- parts[2] %||% NA_character_

  block <- NA_character_
  block_variable <- grep("^block_", parts, value = TRUE)
  if (length(block_variable) > 0) block <- sub("^block_", "", block_variable[1])

  list(
    comparison = comparison,
    differential_prefix = differential_prefix,
    block = block,
    sample_a = left,
    sample_b = right,
    condition_a = left,
    condition_b = right
  )
}

#' Standardise and combine multiple DESeq2 result tables.
#'
#' @param differential_paths Character vector of DE result file paths.
#' @param feature_df Feature annotation data frame from `build_feature_annotations()`.
#'
#' @return A single expression data frame spanning all supplied comparisons.
build_expression_table <- function(differential_paths, feature_df) {
  if (length(differential_paths) == 0) {
    stop("At least one differential result file is required.")
  }

  feature_df <- feature_df[!duplicated(feature_df$feature_id), , drop = FALSE]  #since deduplication wasnt done for annot

  expr_blocks <- lapply(differential_paths, function(path) {
    df <- utils::read.delim(path, check.names = FALSE, stringsAsFactors = FALSE)
    if (!all(c("gene_id", "log2FoldChange", "pvalue", "padj") %in% names(df))) {
      stop("Differential file is missing one or more required columns: ", basename(path))
    }
    if (nrow(df) == 0) {
      message("Skipping empty differential result: ", basename(path))
      return(NULL)
    }

    meta <- parse_differential_filename(path)

    merged <- merge(
      df,
      feature_df[, c("feature_id", "gene_symbol", "feature_type"), drop = FALSE],
      by.x = "gene_id",
      by.y = "feature_id",
      all.x = TRUE,
      sort = FALSE
    )
    gene_symbol <- as.character(merged$gene_symbol)
    # If gene name empty, add ensembl or feature id
    # gene_symbol[is.na(gene_symbol) | !nzchar(gene_symbol)] <- as.character(merged$gene_id)
    missing_gene <- is.na(gene_symbol) | gene_symbol == ""
    gene_symbol[missing_gene] <- as.character(merged$gene_id[missing_gene])

    data.frame(
      feature_id = as.character(merged$gene_id),
      gene_symbol = gene_symbol,
      feature_type = as.character(merged$feature_type),
      protein_id = NA_character_,
      log2fc = as.numeric(merged$log2FoldChange),
      pvalue = as.numeric(merged$pvalue),
      padj = as.numeric(merged$padj),
      base_mean = as.numeric(merged$baseMean %||% NA),
      lfc_se = as.numeric(merged$lfcSE %||% NA),
      comparison = meta$comparison,
      differential_prefix = meta$differential_prefix,
      block = meta$block,
      sample_a = meta$sample_a,
      sample_b = meta$sample_b,
      condition_a = meta$condition_a,
      condition_b = meta$condition_b,
      stringsAsFactors = FALSE
    )
  })

  do.call(rbind, expr_blocks)
}

#' Convert a wide matrix-like table into sparse long format.
#'
#' @param path Path to a counts or normalised-counts TSV.
#' @param value_name Name of the numeric value column to create.
#' @param feature_id_col Name of the feature identifier column.
#' @param drop_cols Optional extra columns to exclude from assay values.
#'
#' @return A sparse long-format data frame with zero values removed.
build_sparse_long_matrix <- function(path, value_name = "counts",
                                     feature_id_col = "gene_id",
                                     drop_cols = character(0)) {
  df <- utils::read.delim(path, check.names = FALSE, stringsAsFactors = FALSE)
  if (!feature_id_col %in% names(df)) {
    stop("Matrix file does not contain feature id column '", feature_id_col, "': ", basename(path))
  }

  counts_cols <- setdiff(names(df), c(feature_id_col, drop_cols))
  feature_ids <- as.character(df[[feature_id_col]])

  blocks <- lapply(counts_cols, function(col_name) {
    values <- as.numeric(df[[col_name]])
    keep <- !is.na(values) & values != 0
    if (!any(keep)) return(NULL)

    data.frame(
      feature_id = feature_ids[keep],
      obs = col_name,
      value = values[keep],
      stringsAsFactors = FALSE
    )
  })

  blocks <- blocks[!vapply(blocks, is.null, logical(1))]    # remove NULL blocks
  if (length(blocks) == 0) {
    out <- data.frame(feature_id = character(0), obs = character(0), value = numeric(0), stringsAsFactors = FALSE)
  } else {
    out <- do.call(rbind, blocks)
  }

  names(out)[3] <- value_name
  out
}

#' Build minimal sample metadata from matrix column names.
#'
#' @param obs Character vector of sample identifiers.
#'
#' @return An `obs_metadata` data frame.
build_obs_metadata <- function(obs) {
  data.frame(
    obs = obs,
    sample_id = obs,
    stringsAsFactors = FALSE
  )
}

#' Write a data frame to Parquet.
#'
#' @param df Data frame to serialise.
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

#' Convert an nf-core bulk RNA-seq results into parquet.
#'
#' @param annotation_path Path to the annotation TSV.
#' @param differential_dir Directory containing `*.deseq2.results_filtered.tsv`.
#' @param counts_path Path to `all.normalised_counts.tsv`.
#' @param logcounts_path Optional path to a separate logcounts matrix.
#' @param output_dir Output directory for parquet.
#'
#' @return Invisibly returns the written parquet paths.
convert_nf_bulk_to_parquet <- function(
    annotation_path,
    differential_dir,
    counts_path,
    logcounts_path = NULL,
    output_dir) {
  if (missing(annotation_path) || !nzchar(annotation_path)) {
    stop("Provide 'annotation_path'.")
  }
  if (missing(differential_dir) || !nzchar(differential_dir)) {
    stop("Provide 'differential_dir'.")
  }
  if (missing(counts_path) || !nzchar(counts_path)) {
    stop("Provide 'counts_path' pointing to a counts file like all.normalised_counts.tsv.")
  }
  if (missing(output_dir) || !nzchar(output_dir)) {
    stop("Provide 'output_dir'.")
  }

  differential_paths <- sort(list.files(
    differential_dir,
    pattern = "^[^_]+_.*\\.deseq2\\.results_filtered\\.tsv$",
    full.names = TRUE
  ))

  feature_df <- build_feature_annotations(annotation_path)
  expression_df <- build_expression_table(differential_paths, feature_df)

  counts_header <- names(utils::read.delim(counts_path, nrows = 1, check.names = FALSE))
  counts_df <- build_sparse_long_matrix(
    counts_path,
    value_name = "counts",
    feature_id_col = "gene_id",
    drop_cols = intersect(c("transcript_id(s)"), counts_header)
  )

  # Merge counts with gene symbols from feature_df to replace Ensembl or other feature ids with gene names
  counts_df <- merge(
    counts_df,
    feature_df[, c("feature_id", "gene_symbol"), drop = FALSE],
    by = "feature_id",
    all.x = TRUE,
    sort = FALSE
  )
  counts_df <- counts_df |>
    select(, -feature_id) |>
    rename(, feature_id = gene_symbol) |>
    relocate(, feature_id)

  # Deduplicate by keeping first occurrence per feature_id (e.g. in case of multiple comparisons or transcripts)
  counts_df <- counts_df[!duplicated(counts_df[, c("feature_id", "obs", "counts")]), , drop = FALSE]

  obs_values <- sort(unique(counts_df$obs))
  obs_df <- build_obs_metadata(obs_values)

  written <- c(
    write_parquet(expression_df, file.path(output_dir, "expression.parquet")),
    write_parquet(counts_df, file.path(output_dir, "counts.parquet")),
    write_parquet(feature_df, file.path(output_dir, "feature_annotations.parquet")),
    write_parquet(obs_df, file.path(output_dir, "obs_metadata.parquet"))
  )

  if (!is.null(logcounts_path) && file.exists(logcounts_path)) {
    logcounts_df <- build_sparse_long_matrix(
      logcounts_path,
      value_name = "logcounts",
      feature_id_col = "gene_id"
    )
    # Replace Ensembl/feature ids with gene symbols
    logcounts_df <- merge(
      logcounts_df,
      feature_df[, c("feature_id", "gene_symbol"), drop = FALSE],
      by = "feature_id",
      all.x = TRUE,
      sort = FALSE
    )
    # Deduplicate by keeping first occurrence per feature_id (in case of multiple comparisons)
    logcounts_df <- logcounts_df[!duplicated(logcounts_df[, c("feature_id", "obs", "logcounts")]), , drop = FALSE]

    written <- c(
      written,
      write_parquet(logcounts_df, file.path(output_dir, "logcounts.parquet"))
    )
  }

  message("Wrote parquet file(s):")
  for (path in written) {
    message("  - ", path)
  }

  invisible(written)
}

if (sys.nframe() == 0) {
  args <- commandArgs(trailingOnly = TRUE)
  if (length(args) < 4) {
    stop(
      paste(
        "Usage:",
        "Rscript app/logic/conversions/nf_bulk_to_parquet.R",
        "<annotation_path> <differential_dir> <counts_path> <output_dir> [logcounts_path]"
      )
    )
  }

  annotation_path <- args[[1]]
  differential_dir <- args[[2]]
  counts_path <- args[[3]]
  output_dir <- args[[4]]
  logcounts_path <- if (length(args) >= 5 && nzchar(args[[5]])) args[[5]] else NULL
  convert_nf_bulk_to_parquet(
    annotation_path = annotation_path,
    differential_dir = differential_dir,
    counts_path = counts_path,
    logcounts_path = logcounts_path,
    output_dir = output_dir
  )
}
