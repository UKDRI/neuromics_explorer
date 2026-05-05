#!/usr/bin/env Rscript

# Convert the Williams Seeker2023 Excel DE workbook into per-dataset parquet files.
# -----------------------------------------------------------------------------
# This is intentionally much simpler than the generic RDS conversion scripts:
# the workbook already contains flat DE-style tables, so the job here is to:
#   1. read the relevant sheets,
#   2. standardise the columns to the semantic names the API expects,
#   3. split the all-cell-types sheet by lineage,
#   4. write `expression.parquet` into each Williams conversions folder.
#
# The registry can keep pointing at `logcounts.parquet` until this script has
# been run and the generated expression parquet files have been checked.

#' Drop a duplicated unnamed first column when Excel stores row names separately.
#'
#' @param df Data frame returned by `readxl::read_xlsx()`.
#'
#' @return A cleaned data frame.
clean_excel_table <- function(df) {
  if (ncol(df) == 0) return(df)

  first_name <- names(df)[1]
  if (grepl("^\\.\\.\\.[0-9]+$", first_name) || !nzchar(first_name)) {
    names(df)[1] <- "row_feature"
  }

  if ("row_feature" %in% names(df) && "gene" %in% names(df)) {
    same_feature <- !is.na(df$row_feature) & !is.na(df$gene) & df$row_feature == df$gene
    if (all(same_feature | !nzchar(as.character(df$row_feature)))) {
      df$row_feature <- NULL
    }
  }

  if (!"gene" %in% names(df) && "row_feature" %in% names(df)) {
    names(df)[names(df) == "row_feature"] <- "gene"
  }

  df
}

#' Standardise one Williams DE sheet to the semantic expression schema.
#'
#' @param df Raw DE table.
#' @param default_cell_type Optional fallback lineage for sheets without one.
#' @param default_condition_a Optional fallback condition A label.
#' @param default_condition_b Optional fallback condition B label.
#'
#' @return A data frame ready to write as `expression.parquet`.
standardise_williams_expression <- function(df,
                                            default_cell_type = NA_character_,
                                            default_sample_a = NA_character_,
                                            default_sample_b = NA_character_,
                                            default_condition_a = NA_character_,
                                            default_condition_b = NA_character_) {
  df <- clean_excel_table(df)

  ensure_col <- function(data, name, default = NA) {
    if (!name %in% names(data)) data[[name]] <- default
    data
  }

  for (required_col in c("gene", "p_val", "avg_log2FC", "p_val_adj", "pct.1", "pct.2")) {
    df <- ensure_col(df, required_col, NA)
  }
  df <- ensure_col(df, "cluster", NA)
  df <- ensure_col(df, "cell_lineage", default_cell_type)
  df <- ensure_col(df, "variable", NA)

  out <- data.frame(
    gene_symbol = as.character(df$gene),
    human_gene = as.character(df$gene),
    protein_id = NA_character_,
    organism = "unknown",
    log2fc = suppressWarnings(as.numeric(df$avg_log2FC)),
    pvalue = suppressWarnings(as.numeric(df$p_val)),
    padj = suppressWarnings(as.numeric(df$p_val_adj)),
    abundance_a = NA_real_,
    abundance_b = NA_real_,
    pct_expressed_a = suppressWarnings(as.numeric(df[["pct.1"]])),
    pct_expressed_b = suppressWarnings(as.numeric(df[["pct.2"]])),
    expression_metric = "avg_log2FC",
    sample_a = ifelse(is.na(default_sample_a), NA_character_, default_sample_a),
    sample_b = ifelse(is.na(default_sample_b), NA_character_, default_sample_b),
    condition_a = ifelse(is.na(default_condition_a), NA_character_, default_condition_a),
    condition_b = ifelse(is.na(default_condition_b), NA_character_, default_condition_b),
    cell_type = as.character(df$cell_lineage),
    cluster_id = ifelse(is.na(df$cluster), as.character(df$cluster), NA_character_),
    cluster_description = as.character(df$variable),
    stringsAsFactors = FALSE
  )

  out$gene_symbol[is.na(out$gene_symbol) | !nzchar(out$gene_symbol)] <- NA_character_
  out$cell_type[is.na(out$cell_type) | !nzchar(out$cell_type)] <- default_cell_type
  out$cluster_id[is.na(out$cluster_id) | !nzchar(out$cluster_id)] <- NA_character_
  out
}

#' Write one expression parquet file.
#'
#' @param df Standardised expression data frame.
#' @param output_dir Target dataset conversion directory.
#'
#' @return Path to the parquet file.
write_expression_parquet <- function(df, output_dir) {
  if (!requireNamespace("arrow", quietly = TRUE)) {
    stop("Package 'arrow' is required to write parquet outputs.")
  }

  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  out_path <- file.path(output_dir, "expression.parquet")
  arrow::write_parquet(df, out_path, compression = "snappy")
  out_path
}

#' Create Williams expression parquet files from the Seeker2023 workbook.
#'
#' @param xlsx_path Workbook containing Williams DE sheets.
#' @param output_root Root directory containing Williams conversion folders.
#'
#' @return Invisibly returns written parquet paths.
convert_williams_xlsx_to_parquet <- function(
    xlsx_path = "data/Williams/snrna/03_Seeker_seurat_objects/40478_2023_1568_MOESM4_ESM.xlsx",
    output_root = "data/Williams/snrna/conversions") {
  if (!requireNamespace("readxl", quietly = TRUE)) {
    stop("Package 'readxl' is required to read the Williams Excel workbook.")
  }

  lineage_to_folder <- c(
    oligodendroglia = "02_HCA_oligodendroglia",
    astrocytes = "03_HCA_astrocytes",
    microglia_macrophages = "04_HCA_microglia",
    vascualar_cells = "05_HCA_vascular_cells",
    neurons = "06_HCA_neurons",
    oligos_opcs_ls = "07_HCA_oligos_opcs_LS"
  )

  normalise_key <- function(x) {
    tolower(gsub("[^a-z0-9]+", "_", trimws(as.character(x))))
  }

  written <- character(0)

  all_celltypes_raw <- readxl::read_xlsx(xlsx_path, sheet = "All_Celltypes")
  all_celltypes_df <- standardise_williams_expression(all_celltypes_raw)
  written <- c(
    written,
    write_expression_parquet(
      all_celltypes_df,
      file.path(output_root, "01_HCA_all_celltypes")
    )
  )

  lineage_keys <- normalise_key(all_celltypes_df$cell_type)
  for (lineage in names(lineage_to_folder)) {
    idx <- which(lineage_keys == lineage)
    if (length(idx) == 0) next

    written <- c(
      written,
      write_expression_parquet(
        all_celltypes_df[idx, , drop = FALSE],
        file.path(output_root, lineage_to_folder[[lineage]])
      )
    )
  }

  opc_raw <- readxl::read_xlsx(xlsx_path, sheet = "OPC_BA4_vs_CSC")
  opc_df <- standardise_williams_expression(
    opc_raw,
    default_cell_type = "oligos_opcs_ls",
    default_sample_a = "BA4",
    default_sample_b = "CSC"
  )
  written <- c(
    written,
    write_expression_parquet(
      opc_df,
      file.path(output_root, "07_HCA_oligos_opcs_LS") #"OPC_BA4_vs_CSC"
    )
  )

  message("Wrote Williams expression parquet file(s):")
  for (path in unique(written)) {
    message("  - ", path)
  }

  invisible(unique(written))
}

if (identical(environment(), globalenv()) && !interactive()) {
  args <- commandArgs(trailingOnly = TRUE)
  xlsx_path <- if (length(args) >= 1) args[[1]] else "data/Williams/snrna/03_Seeker_seurat_objects/40478_2023_1568_MOESM4_ESM.xlsx"
  output_root <- if (length(args) >= 2) args[[2]] else "data/Williams/snrna/conversions"
  convert_williams_xlsx_to_parquet(xlsx_path = xlsx_path, output_root = output_root)
}
