#!/usr/bin/env Rscript

# Container-aware RDS -> Parquet workflow.
# -----------------------------------------------------------------------------
# This script is the "complex object" companion to `rds_to_parquet_indep.R`.
# It is designed for groups that submit a single `.rds` containing richer Bioconductor
# containers such as:
#   - SingleCellExperiment
#   - SummarizedExperiment
#   - RangedSummarizedExperiment
#   - nested named lists of assays or metadata objects
#
# This script inspects one entire object, preserves its internal structure, and writes
# its components into Parquet so it can be registered and parsed in DuckDB.


# dir.create <- base::dir.create

#' Map a container component name to a registry-friendly logical table.
#'
#' @param component_name Component label such as an assay name or metadata slot.
#'
#' @return A single logical table label.
component_to_logical_table <- function(component_name) {
  name <- tolower(component_name)

  if (grepl("^assay:(counts|logcounts|normcounts|normalised|expr|expression)", name)) return("counts")
  if (grepl("^assay:(log2fc|logfc|dea|de_|diff|padj|pval|pvalue)", name)) return("expression")
  if (grepl("^row_data$", name)) return("feature_annotations")
  if (grepl("^col_data$", name)) return("obs_metadata")
  if (grepl("^reduceddimnames:", name)) return("reduced_dims")
  if (grepl("metadata", name)) return("extra_metadata")
  "extra_metadata"
}

#' Derive a dataset-local asset key from an output path.
#'
#' @param path Absolute or relative output path.
#' @param root Output directory for the dataset conversion.
#'
#' @return A relative asset key without the `.parquet` suffix.
path_to_actual_table <- function(path, root) {
  path_norm <- normalizePath(path, winslash = "/", mustWork = FALSE)
  root_norm <- normalizePath(root, winslash = "/", mustWork = FALSE)
  prefix <- paste0(root_norm, "/")
  relative_path <- if (startsWith(path_norm, prefix)) {
    substring(path_norm, nchar(prefix) + 1L)
  } else {
    basename(path_norm)
  }

  tools::file_path_sans_ext(relative_path)
}

#' Build one manifest row for a converted container component.
#'
#' @param component Component label within the container layout.
#' @param component_class Class string recorded for the source component.
#' @param output_path Path to the converted parquet output.
#' @param output_dir Dataset-level output directory used to derive `actual_table`.
#' @param logical_table Optional override for the logical table label.
#' @param exp_name Optional experiment name recorded from a `SingleCellExperiment`.
#'
#' @return A one-row data frame describing the converted component.
build_manifest_row <- function(component, component_class, output_path, output_dir,
                               logical_table = component_to_logical_table(component),
                               exp_name = NA_character_) {
  data.frame(
    component = component,
    logical_table = logical_table,
    actual_table = path_to_actual_table(output_path, output_dir),
    class = component_class,
    exp_name = exp_name,
    output_path = output_path,
    source_type = "parquet",
    stringsAsFactors = FALSE
  )
}

# Small helper so parquet tables always carry an explicit identifier column when
# row names exist in the source object.
#' Prepend an identifier column to a data frame while preserving values.
#'
#' @param df Data frame to augment.
#' @param ids Optional vector of identifiers.
#' @param id_name Name of the identifier column to create.
#'
#' @return A data frame with the identifier column prepended.
prepend_rownames_column <- function(df, ids, id_name) {
  if (is.null(ids)) ids <- as.character(seq_len(nrow(df)))
  cbind(stats::setNames(data.frame(ids, stringsAsFactors = FALSE), id_name), df)
}

#' Rename or move a parquet output into its final canonical filename.
#'
#' @param from Existing parquet path.
#' @param to Final parquet path.
#'
#' @return Final parquet path.
move_parquet_output <- function(from, to) {
  if (normalizePath(from, winslash = "/", mustWork = FALSE) ==
      normalizePath(to, winslash = "/", mustWork = FALSE)) {
    return(to)
  }

  if (file.exists(to)) unlink(to)
  moved <- suppressWarnings(file.rename(from, to))
  if (!isTRUE(moved)) {
    copied <- file.copy(from, to, overwrite = TRUE)
    if (!isTRUE(copied)) {
      stop("Failed to move parquet output from ", from, " to ", to)
    }
    unlink(from)
  }

  to
}

# Write a plain data.frame to parquet without changing the submitted values.
#' Write a data frame to Parquet.
#'
#' @param df Data frame to serialise.
#' @param path Output Parquet path.
#' @param compression Parquet compression codec passed to Arrow.
#'
#' @return The output path.
write_df_parquet <- function(df, path, compression = "snappy") {
  if (!requireNamespace("arrow", quietly = TRUE)) {
    stop("Package 'arrow' is required to write parquet outputs.")
  }
  arrow::write_parquet(df, sink = path, compression = compression)
  path
}

# Dense assay matrices are preserved in wide form so each submitted cell/sample
# column remains explicit in parquet. This keeps the original value layout easy
# to reason about while still making it queryable.
#' Write a dense matrix assay to Parquet.
#'
#' @param x Matrix-like object to serialise.
#' @param path Output Parquet path.
#' @param row_id_name Name of the row identifier column.
#' @param compression Parquet compression codec passed to Arrow.
#'
#' @return The output path.
write_dense_matrix_parquet <- function(x, path, row_id_name = "feature_id", compression = "snappy") {
  df <- as.data.frame(x, stringsAsFactors = FALSE)
  df <- prepend_rownames_column(df, rownames(x), row_id_name)
  write_df_parquet(df, path, compression = compression)
}


#' Write a sparse matrix assay to Parquet in triplet form to reduce memory and parquet size.
#'
#' @param x Sparse matrix-like object to serialise.
#' @param path Output Parquet path.
#' @param row_id_name Name of the row identifier column.
#' @param col_id_name Name of the column identifier column.
#' @param value_name Name of the value column written to parquet.
#' @param compression Parquet compression codec passed to Arrow.
#'
#' @return The output path.
write_sparse_matrix_parquet <- function(x, path, row_id_name = "feature_id",
                                        col_id_name = "obs",
                                        value_name = "counts",
                                        compression = "snappy") {
  if (!requireNamespace("Matrix", quietly = TRUE)) {
    stop("Package 'Matrix' is required to serialise sparse assay matrices.")
  }

  triplets <- Matrix::summary(x)  #Matrix::summary(as(assay(x, "counts"), "dgCMatrix"))  - sce obj  # or TsparseMatrix
  row_ids <- rownames(x) %||% as.character(seq_len(nrow(x)))  #feature_ids
  obs_ids <- colnames(x) %||% as.character(seq_len(ncol(x)))  #samples, cells or contrasts

  df <- data.frame(
    feature_id = row_ids[triplets$i],
    obs = obs_ids[triplets$j],
    counts = triplets$x,
    stringsAsFactors = FALSE
  )
  names(df)[1] <- row_id_name
  names(df)[2] <- col_id_name
  names(df)[3] <- value_name
  write_df_parquet(df, path, compression = compression)
}

# Generic assay serialiser used by SummarizedExperiment-like containers.
# Assay names are preserved in output filenames, while storage strategy depends
# on the in-memory representation of that assay.
#' Write one assay component from a container object to Parquet.
#'
#' @param x Assay object to serialise.
#' @param output_dir Directory for assay outputs.
#' @param assay_name Assay name used in the output filename.
#' @param compression Parquet compression codec passed to Arrow.
#'
#' @return The output path.
write_assay_component <- function(x, output_dir, assay_name, compression = "snappy") {
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  out <- file.path(output_dir, paste0(assay_name, ".parquet"))

  if (inherits(x, "Matrix")) {
    return(write_sparse_matrix_parquet(x, out, compression = compression))
  }

  if (is.matrix(x)) {
    return(write_dense_matrix_parquet(x, out, compression = compression))
  }

  if (is.data.frame(x)) {
    return(write_df_parquet(x, out, compression = compression))
  }

  stop("Unsupported assay object for '", assay_name, "': ", paste(class(x), collapse = ", "))
}

# Row-level annotations are preserved exactly as declared by the lab. For ranged
# containers we prefer genomic coordinates when available because they are often
# more useful than a plain rowData frame downstream.
#' Write row-level annotations from a container object to Parquet.
#'
#' @param x Row metadata or genomic range object to serialise.
#' @param output_dir Directory for dataset outputs.
#' @param compression Parquet compression codec passed to Arrow.
#'
#' @return The output path.
write_row_component <- function(x, output_dir, compression = "snappy") {
  out <- file.path(output_dir, "feature_annotations.parquet")

  if (methods::is(x, "GenomicRanges")) {
    df <- as.data.frame(x)
    return(write_df_parquet(df, out, compression = compression))
  }

  if (is.data.frame(x) || methods::is(x, "SingleCellExperiment")) {
    df <- prepend_rownames_column(rowData(x), rownames(x), "feature_id")
    return(write_df_parquet(df, out, compression = compression))
  }

  df <- as.data.frame(x)
  df <- prepend_rownames_column(df, rownames(df), "feature_id")
  write_df_parquet(df, out, compression = compression)
}

# Column-level annotations become the parquet table that DuckDB can later use as
# metadata for filters such as condition, sample, tissue, or cell type.
#' Write column-level annotations from a container object to Parquet.
#'
#' @param x Column metadata object to serialise.
#' @param output_dir Directory for dataset outputs.
#' @param compression Parquet compression codec passed to Arrow.
#'
#' @return The output path.
write_col_component <- function(x, output_dir, compression = "snappy") {
  out <- file.path(output_dir, "obs_metadata.parquet")
  df <- as.data.frame(x)
  df <- prepend_rownames_column(df, rownames(df), "obs")
  write_df_parquet(df, out, compression = compression)
}

# SingleCellExperiment objects often carry reduced dimensions that are useful
# for later plotting or QA. We preserve them when present rather than dropping
# them on the floor during conversion.
#' Write reduced-dimension matrices from a `SingleCellExperiment`.
#'
#' @param x SingleCellExperiment object.
#' @param output_dir Directory for dataset outputs.
#' @param compression Parquet compression codec passed to Arrow.
#'
#' @return Named character vector of output paths.
write_reduced_dims <- function(x, output_dir, compression = "snappy") {
  if (!requireNamespace("SingleCellExperiment", quietly = TRUE)) {
    return(character(0))
  }

  reduced_names <- SingleCellExperiment::reducedDimNames(x)
  if (length(reduced_names) == 0) return(character(0))

  out_dir <- file.path(output_dir)  #, "reduced_dims"
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

  vapply(reduced_names, function(name) {
    mat <- SingleCellExperiment::reducedDim(x, name)
    out <- file.path(out_dir, paste0(name, ".parquet"))
    write_dense_matrix_parquet(mat, out, row_id_name = "obs", compression = compression)
  }, character(1))
}

#' Canonicalise unique logical-table filenames after conversion.
#'
#' When exactly one output represents a logical table, rename it to
#' `<logical_table>.parquet` so container and independent workflows are easier
#' to reason about together. Ambiguous groups, such as both `counts` and
#' `logcounts`, are left in place.
#'
#' @param manifest Conversion manifest built during this run.
#' @param output_dir Dataset output directory.
#'
#' @return Updated manifest with any moved paths recorded.
finalise_container_manifest <- function(manifest, output_dir) {
  if (nrow(manifest) == 0) return(manifest)

  canonical_tables <- c("counts", "expression", "feature_annotations", "obs_metadata")

  for (logical_table in canonical_tables) {
    idx <- which(manifest$logical_table == logical_table)
    if (length(idx) != 1) next

    target_path <- file.path(output_dir, paste0(logical_table, ".parquet"))
    manifest$output_path[idx] <- move_parquet_output(manifest$output_path[idx], target_path)
    manifest$actual_table[idx] <- path_to_actual_table(target_path, output_dir)
  }

  manifest
}

# This is the main class-driven dispatch. It is intentionally explicit so the
# resulting parquet layout stays understandable to both R and Python code later.
#' Convert a container-style `.rds` object into Parquet assets.
#'
#' @param input_rds Path to the input `.rds` file.
#' @param output_dir Directory for the converted dataset assets.
#' @param compression Parquet compression codec passed to Arrow.
#'
#' @return A data frame describing the converted components.
convert_container_rds <- function(input_rds, output_dir, compression = "snappy") {
  obj <- readRDS(input_rds)
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

  manifest <- list()
  exp_name <- if (inherits(obj, "SingleCellExperiment")) {
    tryCatch(SingleCellExperiment::mainExpName(obj), error = function(e) NA_character_)
  } else {
    NA_character_
  }

  if (inherits(obj, "SingleCellExperiment") || inherits(obj, "SummarizedExperiment")) {
    if (!requireNamespace("SummarizedExperiment", quietly = TRUE)) {
      stop("Package 'SummarizedExperiment' is required for SE/SCE conversion.")
    }

    assays_dir <- file.path(output_dir) #, "assays"
    assay_names <- SummarizedExperiment::assayNames(obj)
    if (length(assay_names) == 0) assay_names <- "assay"

    assay_outputs <- vapply(assay_names, function(name) {
      write_assay_component(
        SummarizedExperiment::assay(obj, name),
        assays_dir,
        assay_name = name,
        compression = compression
      )
    }, character(1))

    row_source <- if (inherits(obj, "RangedSummarizedExperiment") &&
      requireNamespace("SummarizedExperiment", quietly = TRUE)) {
      tryCatch(SummarizedExperiment::rowRanges(obj), error = function(e) SummarizedExperiment::rowData(obj))
    } else {
      SummarizedExperiment::rowData(obj)
    }

    row_out <- write_row_component(row_source, output_dir, compression = compression)
    col_out <- write_col_component(SummarizedExperiment::colData(obj), output_dir, compression = compression)
    reduced_out <- if (inherits(obj, "SingleCellExperiment")) {
      write_reduced_dims(obj, output_dir, compression = compression)
    } else {
      character(0)
    }

    reduced_names <- names(reduced_out) %||% character(0)

    manifest <- do.call(rbind, c(
      lapply(seq_along(assay_names), function(i) {
        build_manifest_row(
          component = paste0("assay:", assay_names[[i]]),
          component_class = class(SummarizedExperiment::assay(obj, assay_names[[i]]))[1],
          output_path = assay_outputs[[i]],
          output_dir = output_dir,
          exp_name = exp_name
        )
      }),
      list(
        build_manifest_row("row_data", "row_component", row_out, output_dir, exp_name = exp_name),
        build_manifest_row("col_data", "col_component", col_out, output_dir, exp_name = exp_name)
      ),
      lapply(seq_along(reduced_out), function(i) {
        build_manifest_row(
          component = paste0("reduced_dim:", reduced_names[[i]]),
          component_class = "reduced_dim",
          output_path = reduced_out[[i]],
          output_dir = output_dir,
          exp_name = exp_name
        )
      })
    ))
  } else if (is.list(obj) && !is.null(names(obj))) {
    # Named list inputs are common when a lab bundles multiple already-processed
    # assets into one `.rds`. We recurse shallowly and write each supported part.
    entries <- lapply(names(obj), function(name) {
      value <- obj[[name]]
      target <- file.path(output_dir, paste0(name, ".parquet"))

      if (is.data.frame(value)) {
        write_df_parquet(value, target, compression = compression)
      } else if (inherits(value, "Matrix")) {
        write_sparse_matrix_parquet(value, target, compression = compression)
      } else if (is.matrix(value)) {
        write_dense_matrix_parquet(value, target, compression = compression)
      } else if (is.atomic(value)) {
        write_df_parquet(data.frame(value = value, stringsAsFactors = FALSE), target, compression = compression)
      } else {
        stop("Unsupported named-list element '", name, "' of class ", paste(class(value), collapse = ", "))
      }

      build_manifest_row(
        component = name,
        component_class = paste(class(value), collapse = ", "),
        output_path = target,
        output_dir = output_dir,
        exp_name = exp_name
      )
    })

    manifest <- do.call(rbind, entries)
  } else if (is.data.frame(obj)) {
    table_out <- write_df_parquet(obj, file.path(output_dir, "table.parquet"), compression = compression)
    manifest <- build_manifest_row(
      component = "table",
      component_class = paste(class(obj), collapse = ", "),
      output_path = table_out,
      output_dir = output_dir,
      logical_table = "expression",
      exp_name = exp_name
    )
  } else if (inherits(obj, "Matrix")) {
    matrix_out <- write_sparse_matrix_parquet(obj, file.path(output_dir, "matrix.parquet"), compression = compression)
    manifest <- build_manifest_row(
      component = "matrix",
      component_class = paste(class(obj), collapse = ", "),
      output_path = matrix_out,
      output_dir = output_dir,
      logical_table = "counts",
      exp_name = exp_name
    )
  } else if (is.matrix(obj)) {
    matrix_out <- write_dense_matrix_parquet(obj, file.path(output_dir, "matrix.parquet"), compression = compression)
    manifest <- build_manifest_row(
      component = "matrix",
      component_class = paste(class(obj), collapse = ", "),
      output_path = matrix_out,
      output_dir = output_dir,
      logical_table = "counts",
      exp_name = exp_name
    )
  } else {
    stop("Unsupported top-level RDS object class: ", paste(class(obj), collapse = ", "))
  }

  manifest <- finalise_container_manifest(manifest, output_dir)
  utils::write.csv(manifest, file.path(output_dir, "conversion_manifest.csv"), row.names = FALSE)
  manifest
}

#' Parse CLI arguments for the container RDS workflow.
#'
#' @param args Character vector from `commandArgs(trailingOnly = TRUE)`.
#'
#' @return A named list ready for `do.call(main, ...)`.
parse_cli_args <- function(args = commandArgs(trailingOnly = TRUE)) {
  if (length(args) < 2) {
    stop("Usage: Rscript misc/rds_to_parquet_containers.R <input.rds> <output_dir> [compression]")
  }

  list(
    input_rds = args[[1]],
    output_dir = args[[2]],
    compression = if (length(args) >= 3 && nzchar(args[[3]])) args[[3]] else "snappy"
  )
}

#' Run the container `.rds` to Parquet workflow.
#'
#' @param input_rds Path to the input `.rds` file.
#' @param output_dir Directory for the converted dataset assets.
#' @param compression Parquet compression codec passed to Arrow.
#'
#' @return A data frame describing the converted components.
main <- function(input_rds, output_dir, compression = "snappy") {
  convert_container_rds(input_rds, output_dir, compression = compression)
}

`%||%` <- function(a, b) if (is.null(a)) b else a

if (sys.nframe() == 0) {
  result <- do.call(main, parse_cli_args())
  print(result)
}
