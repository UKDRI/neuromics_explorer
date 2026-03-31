#!/usr/bin/env Rscript

# RDS -> Parquet mini-workflow for heterogeneous datasets.
# -----------------------------------------------------------------------------
# This script is the "simple files" path in the ingestion toolbox.
# It is intended for labs that deliver multiple independent `.rds` files such as
# `feature_annotations.rds`, `pheno.rds`, `log2fc.rds`, `padj.rds`, etc.
#
# Important constraint:
#   The values are assumed to already be processed by the submitting lab.
#   The script only serialises each object into a parquet-friendly table shape.
#
# Default behaviour:
#   1. Convert each `.rds` data independently.
#   2. Use `parquetize::rds_to_parquet()` for already-tabular objects.
#   3. Preserve matrix-like objects as explicit Parquet tables.
#   4. Let a CSV manifest override filename heuristics when a lab-specific
#      layout does not follow the usual naming patterns.
#   5. Partition for fast querying i.e.
#      `organism=human/modality=transcriptomics/dataset_id=dataset_001`.

#' Guess a logical role for an incoming `.rds` file.
#' @param path Path or filename to inspect.
#' @return A single character role label.
guess_role <- function(path) {
  name <- tolower(tools::file_path_sans_ext(basename(path)))

  if (grepl("annotation|gene_annot|feature_annot|anot", name)) return("feature_annotations")
  if (grepl("pheno|metadata|coldata|sample", name)) return("sample_metadata")
  if (grepl("count|expr|assay|abundance", name)) return("counts")
  if (grepl("log2fc|logfc|dea_log2fc|de_log2fc", name)) return("expression_log2fc")
  if (grepl("padj|fdr|adj", name)) return("expression_padj")
  if (grepl("pval|p_value|pvalue", name)) return("expression_pvalue")
  if (grepl("readme|read_me", name)) return("readme")
  "generic_r_object"
}

#' Map a file role to a registry-friendly logical table name.
#' @param role Role label from the source manifest.
#' @return A single character logical table name.
role_to_logical_table <- function(role) {
  mapping <- c(
    feature_annotations = "feature_annotations",
    sample_metadata = "obs_metadata",
    counts = "counts",
    expression_log2fc = "expression",
    expression_padj = "expression",
    expression_pvalue = "expression",
    readme = "extra_metadata",
    generic_r_object = "extra_metadata"
  )

  mapped_roles <- unname(mapping[role])
  mapped_roles[is.na(mapped_roles)] <- role[is.na(mapped_roles)]
  mapped_roles
}

#' Resolve manifest path for dataset conversion runs.
#'
#' @param output_root Output directory for the conversion run.
#' @param manifest_path Manifest filename or path. Bare filenames are resolved
#'   inside `output_root`.
#'
#' @return A single resolved manifest path.
resolve_manifest_path <- function(output_root, manifest_path = NULL) {
  if (is.null(manifest_path) || !nzchar(manifest_path)) {
    return(NULL)
  } else if (!grepl("^(/|~|[A-Za-z]:[/\\\\])", manifest_path) &&
             !grepl("[/\\\\]", manifest_path)) {
    resolved_path <- file.path(output_root, manifest_path)
  } else {
    resolved_path <- manifest_path
  }

  resolved_path <- path.expand(resolved_path)
  message("[DEBUG] manifest_path: ", resolved_path)
  resolved_path
}

#' Build a default manifest from the `.rds` files in a dataset directory to log conversions.
#'
#' @param dataset_dir Directory containing submitted `.rds` files.
#'
#' @return A data frame describing the default conversion plan.
default_manifest <- function(dataset_dir) {
  files <- list.files(dataset_dir,
    pattern = ".rds",
    full.names = FALSE,
    recursive = TRUE,
    ignore.case = TRUE
  )
  roles <- vapply(files, guess_role, character(1))
  output_names <- basename(tools::file_path_sans_ext(files))

  data.frame(
    file = files,
    role = roles,
    logical_table = vapply(roles, role_to_logical_table, character(1)),
    output_name = output_names,
    actual_table = output_names,
    merge_keys = vapply(roles, role_to_merge_keys, character(1)),
    partition_by = rep("", length(files)),
    original_source_type = rep("rds", length(files)),
    stringsAsFactors = FALSE
  )
}

#' Map roles to keys to allow Parquet files of the same datatype (logical_table: expression,
#' obs_metadata, etc.) to be combined by features and obs (sample, cell, contrasts)
#'
#' @param role Role label from source manifest.
#'
#' @return A comma-separated character string of merge keys, or `""`.
role_to_merge_keys <- function(role) {
  merge_keys <- c(
    expression_log2fc = "feature_id,obs",
    expression_padj = "feature_id,obs",
    expression_pvalue = "feature_id,obs"
  )

  resolved_keys <- unname(merge_keys[role])
  resolved_keys[is.na(resolved_keys)] <- ""
  resolved_keys
}

#' Normalise/standardise manifest column values.
#'
#' @param manifest Manifest data frame read from disk or generated in memory.
#'
#' @return A cleaned manifest data frame with character columns and notification of duplicates.
normalise_manifest <- function(manifest) {
  text_cols <- c("file", "role", "logical_table", "output_name", "actual_table", "merge_keys", "partition_by", "original_source_type")

  for (col in intersect(text_cols, names(manifest))) {
    manifest[[col]] <- as.character(manifest[[col]])
    manifest[[col]][is.na(manifest[[col]])] <- ""
  }

  manifest$output_name <- basename(tools::file_path_sans_ext(manifest$output_name))
  missing_output_name <- !nzchar(manifest$output_name)
  manifest$output_name[missing_output_name] <- basename(tools::file_path_sans_ext(manifest$file[missing_output_name]))

  manifest$actual_table <- basename(manifest$actual_table)
  missing_actual_table <- !nzchar(manifest$actual_table)
  manifest$actual_table[missing_actual_table] <- manifest$output_name[missing_actual_table]

  manifest$partition_by[is.na(manifest$partition_by)] <- ""

  duplicate_output_names <- unique(manifest$output_name[duplicated(manifest$output_name)])
  if (length(duplicate_output_names) > 0) {
    stop(
      "Manifest contains duplicate output_name values after basename normalisation: ",
      paste(duplicate_output_names, collapse = ", "),
      ". Rename the outputs explicitly in the input manifest."
    )
  }

  manifest
}

#' Load a manifest, creating a registry-friendly template when missing.
#'
#' @param dataset_dir Directory containing submitted `.rds` files.
#' @param output_root Output directory for the conversion run.
#' @param manifest_path Manifest filename or path.
#'
#' @return A data frame describing the conversion plan.
load_manifest <- function(dataset_dir, output_root, manifest_path = NULL) {
  resolved_manifest_path <- resolve_manifest_path(output_root, manifest_path)
  manifest_changed <- FALSE
  manifest_from_file <- !is.null(resolved_manifest_path) && file.exists(resolved_manifest_path)

  if (!manifest_from_file) {
    manifest <- default_manifest(dataset_dir)
  } else {
    manifest <- utils::read.csv(resolved_manifest_path, stringsAsFactors = FALSE)
  }

  required_cols <- c("file", "role", "output_name", "partition_by")
  missing_cols <- setdiff(required_cols, names(manifest))
  if (length(missing_cols) > 0) {
    stop("Manifest is missing required column(s): ", paste(missing_cols, collapse = ", "))
  }

  if (!"logical_table" %in% names(manifest)) {
    manifest$logical_table <- vapply(manifest$role, role_to_logical_table, character(1))
    manifest_changed <- TRUE
  }
  if (!"actual_table" %in% names(manifest)) {
    manifest$actual_table <- basename(tools::file_path_sans_ext(manifest$output_name))
    manifest_changed <- TRUE
  }
  if (!"merge_keys" %in% names(manifest)) {
    manifest$merge_keys <- vapply(manifest$role, role_to_merge_keys, character(1))
    manifest_changed <- TRUE
  }
  if ("source_type" %in% names(manifest) && !"original_source_type" %in% names(manifest)) {
    manifest$original_source_type <- manifest$source_type
    manifest$source_type <- NULL
    manifest_changed <- TRUE
  }
  if (!"original_source_type" %in% names(manifest)) {
    manifest$original_source_type <- "rds"
    manifest_changed <- TRUE
  }

  normalised_manifest <- normalise_manifest(manifest)
  if (!identical(manifest, normalised_manifest)) {
    manifest <- normalised_manifest
    manifest_changed <- TRUE
  } else {
    manifest <- normalised_manifest
  }

  if (manifest_changed && manifest_from_file) {
    utils::write.csv(manifest, resolved_manifest_path, row.names = FALSE)
  }

  manifest
}

# Dense matrices cannot be written directly to Parquet, so serialise by reshaping into a long
# columnar-format; i: row index, j: column pointer (= samples, cells, contrasts), x: value (= count)
create_dense_matrix <- function(x, value_name = "value") {
  long_df <- as.data.frame(as.table(x), stringsAsFactors = FALSE)
  names(long_df) <- c("feature_id", "obs", value_name)
  long_df
}

# Sparse matrices are stored in coordinate form so large assays remain practical, size-wise
# long columnar-format; i: row index (= features), j: column pointer (= samples, cells, contrasts), x: value (= count)
create_sparse_matrix <- function(x, col_id = "obs", value_name = "counts") {
  if (!requireNamespace("Matrix", quietly = TRUE)) {
    stop("Package 'Matrix' is required to convert sparse matrix RDS objects.")
  }

  if (is.data.frame(x)) {
    triplets <- Matrix::summary(as(as.matrix(x), "dgCMatrix"))  #or TsparseMatrix
  } else {
    triplets <- Matrix::summary(as(x, "dgCMatrix"))     #Matrix::summary(as(assay(x, "counts"), "dgCMatrix")) - sce obj
  }
  feature_ids <- rownames(x) %||% as.character(seq_len(nrow(x)))
  obs <- colnames(x) %||% as.character(seq_len(ncol(x)))

  df <- data.frame(
    feature_id = feature_ids[triplets$i],
    col_id = obs[triplets$j],
    value_name = triplets$x,
    stringsAsFactors = FALSE
  )

  # Retains information on all identifiers, even those with zero counts
  # df$i <- factor(row.names(triplets)[df$i], levels = row.names(triplets))
  # df$j <- factor(colnames(triplets)[df$j], levels = colnames(triplets))

  df
}

#' Prepend row names as the first column when they carry useful identifiers.
#'
#' @param df Data frame to update.
#' @param id_name Name of the identifier column to create.
#'
#' @return Data frame with a prepended identifier column when applicable.
prepend_rownames_column <- function(df, id_name = "feature_id") {
  row_ids <- rownames(df)
  if (is.null(row_ids) ||
      identical(row_ids, as.character(seq_len(nrow(df)))) ||
      id_name %in% names(df)) {
    return(df)
  }

  data.frame(
    stats::setNames(list(row_ids), id_name),
    df,
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
}

# Named vectors are preserved as a simple key/value table.
# create_named_vector <- function(x, value_name = "value") {
#   data.frame(
#     key = names(x),
#     value = unname(x),
#     stringsAsFactors = FALSE
#   ) |>
#     base::setNames(c("key", value_name))
# }

#' Convert an R object into a Parquet-ready data frame.
#'
#' @param x Object read from an `.rds` file.
#' @param role Logical role inferred or declared for the object.
#' @param source_file Source filename used in error messages.
#'
#' @return A data frame ready for Parquet serialisation.
transform_r_object <- function(x, role, source_file) {
  value_name <- role

  if (is.data.frame(x)) {
    return(x)
  }

  if (inherits(x, "Matrix")) {
    return(create_sparse_matrix(x, value_name = value_name))
  }

  if (is.matrix(x)) {
    return(create_dense_matrix(x, value_name = value_name))
  }

  # if (is.atomic(x) && !is.null(names(x))) {
  #   return(create_named_vector(x, value_name = value_name))
  # }

  if (is.list(x) && length(x) > 0) {
    same_length <- vapply(x, function(item) is.atomic(item) && length(item) == length(x[[1]]), logical(1))
    if (all(same_length)) {
      return(as.data.frame(x, stringsAsFactors = FALSE))
    }
  }

  stop(
    "Unsupported RDS object class for ", source_file, ": ",
    paste(class(x), collapse = ", "),
    ". Add a manifest row and/or a custom conversion for this format"
  )
}

# Write either a single parquet file (write_parquet) or a partitioned parquet dataset (write_dataset) depending
# on whether the object already carries partition-friendly keys.
write_to_parquet <- function(df, output_path, partition_by = character(0), compression = "snappy") {
  if (!requireNamespace("arrow", quietly = TRUE)) {
    stop("Package 'arrow' is required to write parquet outputs.")
  }

  partition_by <- partition_by[nzchar(partition_by)]  # partition_by = c("organism", "modality")
  if (length(partition_by) > 0) {
    if (!all(partition_by %in% names(df))) {
      stop("Partition column(s) not found in df R object: ", paste(setdiff(partition_by, names(df)), collapse = ", "))
    }

    dir_path <- sub("\\.parquet$", "", output_path)
    if (dir.exists(dir_path)) unlink(dir_path, recursive = TRUE)
    arrow::write_dataset(df, path = dir_path, format = "parquet", partitioning = partition_by)  #partitioning = dplyr::group_vars(df)
    return(dir_path)
  }

  arrow::write_parquet(df, sink = output_path, compression = compression)
  output_path
}

# Partition keys for faster querying
# TODO : fix
recommended_partition_cols <- function(df) {
  preferred <- c("organism", "modality", "omic_type", "dataset_id", "study_id")
  present <- preferred[preferred %in% names(df)]

  if (!any(c("dataset_id", "study_id") %in% present)) return(character(0))
  if (!any(c("modality", "omic_type") %in% present)) return(character(0))
  cat("[DEBUG]  partition columns present: ", present)
  present
}

#' Convert a single `.rds` file into a Parquet file
#'
#' @param path_to_file Path to the source `.rds` file.
#' @param output_dir Output directory for the converted asset.
#' @param role Logical role declared for the source file.
#' @param output_name Output asset name without the `.parquet` suffix.
#' @param partition_by Comma-separated partition columns.
#' @param logical_table Registry-friendly logical table name.
#' @param actual_table Registry-friendly asset key for the converted output.
#' @param compression Parquet compression codec passed to Arrow.
#'
#' @return A one-row data frame describing the conversion result.
convert_one_rds <- function(path_to_file, output_dir, role,
                            output_name, partition_by = "",
                            logical_table = role_to_logical_table(role),
                            actual_table = output_name,
                            source_file = path_to_file,
                            merge_keys = role_to_merge_keys(role),
                            compression = "snappy") {
  if (!requireNamespace("parquetize", quietly = TRUE)) {
    stop("Package 'parquetize' is required for direct RDS-to-Parquet conversion.")
  }

  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  output_name <- basename(as.character(output_name))
  actual_table <- basename(as.character(actual_table))
  partition_by <- as.character(partition_by %||% "")
  if (is.na(partition_by) || !nzchar(partition_by)) partition_by <- ""
  output_path <- file.path(output_dir, paste0(output_name, ".parquet"))
  partition_cols <- trimws(strsplit(partition_by, ",", fixed = TRUE)[[1]])
  partition_cols <- partition_cols[nzchar(partition_cols)]

  obj <- readRDS(path_to_file)
  object_class <- paste(class(obj), collapse = ", ")
  used_parquetize <- FALSE

  if (is.data.frame(obj)) {
    id_name <- switch(role,
      counts = "feature_id",
      feature_annotations = "feature_id",
      sample_metadata = "obs",
      NULL
    )
    if (!is.null(id_name)) {
      obj <- prepend_rownames_column(obj, id_name = id_name)
    }
  }

  if (is.data.frame(obj) && length(partition_cols) == 0) {
    partition_cols <- recommended_partition_cols(obj)
  }

  if (is.data.frame(obj) && length(partition_cols) == 0) {
    if (!any(names(obj) %in% c("feature_id", "obs"))) {
      parquetize::rds_to_parquet(
        path_to_file = path_to_file,
        path_to_parquet = output_path,
        partition = "no",
        compression = compression
      )
      used_parquetize <- TRUE
    } else {
      final_path <- write_to_parquet(
        obj,
        output_path = output_path,
        partition_by = partition_cols,
        compression = compression
      )

      return(data.frame(
        source_file = source_file,
        role = role,
        logical_table = logical_table,
        actual_table = actual_table,
        merge_keys = merge_keys,
        partition_by = partition_by,
        original_source_type = "rds",
        source_type = "parquet",
        object_class = object_class,
        output_path = final_path,
        status = "converted_after_transformation",
        stringsAsFactors = FALSE
      ))
    }
  }

  if (used_parquetize) {
    return(data.frame(
      source_file = source_file,
      role = role,
      logical_table = logical_table,
      actual_table = actual_table,
      merge_keys = merge_keys,
      partition_by = partition_by,
      original_source_type = "rds",
      source_type = "parquet",
      object_class = object_class,
      output_path = output_path,
      status = "converted_with_parquetize",
      stringsAsFactors = FALSE
    ))
  }

  transformed_df <- transform_r_object(obj, role = role, source_file = basename(path_to_file))
  final_path <- write_to_parquet(
    transformed_df,
    output_path = output_path,
    partition_by = partition_cols,
    compression = compression
  )

  data.frame(
    source_file = source_file,
    role = role,
    logical_table = logical_table,
    actual_table = actual_table,
    merge_keys = merge_keys,
    partition_by = partition_by,
    original_source_type = "rds",
    source_type = "parquet",
    object_class = object_class,
    output_path = final_path,
    status = "converted_after_transformation",
    stringsAsFactors = FALSE
  )
}

#' Split a comma-separated merge key string into a character vector.
#'
#' @param merge_keys Comma-separated merge key specification.
#'
#' @return Character vector of merge key columns.
parse_merge_keys <- function(merge_keys) {
  merge_keys <- as.character(merge_keys %||% "")
  if (is.na(merge_keys) || !nzchar(merge_keys)) return(character(0))

  keys <- trimws(strsplit(merge_keys, ",", fixed = TRUE)[[1]])
  keys[nzchar(keys)]
}

#' Determine rds file's directory (ie dataset_id, lab_source, name) to group initial Parquet files.
#' For cases where data submitted separately needs to be combined e.g. DEA_log2FC_All + DEA_padj_All
#'
#' @param source_file Relative source file path from the manifest.
#'
#' @return A single character group label.
source_group_dir <- function(source_file) {
  dir_name <- dirname(source_file)
  if (identical(dir_name, ".")) "" else dir_name
}

#' Rename final parquet output to match logical_table, fall back to copy/ delete(unlink) if needed.
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

#' Finalise one logical_table parquet group per dataset by renaming or merging outputs.
#'
#' @param manifest_group Manifest subset for one logical_table group per dataset.
#' @param output_root Root output directory for the conversion run.
#' @param compression Parquet compression codec passed to Arrow.
#'
#' @return Updated manifest subset with final output paths and statuses.
finalise_logical_group <- function(manifest_group, output_root, compression = "snappy") {
  if (!requireNamespace("arrow", quietly = TRUE)) {
    stop("Package 'arrow' is required to finalise parquet outputs.")
  }

  logical_table <- unique(manifest_group$logical_table)
  if (length(logical_table) != 1) {
    stop("logical_table finalisation received multiple logical_table values.")
  }

  final_output_path <- file.path(output_root, paste0(logical_table, ".parquet"))
  group_paths <- manifest_group$output_path

  if (nrow(manifest_group) == 1) {
    manifest_group$output_path <- move_parquet_output(group_paths[[1]], final_output_path)
    manifest_group$status <- paste0(manifest_group$status, "_finalised")
    return(manifest_group)
  }

  parsed_keys <- parse_merge_keys(manifest_group$merge_keys[[1]])
  if (length(parsed_keys) == 0) {
    stop(
      "Multiple parquet outputs map to logical_table '", logical_table,
      "' but no merge_key was supplied. Add a key column such as 'feature_id,obs' for the data to be merged by."
    )
  }

  tables <- lapply(group_paths, function(path) {
    df <- as.data.frame(arrow::read_parquet(path))
    missing_keys <- setdiff(parsed_keys, names(df))
    if (length(missing_keys) > 0) {
      stop(
        "Cannot merge parquet output ", path,
        " because required merge key(s) are missing: ",
        paste(missing_keys, collapse = ", ")
      )
    }
    df
  })

  merged_df <- Reduce(function(left, right) {
    merge(left, right, by = parsed_keys, all = TRUE, sort = FALSE)
  }, tables)

  if (file.exists(final_output_path)) unlink(final_output_path)
  arrow::write_parquet(merged_df, sink = final_output_path, compression = compression)  #TODO optional write_dataset() for partitioning in later release to create dir dataset (not single file), optimise memory for larger datasets
  unlink(group_paths[file.exists(group_paths)])

  manifest_group$output_path <- final_output_path
  manifest_group$status <- paste0("conversion_merged_into_", logical_table)
  manifest_group
}

#' Build the finalised manifest after any rename/merge steps complete.
#'
#' @param manifest_out Conversion records after finalisation.
#'
#' @return A per-source-row data frame with final parquet paths.
build_finalised_manifest <- function(manifest_out) {
  manifest_out[, c(
    "source_file", "role", "logical_table", "actual_table",
    "merge_keys", "partition_by", "source_type", "output_path", "status"
  ), drop = FALSE]
}

#' Finalise logical_table outputs after individual conversions complete.
#' For cases where data submitted separately needs to be combined e.g. DEA_log2FC_All + DEA_padj_All
#'
#' @param manifest_out Conversion manifest produced by the first pass.
#' @param output_root Root output directory for the conversion run.
#' @param compression Parquet compression codec passed to Arrow.
#'
#' @return Updated conversion manifest after final rename/merge.
finalise_logical_tables <- function(manifest_out, output_root, compression = "snappy") {
  eligible <- grepl("^converted", manifest_out$status) & file.exists(manifest_out$output_path)
  if (!any(eligible)) return(manifest_out)

  group_dirs <- vapply(manifest_out$source_file, source_group_dir, character(1))
  group_ids <- paste(group_dirs, manifest_out$logical_table, sep = "||")
  eligible_groups <- unique(group_ids[eligible])
  message("[DEBUG] eligible_groups: ", eligible_groups)
  final_targets <- character(0)

  # Collapse eligible Parquet files using logical_table for successfully converted files
  for (group_id in eligible_groups) {
    group_rows <- which(group_ids == group_id & eligible)
    group_manifest <- manifest_out[group_rows, , drop = FALSE]
    target_name <- unique(group_manifest$logical_table)

    if (length(target_name) != 1) {
      stop("logical_table merging encountered an invalid grouping.")
    }
    if (target_name %in% final_targets) {
      stop(
        "More than one source directory would write the same final parquet name '",
        target_name, ".parquet'. Run the workflow per dataset directory or set explicit merge groups."
      )
    }

    manifest_out[group_rows, ] <- finalise_logical_group(group_manifest, output_root, compression = compression)
    final_targets <- c(final_targets, target_name)
  }

  manifest_out
}

# Convert every `.rds` file inside a directory and emit a manifest of what is done to each source file.
# This manifest is also to build registry entries and semantic views over parquet files.
#' Convert every `.rds` file in a directory into Parquet outputs.
#'
#' @param dataset_dir Directory containing submitted `.rds` files.
#' @param output_root Root directory where dataset-specific Parquet outputs are written.
#' @param manifest_path Manifest filename or path. When missing, a starter
#'   manifest is created in `output_root`. Bare filenames are resolved in
#'   `output_root`.
#' @param finalise_outputs Whether to rename/merge parquet outputs into one file
#'   per logical table after the first conversion pass completes.
#' @param compression Parquet compression codec passed to Arrow.
#'
#' @return A data frame describing each conversion attempt.
convert_dataset_dir <- function(dataset_dir, output_root, manifest_path = NULL,
                                finalise_outputs = TRUE, compression = "snappy") {
  manifest <- load_manifest(dataset_dir, output_root, manifest_path = manifest_path)
  dir.create(output_root, recursive = TRUE, showWarnings = FALSE)

  results <- lapply(seq_len(nrow(manifest)), function(i) {
    row <- manifest[i, , drop = FALSE]
    src <- file.path(dataset_dir, row$file[[1]])
    message("[DEBUG] src ", src)

    if (!file.exists(src)) {
      return(data.frame(
        source_file = row$file[[1]],
        role = row$role[[1]],
        logical_table = row$logical_table[[1]],
        actual_table = row$actual_table[[1]],
        merge_keys = row$merge_keys[[1]],
        partition_by = row$partition_by[[1]],
        original_source_type = row$original_source_type[[1]],
        source_type = "parquet",
        object_class = NA_character_,
        output_path = NA_character_,
        status = "missing_source_file",
        stringsAsFactors = FALSE
      ))
    }

    tryCatch(
      convert_one_rds(
        path_to_file = src,
        output_dir = output_root,
        role = row$role[[1]],
        output_name = row$output_name[[1]],
        partition_by = row$partition_by[[1]],
        logical_table = row$logical_table[[1]],
        actual_table = row$actual_table[[1]],
        source_file = row$file[[1]],
        merge_keys = row$merge_keys[[1]],
        compression = compression
      ),
      error = function(e) {
        data.frame(
          source_file = row$file[[1]],
          role = row$role[[1]],
          logical_table = row$logical_table[[1]],
          actual_table = row$actual_table[[1]],
          merge_keys = row$merge_keys[[1]],
          partition_by = row$partition_by[[1]],
          original_source_type = row$original_source_type[[1]],
          source_type = "parquet",
          object_class = NA_character_,
          output_path = NA_character_,
          status = paste("failed:", conditionMessage(e)),
          stringsAsFactors = FALSE
        )
      }
    )
  })

  manifest_out <- do.call(rbind, results)
  utils::write.csv(manifest_out, file.path(output_root, "intermediate_manifest.csv"), row.names = FALSE) #TODO row names to be id numbers
  if (isTRUE(finalise_outputs)) {
    manifest_out <- finalise_logical_tables(manifest_out, output_root, compression = compression)
  }
  finalised_manifest <- build_finalised_manifest(manifest_out)
  utils::write.csv(finalised_manifest, file.path(output_root, "finalised_manifest.csv"), row.names = FALSE) #TODO row names to be id numbers
  manifest_out
}

#' Parse CLI arguments for the independent RDS workflow.
#'
#' @param args Character vector from `commandArgs(trailingOnly = TRUE)`.
#'
#' @return A named list ready for `do.call(main, ...)`.
parse_cli_args <- function(args = commandArgs(trailingOnly = TRUE)) {
  if (length(args) < 2) {
    stop(
      "Usage: Rscript rds_to_parquet_indep.R <dataset_dir> <output_root> [manifest_path] [compression] [finalise_outputs]"
    )
  }

  list(
    dataset_dir = args[[1]],
    output_root = args[[2]],
    manifest_path = if (length(args) >= 3 && nzchar(args[[3]])) args[[3]] else NULL,
    compression = if (length(args) >= 4 && nzchar(args[[4]])) args[[4]] else "snappy",
    finalise_outputs = if (length(args) >= 5 && nzchar(args[[5]])) {
      tolower(args[[5]]) %in% c("true", "t", "1", "yes", "y")
    } else {
      TRUE
    }
  )
}

#' Run the independent `.rds` to Parquet workflow.
#'
#' @param dataset_dir Directory containing submitted `.rds` files.
#' @param output_root Root directory where dataset-specific Parquet outputs are written.
#' @param manifest_path Manifest filename or path. When missing, a starter
#'   manifest is created in `output_root`. Bare filenames are resolved in
#'   `output_root`.
#' @param finalise_outputs Whether to rename/merge parquet outputs into one file
#'   per logical table after the first pass completes.
#' @param compression Parquet compression codec passed to Arrow.
#'
#' @return A data frame describing the conversion results.
main <- function(dataset_dir, output_root, manifest_path = NULL,
                 finalise_outputs = TRUE, compression = "snappy") {
  convert_dataset_dir(
    dataset_dir = dataset_dir,
    output_root = output_root,
    manifest_path = manifest_path,
    finalise_outputs = finalise_outputs,
    compression = compression
  )
}

`%||%` <- function(a, b) if (!is.null(a)) a else b

if (sys.nframe() == 0) {
  result <- do.call(main, parse_cli_args())
  print(result)
}
