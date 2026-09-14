# Interactive DE results table.
# The CSV export control is rendered by the parent explorer header and datatables.

box::use(
  shiny[NS, moduleServer, reactive, req, tagList],
  DT[DTOutput, renderDT, datatable, formatSignif, formatRound],
)

#' @export
results_ui <- function(id) {
  ns <- NS(id)
  tagList(DTOutput(ns("tbl")))
}

#' @param de_data  reactive data.frame
#' @export
results_server <- function(id, de_data) {
  moduleServer(id, function(input, output, session) {

    output$tbl <- renderDT({
      df <- de_data()
      req(nrow(df) > 0)

      # Drop internal/ redundant columns for display. Order is identity -> grouping -> stats.
      display_cols <- intersect(
        c("dataset_name", "gene_symbol", "human_gene", "protein_id",
          "de_category", "cell_type", "cluster_id",
          "condition_a", "condition_b", "sample_a", "sample_b",
          "log2fc", "pvalue", "padj",
          "abundance_a", "abundance_b", "pct_expressed_a", "pct_expressed_b",
          "organism"),
        names(df)
      )
      df <- df[, display_cols, drop = FALSE]

      # Column availability is per-dataset therefore drop columns that have no value.
      # Ie diaz has abundance_*, williams has pct_expressed_* and cell_type, dion has only cluster_id, etc. 
      keep <- vapply(df, function(x) any(!is.na(x) & nzchar(trimws(as.character(x)))), logical(1))
      df <- df[, keep, drop = FALSE]
      req(ncol(df) > 0)

      # Numeric formatting targets are resolved BEFORE renaming below, then compute them
      # afterwards.
      num_cols  <- names(df)[vapply(df, is.numeric, logical(1))]
      pval_cols <- intersect(c("pvalue", "padj"), num_cols)
      lfc_cols  <- intersect(c("log2fc", "abundance_a", "abundance_b",
                               "pct_expressed_a", "pct_expressed_b"), num_cols)

      display_labels <- c(
        dataset_name = "Dataset", gene_symbol = "Gene", human_gene = "Human gene",
        protein_id = "Protein", de_category = "Contrast / DE category",
        cell_type = "Cell type", cluster_id = "Cluster",
        condition_a = "Condition A", condition_b = "Condition B",
        sample_a = "Sample A", sample_b = "Sample B",
        log2fc = "log2FC", pvalue = "pvalue", padj = "padj",
        abundance_a = "Abundance A", abundance_b = "Abundance B",
        pct_expressed_a = "% expressed A", pct_expressed_b = "% expressed B",
        organism = "Organism"
      )
      relabel <- function(cols) {
        ifelse(cols %in% names(display_labels), unname(display_labels[cols]), cols)
      }
      names(df) <- relabel(names(df))
      pval_cols <- relabel(pval_cols)
      lfc_cols  <- relabel(lfc_cols)

      dt <- datatable(df,
        filter      = "top",
        rownames    = FALSE,
        class       = "table-sm table-condensed table-hover",
        extensions  = c("Scroller", "Buttons"),
        options     = list(
          pageLength = 25,
          scrollX    = TRUE,
          scrollY    = "420px",
          scroller   = TRUE,
          dom        = "Bfrtip",    # controls which DT widgets appear: B=Buttons, f=filter/search, r=processing, t=table, i=info, p=pagination
          buttons    = c('copy', 'csv', 'excel', 'pdf', 'print'),
          #autoWidth = TRUE,
          columnDefs = list(
            list(className = "dt-right", targets = which(sapply(df, is.numeric)) - 1)   # right-align numbers (DT default is left-align)
          )
        )
      )

      # Format numeric columns sensibly
      if (length(pval_cols)) dt <- DT::formatSignif(dt, pval_cols, digits = 3)
      if (length(lfc_cols))  dt <- DT::formatRound(dt, lfc_cols,  digits = 3)
      dt
    })
  })
}
