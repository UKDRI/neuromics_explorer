# Interactive DE results table with export button

box::use(
  shiny[NS, moduleServer, reactive, req, tagList, downloadButton, downloadHandler],
  DT[DTOutput, renderDT, datatable, formatSignif, formatRound],
)

#' @export
results_ui <- function(id) {
  ns <- NS(id)
  tagList(
    downloadButton(ns("dl_csv"), "⬇ Download CSV", class = "btn-sm btn-default",
                   style = "margin-bottom:6px"),
    DTOutput(ns("tbl"))
  )
}

#' @param de_data  reactive data.frame
#' @export
results_server <- function(id, de_data) {
  moduleServer(id, function(input, output, session) {

    output$tbl <- renderDT({
      df <- de_data()
      req(nrow(df) > 0)

      # Drop internal/ redundant columns for display
      display_cols <- intersect(
        c("gene_symbol", "human_gene", "protein_id", "cell_type",
          "condition_a", "condition_b", "log2fc", "pvalue", "padj",
          "abundance_a", "abundance_b", "pct_expressed_a", "pct_expressed_b",
          "organism", "study_id"),
        names(df)
      )
      df <- df[, display_cols, drop = FALSE]

      dt <- datatable(df,
        filter      = "top",
        rownames    = FALSE,
        class       = "table-sm table-condensed table-hover",
        extensions  = c("Buttons", "Scroller"),
        options     = list(
          pageLength = 25,
          scrollX    = TRUE,
          scrollY    = "420px",
          scroller   = TRUE,
          dom        = "Bfrtip",    # controls which DT widgets appear: B=Buttons, f=filter/search, r=processing, t=table, i=info, p=pagination
          buttons    = c("csv", "excel"),
          #autoWidth = TRUE,
          columnDefs = list(
            list(className = "dt-right", targets = which(sapply(df, is.numeric)) - 1)   # right-align numbers (DT default is left-align)
          ) # TODO - check
        )
      )

      # Format numeric columns sensibly
      num_cols <- names(df)[sapply(df, is.numeric)]
      pval_cols <- intersect(c("pvalue", "padj"), num_cols)
      lfc_cols  <- intersect(c("log2fc", "abundance_a", "abundance_b",
                               "pct_expressed_a", "pct_expressed_b"), num_cols)

      if (length(pval_cols)) dt <- DT::formatSignif(dt, pval_cols, digits = 3)
      if (length(lfc_cols))  dt <- DT::formatRound(dt, lfc_cols,  digits = 3)
      dt
    })

    output$dl_csv <- downloadHandler(
      filename = function() paste0("de_results_", Sys.Date(), ".csv"),
      content  = function(file) write.csv(de_data(), file, row.names = FALSE)
    )
  })
}
