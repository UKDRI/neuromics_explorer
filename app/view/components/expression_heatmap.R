# Top N DE genes across cell types / conditions

box::use(
  shiny[NS, moduleServer, reactive, req, tagList, div, p],
  plotly[plotlyOutput, renderPlotly, plot_ly, layout],
  dplyr[mutate, filter, select, arrange, desc],
  tidyr[pivot_wider],
  tibble[column_to_rownames],
  app/logic/query_data/expression[get_top_de],
)

#' @export
heatmap_ui <- function(id) {
  ns <- NS(id)
  tagList(
    plotlyOutput(ns("plot"), height = "600px")
  )
}

#' @param selected_dataset  reactive list(lab_source, study_id, ...)
#' @param padj_thresh       reactive numeric
#' @param lfc_thresh        reactive numeric
#' @param n_genes           reactive integer (top N)
#' @param registry_con      reactive DBI connection
#' @param registry_con      reactive DBI connection
#' @export
heatmap_server <- function(id, selected_dataset, registry_con,
                           padj_thresh, lfc_thresh, n_genes) {
  moduleServer(id, function(input, output, session) {

    top_genes <- reactive({
      ds <- selected_dataset()
      req(ds)
      get_top_de(
        registry_con(),
        ds$lab_source,
        ds$study_id,
        n           = n_genes(),
        padj_thresh = padj_thresh(),
        lfc_thresh  = lfc_thresh()
      )
    })

    output$plot <- renderPlotly({
      df <- top_genes()
      req(nrow(df) > 0)

      # Pivot to matrix: genes × cell_types (or conditions if no cell_type)
      group_col <- if ("cell_type" %in% names(df) && any(!is.na(df$cell_type)))
        "cell_type" else "condition_a"

      mat_df <- df |>
        dplyr::select(gene_symbol, !!group_col, log2fc) |>
        dplyr::mutate(group = .data[[group_col]]) |>
        tidyr::pivot_wider(
          id_cols     = gene_symbol,
          names_from  = group,
          values_from = log2fc,
          values_fn   = mean,          # average if multiple rows per gene×group
          values_fill = 0
        ) |>
        tibble::column_to_rownames("gene_symbol")

      mat <- as.matrix(mat_df)

      # Diverging colour scale centred at 0
      plotly::plot_ly(
        z         = mat,
        x         = colnames(mat),
        y         = rownames(mat),
        type      = "heatmap",
        colorscale = list(
          c(0,   "#2980B9"),   # blue  (down)
          c(0.5, "#FFFFFF"),   # white (0)
          c(1,   "#C0392B")    # red   (up)
        ),
        zmid      = 0,
        hovertemplate = "%{y} · %{x}<br>log2FC: %{z:.3f}<extra></extra>"
      ) |>
        plotly::layout(
          xaxis = list(title="", tickangle=-45),
          yaxis = list(title="", automargin=TRUE)
        )
    })
  })
}
