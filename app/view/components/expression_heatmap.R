# Top N DE genes across cell types / conditions

box::use(
  shiny[NS, moduleServer, reactive, req, tagList, div, p],
  plotly[plotlyOutput, renderPlotly, plot_ly, layout],
  dplyr[mutate, filter, select, arrange, desc, case_when, slice_head],
  tidyr[pivot_wider],
  tibble[column_to_rownames],
  app/logic/api/api_client[fetch_dataset_expression],
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
#' @export
heatmap_server <- function(id, selected_dataset,
                           padj_thresh, lfc_thresh, n_genes) {
  moduleServer(id, function(input, output, session) {

    top_genes <- reactive({
      ds <- selected_dataset()
      req(ds)
      df <- fetch_dataset_expression(
        ds$lab_source,
        ds$study_id,
        padj_thresh = padj_thresh(),
        lfc_thresh  = lfc_thresh()
      )
      req(nrow(df) > 0)

      ranked <- df |>
        dplyr::mutate(
          sig = dplyr::case_when(
            !is.na(padj) & padj < padj_thresh() & log2fc >  lfc_thresh() ~ "Up",
            !is.na(padj) & padj < padj_thresh() & log2fc < -lfc_thresh() ~ "Down",
            TRUE ~ "NS"
          ),
          rank_bucket = ifelse(sig == "NS", 1L, 0L),
          abs_lfc = abs(log2fc)
        ) |>
        dplyr::arrange(rank_bucket, dplyr::desc(abs_lfc), padj) |>
        dplyr::select(gene_symbol) |>
        unique() |>
        dplyr::slice_head(n = n_genes())

      df[df$gene_symbol %in% ranked$gene_symbol, , drop = FALSE]
    })

    output$plot <- renderPlotly({
      df <- top_genes()
      req(nrow(df) > 0)

      # Pivot to matrix: genes × cell_types (or conditions if no cell_type)
      group_col <- if ("cell_type" %in% names(df) && any(!is.na(df$cell_type)))
        "cell_type" else "condition_a"

      plot_source <- df[, intersect(c("gene_symbol", group_col, "log2fc"), names(df)), drop = FALSE]
      names(plot_source)[names(plot_source) == group_col] <- "group"

      mat_df <- plot_source |>
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
