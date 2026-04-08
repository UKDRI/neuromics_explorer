 # ─────────────────────────────────────────────────────────────────────────────
# Dots Plot Module
# ─────────────────────────────────────────────────────────────────────────────
# Dot plot driven by grouped server-side summaries.
# Features:
#   - Dot size = mean expression
#   - Dot color = % cells expressing
#   - Interactive hover with statistics

box::use(
  shiny[moduleServer, NS, reactive, req, tagList, selectInput, sliderInput, validate, need],
  plotly[plotlyOutput, renderPlotly, plot_ly, layout],
  app/logic/api/api_client[fetch_expression_groups],
)

#' @export
dots_ui <- function(id) {
  ns <- NS(id)
  tagList(
    selectInput(ns("metric"), "Value metric", choices = c("log2fc"), selected = "log2fc"),
    sliderInput(ns("top_n"), "Top differentially expressed genes", min = 5, max = 50, value = 20, step = 1),
    plotlyOutput(ns("dots_plot"), height = "520px")
  )
}

#' @param selected_dataset Reactive list describing the active dataset and selected terms.
#' @export
dots_server <- function(id, selected_dataset) {
  moduleServer(id, function(input, output, session) {
    is_single_cell <- reactive({
      ds <- selected_dataset()
      !is.null(ds) && ds$omic_type %in% c("scrna", "snrna")
    })

    plot_data <- reactive({
      ds <- selected_dataset()
      req(ds, is_single_cell())

      fetch_expression_groups(
        lab_source = ds$lab_source,
        study_id = ds$study_id,
        group_by = "cluster_id",
        metric = input$metric,
        top_n = input$top_n
      )
    })

    output$dots_plot <- renderPlotly({
      validate(need(is_single_cell(), "Dot plot is only available for scRNA-seq and snRNA-seq datasets."))
      df <- plot_data()
      validate(need(nrow(df) > 0, "No grouped feature summaries are available for this dataset."))
      df$mean_pct_expressed[is.na(df$mean_pct_expressed)] <- 0
      df$mean_value[is.na(df$mean_value)] <- 0

      plotly::plot_ly(
        df,
        x = ~group_value,
        y = ~feature_label,
        type = "scatter",
        mode = "markers",
        size = ~pmax(mean_pct_expressed, 0) * 0.18 + 6,
        color = ~mean_value,
        colors = c("#2980B9", "#FFFFFF", "#C0392B"),
        marker = list(
          showscale = TRUE,
          colorbar = list(title = "Mean log2FC")
        ),
        text = ~paste0(
          "<b>", feature_label, "</b><br>",
          "group: ", group_value, "<br>",
          "mean log2FC: ", signif(mean_value, 3), "<br>",
          "mean % expressed: ", signif(mean_pct_expressed, 3), "<br>",
          "rows: ", row_count
        ),
        hoverinfo = "text"
      ) |>
        plotly::layout(
          xaxis = list(title = "Group"),
          yaxis = list(title = "Feature", autorange = "reversed")
        )
    })
  })
}

#' @noRd
`%||%` <- function(a, b) if (!is.null(a)) a else b
