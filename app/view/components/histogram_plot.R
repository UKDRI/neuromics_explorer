# Server-side histogram plot built from pre-binned API summaries.

box::use(
  shiny[moduleServer, NS, reactive, req, tagList, selectInput, sliderInput, fluidRow, column, uiOutput, renderUI, validate, need],
  plotly[plotlyOutput, renderPlotly, plot_ly, layout, add_bars],
  app/logic/api/api_client[fetch_expression_histogram],
)

#' @export
histogram_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(4, selectInput(ns("metric"), "Metric", choices = c("log2fc", "abundance_a", "abundance_b", "padj"), selected = "log2fc")),
      column(4, selectInput(ns("group_by"), "Group by", choices = c("None" = "", "cell_type", "condition_a", "condition_b"), selected = "")),
      column(4, sliderInput(ns("bins"), "Bins", min = 10, max = 60, value = 30, step = 1))
    ),
    plotlyOutput(ns("histogram"), height = "500px"),
    uiOutput(ns("hist_stats"))
  )
}

#' @param selected_dataset Reactive list describing the active dataset.
#' @export
histogram_server <- function(id, selected_dataset) {
  moduleServer(id, function(input, output, session) {

    plot_data <- reactive({
      ds <- selected_dataset()
      req(ds)

      fetch_expression_histogram(
        lab_source = ds$lab_source,
        study_id = ds$study_id,
        metric = input$metric,
        bins = input$bins,
        group_by = if (nzchar(input$group_by %||% "")) input$group_by else NULL
      )
    })

    output$histogram <- renderPlotly({
      df <- plot_data()
      validate(need(nrow(df) > 0, "No histogram bins are available for this dataset."))

      groups <- unique(df$group_value)
      p <- plotly::plot_ly()
      for (group in groups) {
        group_df <- df[df$group_value == group, , drop = FALSE]
        midpoints <- (group_df$bin_start + group_df$bin_end) / 2
        p <- p |>
          plotly::add_bars(
            x = midpoints,
            y = group_df$row_count,
            name = group,
            hovertemplate = paste0(
              "<b>", group, "</b><br>",
              "bin: %{x:.3f}<br>",
              "rows: %{y}<extra></extra>"
            )
          )
      }

      p |>
        plotly::layout(
          barmode = if (length(groups) > 1) "overlay" else "group",
          xaxis = list(title = input$metric),
          yaxis = list(title = "Row count")
        )
    })

    output$hist_stats <- renderUI({
      df <- plot_data()
      if (nrow(df) == 0) return(NULL)

      total_rows <- sum(df$row_count, na.rm = TRUE)
      paste("Histogram built from", total_rows, "rows after server-side binning.")
    })
  })
}

#' @noRd
`%||%` <- function(a, b) if (!is.null(a)) a else b
