# Server-side histogram plot built from pre-binned API summaries.

box::use(
  shiny[moduleServer, NS, reactive, req, tagList, selectInput, sliderInput, fluidRow, column,
        uiOutput, renderUI, validate, need, observe, updateSelectInput],
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
#' @param source_data reactive data.frame used to infer available metrics/groupings
#' @export
histogram_server <- function(id, selected_dataset, source_data) {
  moduleServer(id, function(input, output, session) {
    has_values <- function(df, col) {
      col %in% names(df) && any(!is.na(df[[col]]))
    }

    has_multiple_values <- function(df, col) {
      if (!col %in% names(df)) return(FALSE)
      values <- as.character(df[[col]])
      values <- values[!is.na(values) & nzchar(trimws(values))]
      length(unique(values)) > 1
    }

    metric_label_map <- c(
      log2fc = "log2fc",
      pvalue = "pvalue",
      padj = "padj",
      abundance_a = "abundance_a",
      abundance_b = "abundance_b",
      pct_expressed_a = "% expressed A",
      pct_expressed_b = "% expressed B"
    )

    group_label_map <- c(
      de_category = "DE category",
      cluster_id = "Cluster",
      cell_type = "Cell type",
      condition_a = "Condition A",
      condition_b = "Condition B",
      sample_a = "Sample A",
      sample_b = "Sample B",
      tissue = "Tissue",
      sex = "Sex",
      age = "Age",
      cell_id = "Cell ID"
    )

    metric_choices <- reactive({
      df <- source_data()
      choices <- c()
      for (candidate in names(metric_label_map)) {
        if (has_values(df, candidate)) {
          choices[[metric_label_map[[candidate]]]] <- candidate
        }
      }
      choices
    })

    group_choices <- reactive({
      df <- source_data()
      choices <- c("None" = "none")
      for (candidate in names(group_label_map)) {
        if (has_multiple_values(df, candidate)) {
          choices[[group_label_map[[candidate]]]] <- candidate
        }
      }
      choices
    })

    observe({
      choices <- metric_choices()
      req(length(choices) > 0)
      selected <- input$metric %||% "log2fc"
      if (!selected %in% unname(choices)) {
        selected <- if ("log2fc" %in% unname(choices)) "log2fc" else unname(choices)[1]
      }
      updateSelectInput(session, "metric", choices = choices, selected = selected)
    })

    observe({
      choices <- group_choices()
      selected <- input$group_by %||% "none"
      if (!selected %in% unname(choices)) {
        selected <- "none"
      }
      updateSelectInput(session, "group_by", choices = choices, selected = selected)
    })

    plot_data <- reactive({
      ds <- selected_dataset()
      req(ds)

      fetch_expression_histogram(
        lab_source = ds$lab_source,
        study_id = ds$study_id,
        metric = input$metric,
        bins = input$bins,
        group_by = if (identical(input$group_by %||% "none", "none")) NULL else input$group_by
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
