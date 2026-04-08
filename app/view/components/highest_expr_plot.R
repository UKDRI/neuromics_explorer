# Top-feature bar chart driven by grouped API summaries.

box::use(
  shiny[moduleServer, NS, reactive, req, tagList, sliderInput, selectInput, renderUI, uiOutput, validate, need, tags],
  plotly[plotlyOutput, renderPlotly, plot_ly, layout],
  app/logic/api/api_client[fetch_expression_groups],
)

#' @export
highest_expr_ui <- function(id) {
  ns <- NS(id)
  tagList(
    sliderInput(ns("top_n"), "Number of top features", min = 5, max = 50, value = 15, step = 1),
    selectInput(ns("metric"), "Ranking metric", choices = c("abundance_a", "abundance_b", "log2fc"), selected = "abundance_a"),
    plotlyOutput(ns("highest_expr_plot"), height = "500px"),
    uiOutput(ns("top_genes_table"))
  )
}

#' @param selected_dataset Reactive list describing the active dataset and selected terms.
#' @export
highest_expr_server <- function(id, selected_dataset) {
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
        top_n = input$top_n,
        genes = ds$genes %||% character(0),
        proteins = ds$proteins %||% character(0)
      )
    })

    ranked_features <- reactive({
      df <- plot_data()
      req(nrow(df) > 0)

      tbl <- stats::aggregate(mean_value ~ feature_label, data = df, FUN = mean)
      tbl[order(tbl$mean_value, decreasing = TRUE), , drop = FALSE]
    })

    output$highest_expr_plot <- renderPlotly({
      validate(need(is_single_cell(), "Top features is only available for scRNA-seq and snRNA-seq datasets."))
      df <- ranked_features()
      validate(need(nrow(df) > 0, "No ranked features are available for this dataset."))

      plotly::plot_ly(
        df,
        x = ~mean_value,
        y = ~reorder(feature_label, mean_value),
        type = "bar",
        orientation = "h",
        text = ~paste0("<b>", feature_label, "</b><br>mean value: ", signif(mean_value, 3)),
        hoverinfo = "text"
      ) |>
        plotly::layout(
          xaxis = list(title = input$metric),
          yaxis = list(title = "Feature"),
          margin = list(l = 160)
        )
    })

    output$top_genes_table <- renderUI({
      validate(need(is_single_cell(), "Top features is only available for scRNA-seq and snRNA-seq datasets."))
      df <- ranked_features()
      if (nrow(df) == 0) return(NULL)

      df$rank <- seq_len(nrow(df))
      df$mean_value <- signif(df$mean_value, 3)
      df <- df[, c("rank", "feature_label", "mean_value"), drop = FALSE]

      tags$table(
        class = "table table-sm table-striped",
        tags$thead(tags$tr(tags$th("Rank"), tags$th("Feature"), tags$th("Mean value"))),
        tags$tbody(
          lapply(seq_len(nrow(df)), function(i) {
            tags$tr(
              tags$td(df$rank[i]),
              tags$td(df$feature_label[i]),
              tags$td(df$mean_value[i])
            )
          })
        )
      )
    })
  })
}

#' @noRd
`%||%` <- function(a, b) if (!is.null(a)) a else b
