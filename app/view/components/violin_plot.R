
# Distribution of log2FC or abundance grouped by cell type or condition

box::use(
  shiny[NS, moduleServer, reactive, req, tagList, selectInput, div, fluidRow, column, checkboxInput],
  plotly[plotlyOutput, renderPlotly, plot_ly, layout, add_boxplot],
  dplyr[filter, mutate],
)

#' @export
violin_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(4, selectInput(ns("y_var"), "Y axis",
                            choices = c("log2fc", "abundance_a", "abundance_b", "pct_expressed_a"),
                            selected = "log2fc")),
      column(4, selectInput(ns("group_by"), "Group by",
                            choices = c("cell_type", "condition_a", "condition_b"),
                            selected = "cell_type"))
    ),
    checkboxInput(ns("show_box"), "Show box plot overlay", value = TRUE),
    plotlyOutput(ns("plot"), height = "480px")
  )
}

#' @param de_data  reactive data.frame (expression rows for selected gene + dataset)
#' @export
violin_server <- function(id, de_data) {
  moduleServer(id, function(input, output, session) {

    output$plot <- renderPlotly({
      df <- de_data()
      req(nrow(df) > 0)

      y_col  <- input$y_var
      grp    <- input$group_by
      req(y_col %in% names(df))

      group_vals <- if (grp %in% names(df)) df[[grp]] else rep("all", nrow(df))

      plotly::plot_ly(
        df,
        x    = group_vals,
        y    = df[[y_col]],
        type = "violin",
        box  = list(visible = input$show_box),
        meanline = list(visible = TRUE),
        points   = "outliers",
        hovertemplate = paste0(
          "<b>%{x}</b><br>", y_col, ": %{y:.3f}<extra></extra>"
        )
      ) |>
        plotly::layout(
          xaxis    = list(title = grp, tickangle = -30),
          yaxis    = list(title = y_col),
          showlegend = FALSE
        )
    })
  })
}
