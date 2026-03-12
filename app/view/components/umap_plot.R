box::use(
  shiny[NS, moduleServer, reactive, req, renderUI, uiOutput,
        selectInput, div, p, tagList, fluidRow, column],
  plotly[plotlyOutput, renderPlotly, plot_ly, layout],
  dplyr[left_join, mutate],
  app/logic/query_data/reductions[get_reduction, list_reductions],
)

#' @export
ui <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(4, selectInput(ns("reduction"), "Reduction",
                            choices = c("umap", "pca", "tsne"), selected = "umap")),
      column(4, selectInput(ns("colour_by"), "Colour by",
                            choices = c("cell_type", "cluster_id", "condition_a", "sex", "tissue"),
                            selected = "cell_type")),
      column(4, uiOutput(ns("reduction_avail_ui")))
    ),
    plotlyOutput(ns("plot"), height = "540px")
  )
}

#' @export
server <- function(id, selected_dataset) {
  moduleServer(id, function(input, output, session) {

    emb_data <- reactive({
      ds <- selected_dataset()
      req(ds, ds$omic_type %in% c("scrna", "snrna"))
      get_reduction(
        lab       = ds$lab,
        study_id  = ds$study_id,
        organism  = ds$organism %||% "human",
        modality  = ds$omic_type,
        reduction = input$reduction,
        with_metadata = TRUE
      )
    })

    output$plot <- renderPlotly({
      df  <- emb_data()
      req(nrow(df) > 0, "dim_1" %in% names(df), "dim_2" %in% names(df))

      col_var <- input$colour_by
      colour_col <- if (col_var %in% names(df)) df[[col_var]] else NULL

      plotly::plot_ly(
        df,
        x         = ~dim_1,
        y         = ~dim_2,
        type      = "scatter",
        mode      = "markers",
        color     = colour_col,
        text      = ~paste0(
          "Cell: ", cell_id, "<br>",
          col_var, ": ", if (!is.null(colour_col)) colour_col else "—"
        ),
        hoverinfo = "text",
        marker    = list(size = 3, opacity = 0.55)
      ) |>
        plotly::layout(
          xaxis = list(title = paste0(toupper(input$reduction), " 1"),
                       showgrid=FALSE, zeroline=FALSE),
          yaxis = list(title = paste0(toupper(input$reduction), " 2"),
                       showgrid=FALSE, zeroline=FALSE),
          legend = list(title=list(text=col_var))
        )
    })
  })
}

`%||%` <- function(a, b) if (!is.null(a)) a else b