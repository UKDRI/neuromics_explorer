# Distribution of log2FC or abundance grouped by cell type or condition
# Original plotly-first renderer reference:
# plotly::plot_ly(
#   df,
#   x = group_vals, y = df[[y_col]], type = "violin",
#   box = list(visible = isTRUE(input$show_box)),
#   meanline = list(visible = TRUE), points = "outliers"
# )

box::use(
  shiny[NS, moduleServer, reactive, req, tagList, selectInput, div, fluidRow, column, checkboxInput, validate, need],
  plotly[plotlyOutput, renderPlotly, plot_ly, layout, add_markers],
  dplyr[filter, mutate, case_when],
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

#' @param de_data  reactive data.frame (all expression rows for the active dataset)
#' @param padj_thresh reactive numeric threshold used for colouring
#' @param lfc_thresh reactive numeric threshold used for colouring
#' @export
violin_server <- function(id, de_data, padj_thresh, lfc_thresh) {
  moduleServer(id, function(input, output, session) {

    output$plot <- renderPlotly({
      df <- de_data()
      shiny::validate(
        shiny::need(
          nrow(df) > 0,
          "No expression rows are available for the active dataset."
        )
      )

      y_col  <- input$y_var
      grp    <- input$group_by
      req(y_col %in% names(df))

      plot_df <- df |>
        dplyr::mutate(
          group_value = if (grp %in% names(df)) as.character(df[[grp]]) else "all",
          group_value = ifelse(is.na(group_value) | !nzchar(group_value), "unlabelled", group_value),
          sig = dplyr::case_when(
            !is.na(padj) & padj < padj_thresh() & log2fc >  lfc_thresh() ~ "Up",
            !is.na(padj) & padj < padj_thresh() & log2fc < -lfc_thresh() ~ "Down",
            TRUE ~ "NS"
          )
        )

      plotly::plot_ly(
        plot_df,
        x    = ~group_value,
        y    = stats::as.formula(paste0("~", y_col)),
        type = "violin",
        color = I("#D7DEE5"),
        box  = list(visible = isTRUE(input$show_box)),   # list(visible = input$show_box),
        meanline = list(visible = TRUE),
        points   = FALSE,
        hovertemplate = paste0(
          "<b>%{x}</b><br>", y_col, ": %{y:.3f}<extra></extra>"
        )
      ) |>
        plotly::add_markers(
          data = plot_df,
          x = ~group_value,
          y = stats::as.formula(paste0("~", y_col)),
          color = ~sig,
          colors = c(Up = "#C0392B", Down = "#2980B9", NS = "#BDC3C7"),
          marker = list(size = 6, opacity = 0.65),
          hovertemplate = paste0(
            "<b>%{x}</b><br>",
            y_col, ": %{y:.3f}<br>",
            "gene: %{customdata[0]}<br>",
            "padj: %{customdata[1]}<br>",
            "class: %{customdata[2]}<extra></extra>"
          ),
          customdata = cbind(
            plot_df$gene_symbol %||% "",
            signif(plot_df$padj, 3),
            plot_df$sig
          ),
          inherit = FALSE,
          showlegend = TRUE
        ) |>
        plotly::layout(
          legend = list(title = list(text = "Significance"), orientation = "h",
                        y = -0.15),
          xaxis    = list(title = grp, tickangle = -30),
          yaxis    = list(title = y_col),
          showlegend = TRUE
        )
    })
  })
}

`%||%` <- function(a, b) if (!is.null(a)) a else b
