# ─────────────────────────────────────────────────────────────────────────────
# Highest Expression Plot Module
# ─────────────────────────────────────────────────────────────────────────────
# Bar chart or heatmap of top N genes by mean expression
# Similar to scater::plotHighestExprs visualization
# Features:
#   - Filterable by cell_type and condition
#   - Adjustable number of top genes
#   - Color coded by cell type/condition
# ─────────────────────────────────────────────────────────────────────────────

box::use(
  shiny[moduleServer, NS, reactive, req, uiOutput, renderUI, observe, div, tags,
         sliderInput, selectInput, updateSelectInput],
  bslib[card, card_header, card_body, layout_columns],
  plotly[plotlyOutput, renderPlotly, plot_ly, add_trace, layout],
  dplyr[mutate, select, filter, group_by, summarise, arrange, desc, pull, distinct],
)

# ─────────────────────────────────────────────────────────────────────────────
# UI Function
# ─────────────────────────────────────────────────────────────────────────────

#' @export
highest_expr_ui <- function(id) {
  ns <- NS(id)

  div(
    # Controls
    card(
      full_screen = FALSE,
      style = "margin-bottom: 1rem;",
      card_header("Top Genes Selection"),
      card_body(
        layout_columns(
          col_widths = c(4, 4, 4),
          # Number of top genes
          div(
            tags$label("Number of Top Genes:", class = "form-label"),
            sliderInput(
              ns("n_genes"),
              label = NULL,
              min = 5,
              max = 50,
              value = 15,
              step = 1,
              width = "100%"
            )
          ),
          # Cell type filter
          div(
            tags$label("Filter by Cell Type:", class = "form-label"),
            selectInput(
              ns("filter_celltype"),
              label = NULL,
              choices = c("All" = ""),
              selected = "",
              width = "100%"
            )
          ),
          # Condition filter
          div(
            tags$label("Filter by Condition:", class = "form-label"),
            selectInput(
              ns("filter_condition"),
              label = NULL,
              choices = c("All" = ""),
              selected = "",
              width = "100%"
            )
          )
        )
      )
    ),

    # Bar plot
    plotlyOutput(ns("highest_expr_plot"), height = "500px"),

    # Data table with top genes
    uiOutput(ns("top_genes_table"))
  )
}

# ─────────────────────────────────────────────────────────────────────────────
# Server Function
# ─────────────────────────────────────────────────────────────────────────────

#' @export
highest_expr_server <- function(id, de_data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Get unique cell types and conditions for filters
    filter_options <- reactive({
      req(de_data())
      data <- de_data()

      list(
        cell_types = sort(unique(data$cell_type)),
        conditions = sort(unique(data$condition))
      )
    })

    # Update filter dropdowns
    observe({
      options <- filter_options()

      updateSelectInput(
        session,
        "filter_celltype",
        choices = c("All" = "", options$cell_types)
      )

      updateSelectInput(
        session,
        "filter_condition",
        choices = c("All" = "", options$conditions)
      )
    })

    # Prepare plot data
    plot_data <- reactive({
      req(de_data())

      data <- de_data()

      # Apply filters
      if (input$filter_celltype != "") {
        data <- data %>% filter(cell_type == input$filter_celltype)
      }

      if (input$filter_condition != "") {
        data <- data %>% filter(condition == input$filter_condition)
      }

      if (nrow(data) == 0) {
        return(NULL)
      }

      # Get top N genes by mean expression
      top_genes_df <- data %>%
        group_by(gene) %>%
        summarise(
          mean_expr = mean(mean_expr, na.rm = TRUE),
          n_cells = n(),
          cell_types = paste(unique(cell_type), collapse = ", "),
          .groups = "drop"
        ) %>%
        arrange(desc(mean_expr)) %>%
        head(input$n_genes)

      top_genes_df
    })

    # Render bar plot
    output$highest_expr_plot <- renderPlotly({
      req(plot_data())

      df <- plot_data() %>%
        arrange(mean_expr)  # Sort for horizontal bar plot

      p <- plot_ly(
        data = df,
        x = ~mean_expr,
        y = ~gene,
        type = "bar",
        orientation = "h",
        marker = list(
          color = ~mean_expr,
          colorscale = "Viridis",
          showscale = FALSE
        ),
        text = ~ paste0(
          "<b>", gene, "</b><br>",
          "Mean Expression: ", round(mean_expr, 3), "<br>",
          "N Cells: ", n_cells, "<br>",
          "Cell Types: ", cell_types
        ),
        hovertemplate = "%{text}<extra></extra>",
        name = "Mean Expression"
      ) %>%
        layout(
          title = paste("Top", input$n_genes, "Genes by Expression"),
          xaxis = list(title = "Mean Expression (log2)"),
          yaxis = list(title = "Gene"),
          plot_bgcolor = "#f8f9fa",
          paper_bgcolor = "white",
          hovermode = "closest",
          margin = list(l = 150)
        )

      p
    })

    # Render top genes table
    output$top_genes_table <- renderUI({
      req(plot_data())

      df <- plot_data() %>%
        arrange(desc(mean_expr)) %>%
        select(gene, mean_expr, n_cells, cell_types) %>%
        mutate(
          mean_expr = round(mean_expr, 3),
          rank = seq_len(n())
        ) %>%
        select(rank, gene, mean_expr, n_cells, cell_types)

      # Create simple HTML table
      table_html <- tags$table(
        class = "table table-sm table-striped",
        style = "margin-top: 1rem; font-size: 0.875rem;",
        tags$thead(
          tags$tr(
            tags$th("Rank", style = "width: 10%; text-align: center;"),
            tags$th("Gene", style = "width: 25%;"),
            tags$th("Mean Expr", style = "width: 20%; text-align: right;"),
            tags$th("N Cells", style = "width: 15%; text-align: center;"),
            tags$th("Cell Types", style = "width: 30%;")
          )
        ),
        tags$tbody(
          apply(df, 1, function(row) {
            tags$tr(
              tags$td(row[1], style = "text-align: center;"),
              tags$td(tags$strong(row[2])),
              tags$td(row[3], style = "text-align: right;"),
              tags$td(row[4], style = "text-align: center;"),
              tags$td(row[5], style = "font-size: 0.8rem;")
            )
          })
        )
      )

      tags$div(
        style = "margin-top: 1.5rem; padding: 1rem; background-color: #f8f9fa; border-radius: 4px;",
        tags$h5("Top Genes Summary",
                style = "margin-top: 0; margin-bottom: 1rem; font-weight: bold;"),
        table_html
      )
    })
  })
}
