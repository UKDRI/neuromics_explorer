# ─────────────────────────────────────────────────────────────────────────────
# Dots Plot Module
# ─────────────────────────────────────────────────────────────────────────────
# Matrix plot showing genes (rows) × cell_types (cols)
# Similar to scater::plotDots visualization
# Features:
#   - Dot size = mean expression
#   - Dot color = % cells expressing
#   - Interactive hover with statistics
# ─────────────────────────────────────────────────────────────────────────────

box::use(
  shiny[moduleServer, NS, reactive, req, uiOutput, renderUI, observe, div, tags,
         selectInput, checkboxInput],
  bslib[card, card_header, card_body, layout_columns],
  plotly[plotlyOutput, renderPlotly, plot_ly, add_trace, layout],
  dplyr[mutate, select, filter, group_by, summarise, distinct, pull, n_distinct],
)

# ─────────────────────────────────────────────────────────────────────────────
# UI Function
# ─────────────────────────────────────────────────────────────────────────────

#' @export
dots_ui <- function(id) {
  ns <- NS(id)

  div(
    # Controls
    card(
      full_screen = FALSE,
      style = "margin-bottom: 1rem;",
      card_header("Dots Plot Options"),
      card_body(
        layout_columns(
          col_widths = c(6, 6),
          # Gene selector
          div(
            tags$label("Genes to Display:", class = "form-label"),
            selectInput(
              ns("gene_select"),
              label = NULL,
              choices = c("Top 15" = "top15", "Top 30" = "top30", "Custom" = "custom"),
              selected = "top15",
              width = "100%"
            )
          ),
          # Cell type filter
          div(
            tags$label("Cell Types:", class = "form-label"),
            tags$div(
              id = ns("celltype_checkboxes"),
              style = "border: 1px solid #dee2e6; padding: 0.5rem; border-radius: 4px; max-height: 150px; overflow-y: auto;"
            )
          )
        )
      )
    ),

    # Legend note
    tags$div(
      style = "padding: 0.75rem; background-color: #f0f7ff; border-left: 4px solid #0a7aff; border-radius: 4px; margin-bottom: 1rem; font-size: 0.875rem;",
      tags$p(
        tags$strong("Legend: "), "Dot size = Mean expression | Dot color = % cells expressing",
        style = "margin: 0;"
      )
    ),

    # Dots plot
    plotlyOutput(ns("dots_plot"), height = "500px"),

    # Statistics
    uiOutput(ns("dots_stats"))
  )
}

# ─────────────────────────────────────────────────────────────────────────────
# Server Function
# ─────────────────────────────────────────────────────────────────────────────

#' @export
dots_server <- function(id, de_data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Sample data structure:
    # de_data() contains: gene, cell_type, mean_expr, pct_cells_expressing

    # Get unique cell types
    cell_types_available <- reactive({
      req(de_data())
      data <- de_data()
      if (is.null(data) || nrow(data) == 0) {
        return(c())
      }
      unique(sort(data$cell_type))
    })

    # Render cell type checkboxes
    observe({
      cell_types <- cell_types_available()

      if (length(cell_types) == 0) {
        output$celltype_checkboxes <- renderUI({
          tags$p("No cell types available", style = "color: #6c757d; font-size: 0.875rem;")
        })
        return()
      }

      checkboxes <- lapply(cell_types, function(ct) {
        tags$div(
          style = "display: flex; align-items: center;",
          checkboxInput(
            ns(paste0("celltype_", ct)),
            label = ct,
            value = TRUE
          )
        )
      })

      output$celltype_checkboxes <- renderUI({
        do.call(tags$div, checkboxes)
      })
    })

    # Get selected cell types
    selected_cell_types <- reactive({
      cell_types <- cell_types_available()
      selected <- c()
      for (ct in cell_types) {
        if (isTRUE(input[[paste0("celltype_", ct)]])) {
          selected <- c(selected, ct)
        }
      }
      selected
    })

    # Get genes to display
    genes_to_display <- reactive({
      req(de_data())
      data <- de_data() %>%
        filter(cell_type %in% selected_cell_types())

      if (nrow(data) == 0) {
        return(c())
      }

      if (input$gene_select == "top15") {
        top_genes <- data %>%
          group_by(gene) %>%
          summarise(mean_expr = mean(mean_expr, na.rm = TRUE)) %>%
          arrange(desc(mean_expr)) %>%
          pull(gene) %>%
          head(15)
        return(top_genes)
      } else if (input$gene_select == "top30") {
        top_genes <- data %>%
          group_by(gene) %>%
          summarise(mean_expr = mean(mean_expr, na.rm = TRUE)) %>%
          arrange(desc(mean_expr)) %>%
          pull(gene) %>%
          head(30)
        return(top_genes)
      }

      return(unique(data$gene))
    })

    # Prepare plot data
    plot_data <- reactive({
      req(de_data())
      req(length(selected_cell_types()) > 0)
      req(length(genes_to_display()) > 0)

      data <- de_data() %>%
        filter(gene %in% genes_to_display()) %>%
        filter(cell_type %in% selected_cell_types())

      if (nrow(data) == 0) {
        return(NULL)
      }

      # Ensure pct_cells_expressing exists, calculate if needed
      if (!"pct_cells_expressing" %in% names(data)) {
        data <- data %>%
          mutate(pct_cells_expressing = ifelse(mean_expr > 0, 80, 20))
      }

      data
    })

    # Render dots plot
    output$dots_plot <- renderPlotly({
      req(plot_data())

      df <- plot_data()

      # Create heatmap-like visualization with plotly
      # X-axis: cell types
      # Y-axis: genes
      # Size and color: expression metrics

      cell_types <- sort(unique(df$cell_type))
      genes <- sort(unique(df$gene))

      # Reshape to matrix
      plot_df <- expand.grid(gene = genes, cell_type = cell_types)
      plot_df <- merge(plot_df, df, by = c("gene", "cell_type"), all.x = TRUE)
      plot_df <- plot_df %>%
        mutate(
          mean_expr = ifelse(is.na(mean_expr), 0, mean_expr),
          pct_cells_expressing = ifelse(is.na(pct_cells_expressing), 0, pct_cells_expressing)
        )

      p <- plot_ly(
        data = plot_df,
        x = ~cell_type,
        y = ~gene,
        mode = "markers",
        type = "scatter",
        marker = list(
          size = ~(mean_expr * 15 + 4),  # Scale expression to size
          color = ~pct_cells_expressing,  # Color by % expressing
          colorscale = "Viridis",
          showscale = TRUE,
          colorbar = list(
            title = "% Cells<br>Expressing",
            thickness = 15,
            len = 0.7
          ),
          opacity = 0.8,
          line = list(width = 0.5, color = "white")
        ),
        text = ~ paste0(
          "<b>", gene, " - ", cell_type, "</b><br>",
          "Mean Expression: ", round(mean_expr, 3), "<br>",
          "% Expressing: ", round(pct_cells_expressing, 1), "%"
        ),
        hovertemplate = "%{text}<extra></extra>",
        name = ""
      ) %>%
        layout(
          title = "Gene Expression Across Cell Types",
          xaxis = list(
            title = "Cell Type",
            tickangle = -45
          ),
          yaxis = list(
            title = "Gene",
            autorange = "reversed"
          ),
          height = 400 + length(genes) * 15,
          plot_bgcolor = "#f8f9fa",
          paper_bgcolor = "white",
          hovermode = "closest"
        )

      p
    })

    # Summary statistics
    output$dots_stats <- renderUI({
      req(plot_data())

      df <- plot_data()
      n_genes <- n_distinct(df$gene)
      n_cells <- n_distinct(df$cell_type)
      mean_expr <- mean(df$mean_expr, na.rm = TRUE)
      max_pct <- max(df$pct_cells_expressing, na.rm = TRUE)

      tags$div(
        style = "margin-top: 1rem; padding: 0.75rem; background-color: #f0f7ff; border-left: 4px solid #0a7aff; border-radius: 4px;",
        layout_columns(
          col_widths = c(3, 3, 3, 3),
          div(
            tags$p(tags$strong("Genes"), style = "margin: 0; font-size: 0.875rem; color: #6c757d;"),
            tags$p(n_genes, style = "margin: 0; font-size: 1.1rem; font-weight: bold; color: #0a7aff;")
          ),
          div(
            tags$p(tags$strong("Cell Types"), style = "margin: 0; font-size: 0.875rem; color: #6c757d;"),
            tags$p(n_cells, style = "margin: 0; font-size: 1.1rem; font-weight: bold; color: #0a7aff;")
          ),
          div(
            tags$p(tags$strong("Mean Expr"), style = "margin: 0; font-size: 0.875rem; color: #6c757d;"),
            tags$p(round(mean_expr, 2), style = "margin: 0; font-size: 1.1rem; font-weight: bold; color: #0a7aff;")
          ),
          div(
            tags$p(tags$strong("Max %"), style = "margin: 0; font-size: 0.875rem; color: #6c757d;"),
            tags$p(paste0(round(max_pct, 0), "%"), style = "margin: 0; font-size: 1.1rem; font-weight: bold; color: #0a7aff;")
          )
        )
      )
    })
  })
}
