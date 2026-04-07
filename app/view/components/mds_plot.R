# ─────────────────────────────────────────────────────────────────────────────
# MDS Plot Module
# ─────────────────────────────────────────────────────────────────────────────
# Multidimensional scaling (MDS) plot of samples/cells
# Interactive plotly visualization with zoom, pan, hover
# Features:
#   - Color by cell_type or condition
#   - Interactive hover with metadata
#   - Zoom and pan capabilities
# ─────────────────────────────────────────────────────────────────────────────

box::use(
  shiny[moduleServer, NS, reactive, req, uiOutput, renderUI, observe, div, tags,
         selectInput, updateSelectInput],
  bslib[card, card_header, card_body, layout_columns],
  plotly[plotlyOutput, renderPlotly, plot_ly, add_trace, layout],
  dplyr[mutate, select, filter, group_by, distinct],
  tidyr[pivot_wider],
  stats[dist, cmdscale],
)

# ─────────────────────────────────────────────────────────────────────────────
# UI Function
# ─────────────────────────────────────────────────────────────────────────────

#' @export
mds_ui <- function(id) {
  ns <- NS(id)

  div(
    # Controls
    card(
      full_screen = FALSE,
      style = "margin-bottom: 1rem;",
      card_header("MDS Options"),
      card_body(
        layout_columns(
          col_widths = c(4, 4, 4),
          # Color by selector
          div(
            tags$label("Color by:", class = "form-label"),
            selectInput(
              ns("color_by"),
              label = NULL,
              choices = c("Cell Type" = "cell_type", "Condition" = "condition"),
              selected = "cell_type",
              width = "100%"
            )
          ),
          # MDS dimensions
          div(
            tags$label("X-Axis Dimension:", class = "form-label"),
            selectInput(
              ns("x_dim"),
              label = NULL,
              choices = c("MDS1" = "mds1", "MDS2" = "mds2", "MDS3" = "mds3"),
              selected = "mds1",
              width = "100%"
            )
          ),
          # Y dimension
          div(
            tags$label("Y-Axis Dimension:", class = "form-label"),
            selectInput(
              ns("y_dim"),
              label = NULL,
              choices = c("MDS1" = "mds1", "MDS2" = "mds2", "MDS3" = "mds3"),
              selected = "mds2",
              width = "100%"
            )
          )
        )
      )
    ),

    # MDS plot
    plotlyOutput(ns("mds_plot"), height = "500px"),

    # Statistics
    uiOutput(ns("mds_stats"))
  )
}

# ─────────────────────────────────────────────────────────────────────────────
# Server Function
# ─────────────────────────────────────────────────────────────────────────────

#' @export
mds_server <- function(id, de_data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Sample data structure:
    # de_data() contains: gene, cell_id, cell_type, condition, mean_expr, padj, etc.

    # Prepare MDS data
    mds_data <- reactive({
      req(de_data())

      data <- de_data()
      if (is.null(data) || nrow(data) == 0) {
        return(NULL)
      }

      # Create expression matrix (genes x cells)
      # Pivot to wide format
      expr_matrix <- data %>%
        select(gene, cell_id, mean_expr) %>%
        distinct() %>%
        tidyr::pivot_wider(
          names_from = cell_id,
          values_from = mean_expr,
          values_fill = 0
        ) %>%
        as.data.frame()

      rownames(expr_matrix) <- expr_matrix$gene
      expr_matrix$gene <- NULL

      if (ncol(expr_matrix) < 2) {
        return(NULL)
      }

      # Compute distance matrix (genes)
      dist_matrix <- dist(t(expr_matrix), method = "euclidean")

      # Compute MDS
      mds <- cmdscale(dist_matrix, k = 3)
      colnames(mds) <- c("mds1", "mds2", "mds3")

      # Combine with metadata
      mds_df <- as.data.frame(mds)
      mds_df$cell_id <- rownames(mds_df)

      # Add metadata (cell_type, condition)
      meta <- data %>%
        select(cell_id, cell_type, condition) %>%
        distinct()

      mds_df <- merge(mds_df, meta, by = "cell_id", all.x = TRUE)

      mds_df
    })

    # Render MDS plot
    output$mds_plot <- renderPlotly({
      req(mds_data())

      df <- mds_data()
      color_by <- input$color_by
      x_dim <- input$x_dim
      y_dim <- input$y_dim

      # Color palette
      cols <- c(
        "#1f77b4", "#ff7f0e", "#2ca02c", "#d62728", "#9467bd",
        "#8c564b", "#e377c2", "#7f7f7f", "#bcbd22", "#17becf",
        "#0a7aff", "#007faf", "#ff6b6b", "#28a745", "#ffc107"
      )

      # Get unique categories for color
      categories <- unique(df[[color_by]])
      col_map <- setNames(
        cols[seq_along(categories)],
        categories
      )

      p <- plot_ly(
        data = df,
        x = as.formula(paste0("~", x_dim)),
        y = as.formula(paste0("~", y_dim)),
        color = as.formula(paste0("~", color_by)),
        colors = col_map,
        type = "scatter",
        mode = "markers",
        marker = list(
          size = 8,
          opacity = 0.7,
          line = list(width = 0.5, color = "white")
        ),
        text = ~ paste0(
          "<b>Cell: ", cell_id, "</b><br>",
          "Cell Type: ", cell_type, "<br>",
          "Condition: ", condition, "<br>",
          "MDS1: ", round(mds1, 3), "<br>",
          "MDS2: ", round(mds2, 3), "<br>",
          "MDS3: ", round(mds3, 3)
        ),
        hovertemplate = "%{text}<extra></extra>"
      ) %>%
        layout(
          title = paste0("MDS Plot - Colored by ", color_by),
          xaxis = list(title = paste("MDS Dimension 1 (", x_dim, ")")),
          yaxis = list(title = paste("MDS Dimension 2 (", y_dim, ")")),
          plot_bgcolor = "#f8f9fa",
          paper_bgcolor = "white",
          hovermode = "closest",
          legend = list(
            title = list(text = color_by),
            orientation = "v"
          )
        )

      p
    })

    # Summary statistics
    output$mds_stats <- renderUI({
      req(mds_data())

      df <- mds_data()
      n_cells <- nrow(df)
      n_types <- length(unique(df$cell_type))
      n_cond <- length(unique(df$condition))

      tags$div(
        style = "margin-top: 1rem; padding: 0.75rem; background-color: #f0f7ff; border-left: 4px solid #0a7aff; border-radius: 4px;",
        layout_columns(
          col_widths = c(3, 3, 3, 3),
          div(
            tags$p(tags$strong("Total Cells"), style = "margin: 0; font-size: 0.875rem; color: #6c757d;"),
            tags$p(n_cells, style = "margin: 0; font-size: 1.1rem; font-weight: bold; color: #0a7aff;")
          ),
          div(
            tags$p(tags$strong("Cell Types"), style = "margin: 0; font-size: 0.875rem; color: #6c757d;"),
            tags$p(n_types, style = "margin: 0; font-size: 1.1rem; font-weight: bold; color: #0a7aff;")
          ),
          div(
            tags$p(tags$strong("Conditions"), style = "margin: 0; font-size: 0.875rem; color: #6c757d;"),
            tags$p(n_cond, style = "margin: 0; font-size: 1.1rem; font-weight: bold; color: #0a7aff;")
          ),
          div(
            tags$p(tags$strong("Method"), style = "margin: 0; font-size: 0.875rem; color: #6c757d;"),
            tags$p("Classical", style = "margin: 0; font-size: 1.1rem; font-weight: bold; color: #0a7aff;")
          )
        )
      )
    })
  })
}
