# ─────────────────────────────────────────────────────────────────────────────
# Feature Scatter Plot Module
# ─────────────────────────────────────────────────────────────────────────────
# Plotly scatter plot showing expression/features across samples/cells
# Features:
#   - Dropdown to select features (searched terms)
#   - Checkboxes to toggle individual genes on/off
#   - Interactive hover with cell metadata
# ─────────────────────────────────────────────────────────────────────────────

box::use(
  shiny[moduleServer, NS, reactive, observe, uiOutput, renderUI, req, div, tags,
         selectInput, textInput, checkboxInput],
  shinyjs[runjs],
  bslib[card, card_header, card_body, layout_columns],
  plotly[plotlyOutput, renderPlotly, plot_ly, add_trace, layout],
  dplyr[mutate, select, filter, everything],
)

# ─────────────────────────────────────────────────────────────────────────────
# UI Function
# ─────────────────────────────────────────────────────────────────────────────

#' @export
feature_scatter_ui <- function(id) {
  ns <- NS(id)

  div(
    # Controls: Feature dropdown and gene selector checkboxes
    card(
      full_screen = FALSE,
      style = "margin-bottom: 1rem;",
      card_header(
        "Feature Selection"
      ),
      card_body(
        layout_columns(
          col_widths = c(6, 6),
          # Feature dropdown selector
          div(
            tags$label("Select Features:", class = "form-label"),
            selectInput(
              ns("feature_select"),
              label = NULL,
              choices = c("None" = ""),
              selected = "",
              width = "100%"
            )
          ),
          # Gene/term filter
          div(
            tags$label("Filter Genes:", class = "form-label"),
            textInput(
              ns("gene_filter"),
              label = NULL,
              placeholder = "Type to filter genes...",
              width = "100%"
            )
          )
        ),
        # Gene selection checkboxes
        tags$div(
          id = ns("gene_checkboxes_container"),
          style = "border: 1px solid #dee2e6; padding: 0.75rem; border-radius: 4px; max-height: 200px; overflow-y: auto;",
          tags$p("No genes selected", style = "color: #6c757d; font-size: 0.875rem;")
        )
      )
    ),

    # Scatter plot
    plotlyOutput(ns("scatter_plot"), height = "500px"),

    # Plot info/summary
    uiOutput(ns("scatter_info"))
  )
}

# ─────────────────────────────────────────────────────────────────────────────
# Server Function
# ─────────────────────────────────────────────────────────────────────────────

#' @export
feature_scatter_server <- function(id, de_data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Sample data structure:
    # de_data() contains:
    #   - gene names in rows
    #   - cell_id, cell_type, condition, log2fc, padj, pval, mean_expr
    #   - expression matrix (normalized log2)

    # Reactive: Extract available genes from de_data
    available_genes <- reactive({
      req(de_data())
      data <- de_data()
      if (is.null(data) || nrow(data) == 0) {
        return(c())
      }
      # Assuming de_data has gene column
      unique(sort(data$gene))
    })

    # Reactive: Filtered genes based on text input
    filtered_genes <- reactive({
      genes <- available_genes()
      filter_text <- tolower(input$gene_filter)
      if (filter_text == "") {
        return(genes)
      }
      genes[tolower(genes) %like% filter_text]
    })

    # Reactive: Selected genes from checkboxes
    selected_genes <- reactive({
      genes <- filtered_genes()
      selected <- c()
      for (gene in genes) {
        if (isTRUE(input[[paste0("gene_", gene)]])) {
          selected <- c(selected, gene)
        }
      }
      selected
    })

    # Render dynamic gene checkboxes
    observe({
      genes <- filtered_genes()
      selected <- selected_genes()

      if (length(genes) == 0) {
        output$gene_checkboxes_container <- renderUI({
          tags$p("No genes match filter", style = "color: #6c757d; font-size: 0.875rem;")
        })
        return()
      }

      checkboxes <- lapply(genes, function(gene) {
        tags$div(
          style = "display: flex; align-items: center; padding: 0.25rem 0;",
          checkboxInput(
            ns(paste0("gene_", gene)),
            label = gene,
            value = gene %in% selected
          )
        )
      })

      output$gene_checkboxes_container <- renderUI({
        do.call(tags$div, c(checkboxes, style = "max-height: 200px; overflow-y: auto;"))
      })
    })

    # Scatter plot reactive
    plot_data <- reactive({
      req(de_data())
      req(input$feature_select)
      req(length(selected_genes()) > 0)

      data <- de_data()
      selected <- selected_genes()

      # Filter to selected genes
      plot_df <- data %>%
        filter(gene %in% selected)

      if (nrow(plot_df) == 0) {
        return(NULL)
      }

      plot_df
    })

    # Render scatter plot
    output$scatter_plot <- renderPlotly({
      req(plot_data())

      df <- plot_data()
      selected <- selected_genes()

      if (length(selected) == 0 | nrow(df) == 0) {
        return(
          plot_ly() %>%
            layout(
              title = "Select genes to display",
              xaxis = list(title = "Feature 1"),
              yaxis = list(title = "Feature 2")
            )
        )
      }

      # Create scatter plot with genes as different colors/traces
      p <- plot_ly()

      # Color palette for genes
      cols <- c(
        "#1f77b4", "#ff7f0e", "#2ca02c", "#d62728", "#9467bd",
        "#8c564b", "#e377c2", "#7f7f7f", "#bcbd22", "#17becf"
      )

      for (i in seq_along(selected)) {
        gene <- selected[i]
        gene_data <- df %>% filter(gene == !!gene)

        p <- p %>%
          add_trace(
            data = gene_data,
            x = ~feature_1,  # Adjust column names as needed
            y = ~feature_2,  # Adjust column names as needed
            type = "scatter",
            mode = "markers",
            name = gene,
            marker = list(
              size = 6,
              color = cols[(i - 1) %% length(cols) + 1],
              opacity = 0.7
            ),
            text = ~ paste0(
              "<b>", gene, "</b><br>",
              "Cell ID: ", cell_id, "<br>",
              "Cell Type: ", cell_type, "<br>",
              "Condition: ", condition, "<br>",
              "Expression: ", round(mean_expr, 2)
            ),
            hovertemplate = "%{text}<extra></extra>"
          )
      }

      p <- p %>%
        layout(
          title = paste("Featured Gene Expression:", paste(selected, collapse = ", ")),
          xaxis = list(title = "Feature 1"),
          yaxis = list(title = "Feature 2"),
          hovermode = "closest",
          plot_bgcolor = "#f8f9fa",
          paper_bgcolor = "white"
        )

      p
    })

    # Summary info
    output$scatter_info <- renderUI({
      selected <- selected_genes()
      if (length(selected) == 0) {
        return(NULL)
      }

      tags$div(
        style = "margin-top: 1rem; padding: 0.75rem; background-color: #e7f3ff; border-left: 4px solid #0a7aff; border-radius: 4px;",
        tags$p(
          tags$strong("Selected genes: "),
          paste(selected, collapse = ", "),
          style = "margin: 0; font-size: 0.9rem;"
        )
      )
    })
  })
}
