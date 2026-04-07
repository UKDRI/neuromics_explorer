# ─────────────────────────────────────────────────────────────────────────────
# Histogram Plot Module
# ─────────────────────────────────────────────────────────────────────────────
# Plotly histogram showing distribution of expression/log2fc across samples
# Features:
#   - Faceted or stacked histograms by cell_type/condition
#   - Interactive controls for binning
#   - Zoom, pan, export capabilities
# ─────────────────────────────────────────────────────────────────────────────

box::use(
  shiny[moduleServer, NS, reactive, req, uiOutput, renderUI, div, tags,
         selectInput, sliderInput],
  bslib[card, card_header, card_body, layout_columns],
  plotly[plotlyOutput, renderPlotly, plot_ly, add_trace, layout],
  dplyr[mutate, select, filter, arrange, group_by, summarise, n],
)

# ─────────────────────────────────────────────────────────────────────────────
# UI Function
# ─────────────────────────────────────────────────────────────────────────────

#' @export
histogram_ui <- function(id) {
  ns <- NS(id)

  div(
    # Controls
    card(
      full_screen = FALSE,
      style = "margin-bottom: 1rem;",
      card_header("Histogram Options"),
      card_body(
        layout_columns(
          col_widths = c(4, 4, 4),
          # X-axis metric selector
          div(
            tags$label("Metric:", class = "form-label"),
            selectInput(
              ns("metric"),
              label = NULL,
              choices = c("Log2FoldChange" = "log2fc", "Mean Expression" = "mean_expr", "P-Value" = "pval"),
              selected = "log2fc",
              width = "100%"
            )
          ),
          # Facet by
          div(
            tags$label("Facet By:", class = "form-label"),
            selectInput(
              ns("facet_by"),
              label = NULL,
              choices = c("Cell Type" = "cell_type", "Condition" = "condition", "None" = ""),
              selected = "cell_type",
              width = "100%"
            )
          ),
          # Number of bins
          div(
            tags$label("Bins:", class = "form-label"),
            sliderInput(
              ns("nbins"),
              label = NULL,
              min = 10,
              max = 50,
              value = 20,
              step = 1,
              width = "100%"
            )
          )
        )
      )
    ),

    # Histogram
    plotlyOutput(ns("histogram"), height = "500px"),

    # Statistics
    uiOutput(ns("hist_stats"))
  )
}

# ─────────────────────────────────────────────────────────────────────────────
# Server Function
# ─────────────────────────────────────────────────────────────────────────────

#' @export
histogram_server <- function(id, de_data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Prepare histogram data
    plot_data <- reactive({
      req(de_data())

      data <- de_data()
      if (is.null(data) || nrow(data) == 0) {
        return(NULL)
      }

      metric <- input$metric
      facet_by <- input$facet_by

      # Select metric column
      plot_df <- data %>%
        select(gene, cell_type, condition, !!metric) %>%
        rename(value = !!metric) %>%
        filter(!is.na(value))

      # Add facet column if needed
      if (facet_by != "") {
        plot_df <- plot_df %>%
          select(gene, value, !!facet_by) %>%
          rename(facet_group = !!facet_by)
      } else {
        plot_df$facet_group <- "All"
      }

      plot_df
    })

    # Render histogram
    output$histogram <- renderPlotly({
      req(plot_data())

      df <- plot_data()
      facet_by <- input$facet_by

      if (facet_by == "") {
        # Single histogram
        p <- plot_ly(df, x = ~value, nbinsx = input$nbins, type = "histogram") %>%
          add_trace(
            marker = list(color = "#0a7aff", opacity = 0.75),
            name = "Frequency"
          ) %>%
          layout(
            title = paste0("Distribution of ", input$metric),
            xaxis = list(title = input$metric),
            yaxis = list(title = "Frequency"),
            plot_bgcolor = "#f8f9fa",
            paper_bgcolor = "white",
            hovermode = "closest"
          )
      } else {
        # Faceted histograms
        facet_groups <- unique(df$facet_group)
        cols <- c(
          "#0a7aff", "#007faf", "#ff6b6b", "#28a745", "#ffc107",
          "#17a2b8", "#6610f2", "#e83e8c", "#fd7e14"
        )

        p <- plot_ly()
        for (i in seq_along(facet_groups)) {
          group <- facet_groups[i]
          group_data <- df %>% filter(facet_group == group)

          p <- p %>%
            add_trace(
              x = group_data$value,
              type = "histogram",
              nbinsx = input$nbins,
              name = group,
              marker = list(color = cols[(i - 1) %% length(cols) + 1], opacity = 0.7)
            )
        }

        p <- p %>%
          layout(
            title = paste0("Distribution of ", input$metric, " by ", input$facet_by),
            xaxis = list(title = input$metric),
            yaxis = list(title = "Frequency"),
            barmode = "overlay",
            plot_bgcolor = "#f8f9fa",
            paper_bgcolor = "white",
            hovermode = "closest"
          )
      }

      p
    })

    # Summary statistics
    output$hist_stats <- renderUI({
      req(plot_data())

      df <- plot_data()
      mean_val <- mean(df$value, na.rm = TRUE)
      median_val <- median(df$value, na.rm = TRUE)
      sd_val <- sd(df$value, na.rm = TRUE)
      n_val <- nrow(df)

      tags$div(
        style = "margin-top: 1rem; padding: 0.75rem; background-color: #f0f7ff; border-left: 4px solid #0a7aff; border-radius: 4px;",
        layout_columns(
          col_widths = c(3, 3, 3, 3),
          div(
            tags$p(tags$strong("N"), style = "margin: 0; font-size: 0.875rem; color: #6c757d;"),
            tags$p(n_val, style = "margin: 0; font-size: 1.1rem; font-weight: bold; color: #0a7aff;")
          ),
          div(
            tags$p(tags$strong("Mean"), style = "margin: 0; font-size: 0.875rem; color: #6c757d;"),
            tags$p(round(mean_val, 3), style = "margin: 0; font-size: 1.1rem; font-weight: bold; color: #0a7aff;")
          ),
          div(
            tags$p(tags$strong("Median"), style = "margin: 0; font-size: 0.875rem; color: #6c757d;"),
            tags$p(round(median_val, 3), style = "margin: 0; font-size: 1.1rem; font-weight: bold; color: #0a7aff;")
          ),
          div(
            tags$p(tags$strong("SD"), style = "margin: 0; font-size: 0.875rem; color: #6c757d;"),
            tags$p(round(sd_val, 3), style = "margin: 0; font-size: 1.1rem; font-weight: bold; color: #0a7aff;")
          )
        )
      )
    })
  })
}
