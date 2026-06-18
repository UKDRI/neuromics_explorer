box::use(
  shiny[NS, moduleServer, reactive, req, observe, tagList, fluidRow, column, selectInput,
        checkboxInput, checkboxGroupInput, uiOutput, renderUI, validate, need, div, tags],
  plotly[plotlyOutput, renderPlotly, plot_ly, layout, add_trace],
  app/logic/api/api_client[fetch_dataset_embeddings],
)

#' @export
umap_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(
        4,
        selectInput(
          ns("reduction"),
          "Embedding",
          choices = c("UMAP" = "umap", "PCA" = "pca", "tSNE" = "tsne"),
          selected = "umap"
        )
      ),
      column(
        4,
        selectInput(
          ns("assay"),
          "Expression assay",
          choices = c("Logcounts" = "logcounts", "Counts" = "counts"),  #, "Expression" = "expression"
          selected = "logcounts"
        )
      ),
      column(
        4,
        checkboxInput(
          ns("overlay_terms"),
          "Overlay checked genes (max 3)",
          value = FALSE
        )
      )
    ),
    uiOutput(ns("term_selector_ui")),
    uiOutput(ns("plots_ui"))
  )
}

#' @export
umap_server <- function(id, selected_dataset) {
  moduleServer(id, function(input, output, session) {

    is_single_cell <- reactive({
      ds <- selected_dataset()
      !is.null(ds) && ds$omic_type %in% c("scrna", "snrna")
    })

    available_gene_terms <- reactive({
      ds <- selected_dataset()
      if (is.null(ds) || !is_single_cell()) return(character(0))
      genes <- unique(trimws(as.character(ds$genes %||% character(0))))
      genes[nzchar(genes)]
    })

    output$term_selector_ui <- renderUI({
      if (!is_single_cell()) {
        return(
          tags$div(
            class = "alert alert-info",
            role = "alert",
            "Embedding plots are only available for scRNA-seq and snRNA-seq datasets."
          )
        )
      }

      terms <- available_gene_terms()
      if (length(terms) == 0) {
        return(
          tags$div(
            class = "alert alert-info",
            role = "alert",
            "Select one or more genes in the search modal to colour the embedding by expression."
          )
        )
      }

      tags$details(
        style = "margin-bottom: 14px;",
        tags$summary(
          style = "cursor: pointer; font-weight: 600; margin-bottom: 8px;",
          "Genes to display on the embedding"
        ),
        checkboxGroupInput(
          session$ns("embedding_terms"),
          label = NULL,
          choices = terms,
          selected = character(0)
        )
      )
    })

    embedding_data <- reactive({
      ds <- selected_dataset()
      req(ds, is_single_cell())

      result <- tryCatch(fetch_dataset_embeddings(
          lab_source = ds$lab_source,
          study_id   = ds$study_id,
          reduction  = input$reduction,
          assay      = input$assay,
          genes      = input$embedding_terms %||% character(0)
        ),
          error = function(e) NULL
        )
      req(!is.null(result))
      result
    })

    checked_terms <- reactive({
      selected <- input$embedding_terms %||% character(0)
      selected[nzchar(selected)]
    })

    overlay_terms <- reactive({
      terms <- checked_terms()
      if (!isTRUE(input$overlay_terms) || length(terms) < 2) return(character(0))
      terms[seq_len(min(3L, length(terms)))]
    })

    base_embedding_df <- reactive({
      df <- embedding_data()
      validate(
        need(nrow(df) > 0, "No embedding rows are available for this dataset.")
      )
      unique(df[, intersect(
        c("obs", "dim_1", "dim_2", "cell_type", "cluster_id", "condition_a", "condition_b", "tissue", "sex", "age", "cell_id"),
        names(df)
      ), drop = FALSE])
    })

    default_colour_column <- reactive({
      df <- base_embedding_df()
      for (candidate in c("cell_type", "cluster_id", "tissue", "condition_a", "sex")) {
        if (candidate %in% names(df) && any(!is.na(df[[candidate]]) & nzchar(as.character(df[[candidate]])))) {
          return(candidate)
        }
      }
      NULL
    })

    output$plots_ui <- renderUI({
      if (!is_single_cell()) {
        return(NULL)
      }

      term_ids <- checked_terms()
      overlay_ids <- overlay_terms()

      panels <- list(
        div(
          style = "border: 1px solid #e3e7eb; border-radius: 8px; padding: 10px; background: white;",
          tags$h5("Embedding overview", style = "margin-top: 0;"),
          plotlyOutput(session$ns("overview_plot"), height = "420px")
        )
      )

      if (length(overlay_ids) > 0) {
        panels[[length(panels) + 1]] <- div(
          style = "border: 1px solid #e3e7eb; border-radius: 8px; padding: 10px; background: white;",
          tags$h5(
            paste("Overlay:", paste(overlay_ids, collapse = ", ")),
            style = "margin-top: 0;"
          ),
          if (length(term_ids) > 3) {
            tags$p(
              "Only the first three checked genes are used in the overlay plot.",
              style = "font-size: 12px; color: #666; margin-bottom: 8px;"
            )
          },
          plotlyOutput(session$ns("overlay_plot"), height = "420px")
        )
      }

      for (i in seq_along(term_ids)) {
        panels[[length(panels) + 1]] <- div(
          style = "border: 1px solid #e3e7eb; border-radius: 8px; padding: 10px; background: white;",
          tags$h5(term_ids[[i]], style = "margin-top: 0;"),
          plotlyOutput(session$ns(paste0("term_plot_", i)), height = "420px")
        )
      }

      tags$div(
        style = "display: grid; grid-template-columns: repeat(auto-fit, minmax(420px, 1fr)); gap: 14px;",
        panels
      )
    })

    output$overview_plot <- renderPlotly({
      base_df <- base_embedding_df()
      colour_col <- default_colour_column()
      hover_text <- paste0(
        "<b>", base_df$obs, "</b>",
        if ("cell_type" %in% names(base_df)) paste0("<br>Cell type: ", base_df$cell_type) else "",
        if ("cluster_id" %in% names(base_df)) paste0("<br>Cluster: ", base_df$cluster_id) else "",
        if ("tissue" %in% names(base_df)) paste0("<br>Tissue: ", base_df$tissue) else ""
      )

      if (is.null(colour_col)) {
        plotly::plot_ly(
          base_df,
          x = ~dim_1,
          y = ~dim_2,
          type = "scatter",
          mode = "markers",
          text = hover_text,
          hoverinfo = "text",
          marker = list(size = 4, opacity = 0.65, color = "#5B6C8F")
        ) |>
          plotly::layout(
            xaxis = list(title = paste0(toupper(input$reduction), " 1"), showgrid = FALSE, zeroline = FALSE),
            yaxis = list(title = paste0(toupper(input$reduction), " 2"), showgrid = FALSE, zeroline = FALSE),
            showlegend = FALSE
          )
      } else {
        plotly::plot_ly(
          base_df,
          x = ~dim_1,
          y = ~dim_2,
          type = "scatter",
          mode = "markers",
          color = as.character(base_df[[colour_col]]),
          text = hover_text,
          hoverinfo = "text",
          marker = list(size = 4, opacity = 0.7)
        ) |>
          plotly::layout(
            xaxis = list(title = paste0(toupper(input$reduction), " 1"), showgrid = FALSE, zeroline = FALSE),
            yaxis = list(title = paste0(toupper(input$reduction), " 2"), showgrid = FALSE, zeroline = FALSE),
            legend = list(title = list(text = colour_col))
          )
      }
    })

    observe({
      df <- embedding_data()
      terms <- checked_terms()

      if (length(terms) == 0) {
        return(invisible(NULL))
      }

      for (i in seq_along(terms)) {
        local({
          idx <- i
          term <- terms[[idx]]
          output_id <- paste0("term_plot_", idx)

          output[[output_id]] <- renderPlotly({
            term_df <- df[df$term == term, , drop = FALSE]
            validate(
              need(nrow(term_df) > 0, paste("No embedding overlay rows are available for", term))
            )
            term_df$expression_scaled <- log10(pmax(term_df$expression_value, 0) + 1)

            hover_text <- paste0(
              "<b>", term_df$obs, "</b><br>",
              term, ": ", signif(term_df$expression_value, 3),
              if ("cell_type" %in% names(term_df)) paste0("<br>Cell type: ", term_df$cell_type) else "",
              if ("cluster_id" %in% names(term_df)) paste0("<br>Cluster: ", term_df$cluster_id) else ""
            )

            plotly::plot_ly(
              term_df,
              x = ~dim_1,
              y = ~dim_2,
              type = "scatter",
              mode = "markers",
              text = hover_text,
              hoverinfo = "text",
              marker = list(
                size = 4,
                opacity = 0.85,
                color = term_df$expression_scaled,
                colorscale = "Viridis",
                showscale = TRUE,
                colorbar = list(title = paste0("log10(", term, " + 1)"))
              )
            ) |>
              plotly::layout(
                xaxis = list(title = paste0(toupper(input$reduction), " 1"), showgrid = FALSE, zeroline = FALSE),
                yaxis = list(title = paste0(toupper(input$reduction), " 2"), showgrid = FALSE, zeroline = FALSE),
                showlegend = FALSE
              )
          })
        })
      }
    })

    observe({
      df <- embedding_data()
      overlay_gene_terms <- overlay_terms()

      if (length(overlay_gene_terms) == 0) {
        return(invisible(NULL))
      }

      output$overlay_plot <- renderPlotly({
        base_df <- base_embedding_df()
        palette <- c("#2E86DE", "#F1C40F", "#C0392B")

        p <- plotly::plot_ly(
          base_df,
          x = ~dim_1,
          y = ~dim_2,
          type = "scatter",
          mode = "markers",
          text = ~obs,
          hoverinfo = "text",
          marker = list(size = 4, opacity = 0.15, color = "#C7CED6"),
          name = "Background"
        )

        for (i in seq_along(overlay_gene_terms)) {
          term <- overlay_gene_terms[[i]]
          term_df <- df[df$term == term, , drop = FALSE]
          if (nrow(term_df) == 0) next

          scaled_alpha <- scale_to_alpha(log10(pmax(term_df$expression_value, 0) + 1))
          term_colours <- rgba_values(palette[[i]], scaled_alpha)

          p <- plotly::add_trace(
            p,
            data = term_df,
            x = ~dim_1,
            y = ~dim_2,
            type = "scatter",
            mode = "markers",
            text = paste0("<b>", term_df$obs, "</b><br>", term, ": ", signif(term_df$expression_value, 3)),
            hoverinfo = "text",
            marker = list(size = 5, color = term_colours),
            name = term,
            inherit = FALSE
          )
        }

        p |>
          plotly::layout(
            xaxis = list(title = paste0(toupper(input$reduction), " 1"), showgrid = FALSE, zeroline = FALSE),
            yaxis = list(title = paste0(toupper(input$reduction), " 2"), showgrid = FALSE, zeroline = FALSE),
            legend = list(title = list(text = "Overlay genes"))
          )
      })
    })
  })
}

scale_to_alpha <- function(values) {
  values <- as.numeric(values)
  values[is.na(values)] <- 0
  max_value <- suppressWarnings(max(values, na.rm = TRUE))
  if (!is.finite(max_value) || max_value <= 0) {
    return(rep(0, length(values)))
  }
  pmax(pmin(values / max_value, 1), 0)
}

rgba_values <- function(hex_colour, alpha_values) {
  rgb_matrix <- grDevices::col2rgb(hex_colour)
  vapply(alpha_values, function(alpha) {
    grDevices::rgb(
      rgb_matrix[1, 1] / 255,
      rgb_matrix[2, 1] / 255,
      rgb_matrix[3, 1] / 255,
      alpha = alpha
    )
  }, character(1))
}

`%||%` <- function(a, b) if (!is.null(a)) a else b
