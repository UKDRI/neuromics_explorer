# Distribution of log2FC or abundance grouped by cell type or condition
# Original plotly-first renderer reference:
# plotly::plot_ly(
#   df,
#   x = group_vals, y = df[[y_col]], type = "violin",
#   box = list(visible = isTRUE(input$show_box)),
#   meanline = list(visible = TRUE), points = "outliers"
# )

box::use(
  shiny[NS, moduleServer, reactive, req, tagList, selectInput, updateSelectInput,
        fluidRow, column, checkboxInput, validate, need, observe],
  plotly[plotlyOutput, renderPlotly, plot_ly, layout, add_trace],
  dplyr[filter, mutate, case_when],
)

#' @export
violin_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(4, selectInput(ns("y_var"), "Y axis",
                            choices = character(0))),
      column(4, selectInput(ns("group_by"), "Group by",
                            choices = character(0)))
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
    has_values <- function(df, col) {
      col %in% names(df) &&
        any(!is.na(df[[col]]) & nzchar(trimws(as.character(df[[col]]))))
    }

    feature_labels <- function(df) {
      label <- rep("feature", nrow(df))
      for (candidate in c("gene_symbol", "human_gene", "protein_id")) {
        if (!candidate %in% names(df)) next
        values <- as.character(df[[candidate]])
        keep <- !is.na(values) & nzchar(values) & label == "feature"
        label[keep] <- values[keep]
      }
      label
    }

    metric_choices <- reactive({
      df <- de_data()
      if (is.null(df) || nrow(df) == 0) return(character(0))

      available <- character(0)
      labels <- c(
        log2fc = "log2fc",
        abundance_a = "abundance_a",
        abundance_b = "abundance_b",
        pct_expressed_a = "pct_expressed_a",
        pct_expressed_b = "pct_expressed_b",
        expression_metric = "expression_metric"
      )

      for (candidate in names(labels)) {
        if (candidate %in% names(df) && any(!is.na(df[[candidate]]))) {
          available[[labels[[candidate]]]] <- candidate
        }
      }

      available
    })

    build_de_category <- function(df) {
      if (!all(c("padj", "log2fc") %in% names(df))) {
        return(rep("No", nrow(df)))
      }

      direction_ref <- rep("", nrow(df))
      if ("condition_b" %in% names(df)) {
        direction_ref <- as.character(df$condition_b %||% "")
      }
      if ("condition_a" %in% names(df)) {
        empty_idx <- is.na(direction_ref) | !nzchar(direction_ref)
        direction_ref[empty_idx] <- as.character(df$condition_a[empty_idx] %||% "")
      }

      direction_ref[is.na(direction_ref) | !nzchar(direction_ref)] <- "reference"
      up_labels <- paste("Up with", direction_ref)
      down_labels <- paste("Down with", direction_ref)

      dplyr::case_when(
        !is.na(df$padj) & df$padj < padj_thresh() & !is.na(df$log2fc) & df$log2fc > lfc_thresh() ~ up_labels,
        !is.na(df$padj) & df$padj < padj_thresh() & !is.na(df$log2fc) & df$log2fc < -lfc_thresh() ~ down_labels,
        TRUE ~ "No"
      )
    }

    group_choices <- reactive({
      df <- de_data()
      if (is.null(df) || nrow(df) == 0) return(character(0))

      choices <- c()
      if (all(c("padj", "log2fc") %in% names(df)) &&
          any(!is.na(df$padj)) &&
          any(!is.na(df$log2fc))) {
        choices[["DE category"]] <- "de_category"
      }

      labels <- c(
        cluster_id = "Cluster",
        cell_type = "Cell type",
        condition_a = "Condition A",
        condition_b = "Condition B",
        sample_a = "Sample A",
        sample_b = "Sample B",
        tissue = "Tissue",
        sex = "Sex",
        age = "Age",
        cell_id = "Cell ID"
      )

      for (candidate in names(labels)) {
        if (has_values(df, candidate)) {
          choices[[labels[[candidate]]]] <- candidate
        }
      }

      choices
    })

    observe({
      choices <- metric_choices()
      if (length(choices) == 0) return()
      selected <- input$y_var %||% ""
      if (!selected %in% unname(choices)) {
        selected <- if ("log2fc" %in% unname(choices)) "log2fc" else unname(choices)[1]
      }
      updateSelectInput(session, "y_var", choices = choices, selected = selected)
    })

    observe({
      choices <- group_choices()
      if (length(choices) == 0) return()
      selected <- input$group_by %||% ""
      if (!selected %in% unname(choices)) {
        if ("de_category" %in% unname(choices)) {
          selected <- "de_category"
        } else if ("cluster_id" %in% unname(choices)) {
          selected <- "cluster_id"
        } else if ("cell_type" %in% unname(choices)) {
          selected <- "cell_type"
        } else {
          selected <- unname(choices)[1]
        }
      }
      updateSelectInput(session, "group_by", choices = choices, selected = selected)
    })

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

      group_value <- if (identical(grp, "de_category")) {
        build_de_category(df)
      } else if (grp %in% names(df)) {
        as.character(df[[grp]])
      } else {
        rep("all", nrow(df))
      }

      group_value[is.na(group_value) | !nzchar(group_value)] <- "unlabelled"

      plot_df <- df |>
        dplyr::mutate(
          group_value = group_value,
          feature_label = feature_labels(df),
          sig = dplyr::case_when(
            !is.na(padj) & padj < padj_thresh() & log2fc >  lfc_thresh() ~ "Up",
            !is.na(padj) & padj < padj_thresh() & log2fc < -lfc_thresh() ~ "Down",
            TRUE ~ "NS"
          )
        )
      plot_df <- plot_df[!is.na(plot_df[[y_col]]), , drop = FALSE]
      shiny::validate(
        shiny::need(nrow(plot_df) > 0, paste("No", y_col, "values are available for this dataset."))
      )

      group_levels <- unique(plot_df$group_value)
      group_title <- names(group_choices())[match(grp, unname(group_choices()))]
      if (is.na(group_title) || !nzchar(group_title)) group_title <- grp
      p <- plotly::plot_ly()

      for (group_name in group_levels) {
        group_df <- plot_df[plot_df$group_value == group_name, , drop = FALSE]
        p <- p |>
          plotly::add_trace(
            x = rep(group_name, nrow(group_df)),
            y = group_df[[y_col]],
            type = "violin",
            name = group_name,
            box = list(visible = isTRUE(input$show_box)),
            meanline = list(visible = TRUE),
            points = FALSE,
            line = list(color = "#D7DEE5"),
            fillcolor = "rgba(215, 222, 229, 0.45)",
            hovertemplate = paste0(
              "<b>", group_name, "</b><br>",
              y_col, ": %{y:.3f}<extra></extra>"
            ),
            showlegend = FALSE
          )
      }

      point_text <- paste0(
        "<b>", plot_df$group_value, "</b><br>",
        y_col, ": ", signif(plot_df[[y_col]], 3), "<br>",
        "feature: ", plot_df$feature_label, "<br>",
        "padj: ", signif(plot_df$padj, 3), "<br>",
        "class: ", plot_df$sig
      )

      p |>
        plotly::add_trace(
          x = plot_df$group_value,
          y = plot_df[[y_col]],
          type = "scatter",
          mode = "markers",
          color = plot_df$sig,
          colors = c(Up = "#C0392B", Down = "#2980B9", NS = "#BDC3C7"),
          marker = list(size = 6, opacity = 0.65),
          text = point_text,
          hoverinfo = "text",
          showlegend = TRUE
        ) |>
        plotly::layout(
          legend = list(title = list(text = "Significance"), orientation = "h",
                        y = -0.15),
          xaxis    = list(title = group_title, tickangle = -30),
          yaxis    = list(title = y_col),
          showlegend = TRUE
        )
    })
  })
}

`%||%` <- function(a, b) if (!is.null(a)) a else b
