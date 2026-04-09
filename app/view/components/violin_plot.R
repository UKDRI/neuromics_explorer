# Distribution of dataset-level DE metrics or per-cell gene expression values.

box::use(
  shiny[NS, moduleServer, reactive, req, tagList, selectInput, updateSelectInput,
        fluidRow, column, checkboxInput, validate, need, observe, uiOutput, renderUI],
  plotly[plotlyOutput, renderPlotly, plot_ly, layout, add_trace],
  dplyr[mutate, case_when],
  app/logic/api/api_client[fetch_expression_feature_values],
)

#' @export
violin_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(4, selectInput(ns("feature_term"), "View", choices = character(0))),
      column(4, selectInput(ns("y_var"), "Y axis", choices = character(0))),
      column(4, selectInput(ns("x_axis"), "X-axis", choices = character(0)))
    ),
    uiOutput(ns("group_by_ui")),
    checkboxInput(ns("show_box"), "Show box plot overlay", value = TRUE),
    plotlyOutput(ns("plot"), height = "480px")
  )
}

#' @param de_data reactive data.frame for dataset-wide DE/expression rows
#' @param selected_dataset reactive list carrying active dataset and modal-selected genes
#' @param padj_thresh reactive numeric threshold used for significance colouring
#' @param lfc_thresh reactive numeric threshold used for significance colouring
#' @export
violin_server <- function(id, de_data, selected_dataset, padj_thresh, lfc_thresh) {
  moduleServer(id, function(input, output, session) {
    has_values <- function(df, col) {
      col %in% names(df) &&
        any(!is.na(df[[col]]) & nzchar(trimws(as.character(df[[col]]))))
    }

    has_multiple_values <- function(df, col) {
      if (!has_values(df, col)) return(FALSE)
      values <- as.character(df[[col]])
      values <- values[!is.na(values) & nzchar(trimws(values))]
      length(unique(values)) > 1
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

    metadata_label_map <- c(
      de_category = "DE category",
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

    metadata_choices <- function(df, include_none = FALSE) {
      choices <- c()
      if (include_none) choices[["None"]] <- "none"

      for (candidate in names(metadata_label_map)) {
        if (has_multiple_values(df, candidate)) {
          choices[[metadata_label_map[[candidate]]]] <- candidate
        }
      }
      choices
    }

    default_choice <- function(choices, preferred = character(0), fallback = NULL) {
      values <- unname(choices)
      for (candidate in preferred) {
        if (candidate %in% values) return(candidate)
      }
      if (!is.null(fallback) && fallback %in% values) return(fallback)
      if (length(values) == 0) return(character(0))
      values[[1]]
    }

    build_de_category <- function(df) {
      if (has_values(df, "de_category")) {
        values <- as.character(df$de_category)
        values[is.na(values) | !nzchar(values)] <- "unlabelled"
        return(values)
      }

      for (candidate in c("comparison", "DE", "category", "variable")) {
        if (has_values(df, candidate)) {
          values <- as.character(df[[candidate]])
          values[is.na(values) | !nzchar(values)] <- "unlabelled"
          return(values)
        }
      }

      if (!all(c("padj", "log2fc") %in% names(df))) {
        return(rep("No", nrow(df)))
      }

      dplyr::case_when(
        !is.na(df$padj) & df$padj < padj_thresh() & !is.na(df$log2fc) & df$log2fc > lfc_thresh() ~ "Up",
        !is.na(df$padj) & df$padj < padj_thresh() & !is.na(df$log2fc) & df$log2fc < -lfc_thresh() ~ "Down",
        TRUE ~ "No"
      )
    }

    searched_genes <- reactive({
      ds <- selected_dataset()
      if (is.null(ds) || !isTRUE(ds$omic_type %in% c("scrna", "snrna"))) return(character(0))
      genes <- unique(trimws(as.character(ds$genes %||% character(0))))
      genes[nzchar(genes)]
    })

    view_choices <- reactive({
      genes <- searched_genes()
      c("Dataset-wide" = "__dataset__", stats::setNames(genes, genes))
    })

    is_gene_mode <- reactive({
      selected <- input$feature_term %||% "__dataset__"
      nzchar(selected) && !identical(selected, "__dataset__")
    })

    dataset_metric_choices <- reactive({
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

    feature_expression_data <- reactive({
      if (!is_gene_mode()) return(NULL)
      ds <- selected_dataset()
      req(ds)

      fetch_expression_feature_values(
        lab_source = ds$lab_source,
        study_id = ds$study_id,
        genes = input$feature_term,
        assay = "logcounts",
        limit = 100000L
      )
    })

    x_axis_choices <- reactive({
      if (is_gene_mode()) {
        df <- feature_expression_data()
        if (is.null(df) || nrow(df) == 0) return(c("All cells" = "none"))
        metadata_choices(df, include_none = TRUE)
      } else {
        df <- de_data()
        if (is.null(df) || nrow(df) == 0) return(character(0))

        choices <- c()
        if (has_multiple_values(df, "de_category") ||
            (all(c("padj", "log2fc") %in% names(df)) &&
             any(!is.na(df$padj)) &&
             any(!is.na(df$log2fc)))) {
          choices[["DE category"]] <- "de_category"
        }

        for (candidate in names(metadata_label_map)[names(metadata_label_map) != "de_category"]) {
          if (has_multiple_values(df, candidate)) {
            choices[[metadata_label_map[[candidate]]]] <- candidate
          }
        }

        choices
      }
    })

    group_by_choices <- reactive({
      if (!is_gene_mode()) return(character(0))
      df <- feature_expression_data()
      if (is.null(df) || nrow(df) == 0) return(c("None" = "none"))
      metadata_choices(df, include_none = TRUE)
    })

    output$group_by_ui <- renderUI({
      if (!is_gene_mode()) return(NULL)
      fluidRow(
        column(4, selectInput(session$ns("group_by"), "Group by", choices = character(0)))
      )
    })

    observe({
      choices <- view_choices()
      selected <- input$feature_term %||% "__dataset__"
      if (!selected %in% unname(choices)) {
        selected <- "__dataset__"
      }
      updateSelectInput(session, "feature_term", choices = choices, selected = selected)
    })

    observe({
      choices <- if (is_gene_mode()) c("Expression level" = "expression_value") else dataset_metric_choices()
      if (length(choices) == 0) return()
      selected <- input$y_var %||% ""
      if (!selected %in% unname(choices)) {
        selected <- unname(choices)[1]
      }
      updateSelectInput(session, "y_var", choices = choices, selected = selected)
    })

    observe({
      choices <- x_axis_choices()
      if (length(choices) == 0) return()
      selected <- input$x_axis %||% ""
      preferred <- if (is_gene_mode()) {
        c("cluster_id", "cell_type", "condition_a", "condition_b", "tissue", "sex", "age", "sample_a")
      } else {
        c("de_category", "cell_type", "cluster_id", "condition_a", "condition_b", "sample_a")
      }
      if (!selected %in% unname(choices)) {
        selected <- default_choice(choices, preferred = preferred, fallback = "none")
      }
      updateSelectInput(session, "x_axis", choices = choices, selected = selected)
    })

    observe({
      if (!is_gene_mode()) return(invisible(NULL))
      choices <- group_by_choices()
      selected <- input$group_by %||% "none"
      preferred <- c("tissue", "sex", "condition_a", "condition_b", "sample_a", "sample_b")
      if (!selected %in% unname(choices)) {
        selected <- default_choice(choices, preferred = preferred, fallback = "none")
      }
      updateSelectInput(session, "group_by", choices = choices, selected = selected)
    })

    output$plot <- renderPlotly({
      if (!is_gene_mode()) {
        df <- de_data()
        validate(need(nrow(df) > 0, "No expression rows are available for the active dataset."))

        y_col <- input$y_var
        x_axis <- input$x_axis
        req(y_col %in% names(df))

        x_value <- if (identical(x_axis, "de_category")) {
          build_de_category(df)
        } else if (x_axis %in% names(df)) {
          as.character(df[[x_axis]])
        } else {
          rep("all", nrow(df))
        }
        x_value[is.na(x_value) | !nzchar(x_value)] <- "unlabelled"

        plot_df <- df |>
          dplyr::mutate(
            x_value = x_value,
            feature_label = feature_labels(df),
            sig = dplyr::case_when(
              !is.na(padj) & padj < padj_thresh() & log2fc > lfc_thresh() ~ "Up",
              !is.na(padj) & padj < padj_thresh() & log2fc < -lfc_thresh() ~ "Down",
              TRUE ~ "NS"
            )
          )
        plot_df <- plot_df[!is.na(plot_df[[y_col]]), , drop = FALSE]
        validate(need(nrow(plot_df) > 0, paste("No", y_col, "values are available for this dataset.")))

        x_levels <- unique(plot_df$x_value)
        x_title <- names(x_axis_choices())[match(x_axis, unname(x_axis_choices()))]
        if (is.na(x_title) || !nzchar(x_title)) x_title <- "X-axis"

        p <- plotly::plot_ly()
        for (x_name in x_levels) {
          group_df <- plot_df[plot_df$x_value == x_name, , drop = FALSE]
          p <- p |>
            plotly::add_trace(
              x = rep(x_name, nrow(group_df)),
              y = group_df[[y_col]],
              type = "violin",
              name = x_name,
              box = list(visible = isTRUE(input$show_box)),
              meanline = list(visible = TRUE),
              points = FALSE, #"outliers"
              line = list(color = "#D7DEE5"),
              fillcolor = "rgba(215, 222, 229, 0.45)",
              hovertemplate = paste0("<b>", x_name, "</b><br>", y_col, ": %{y:.3f}<extra></extra>"),
              showlegend = FALSE
            )
        }

        point_text <- paste0(
          "<b>", plot_df$x_value, "</b><br>",
          y_col, ": ", signif(plot_df[[y_col]], 3), "<br>",
          "feature: ", plot_df$feature_label, "<br>",
          "padj: ", signif(plot_df$padj, 3), "<br>",
          "class: ", plot_df$sig
        )

        return(
          p |>
            plotly::add_trace(
              x = plot_df$x_value,
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
              legend = list(title = list(text = "Significance"), orientation = "h", y = -0.15),
              xaxis = list(title = x_title, tickangle = -30),
              yaxis = list(title = y_col),
              showlegend = TRUE
            )
        )
      }

      df <- feature_expression_data()
      validate(need(nrow(df) > 0, "No feature-level expression values are available for the selected gene."))

      x_axis <- input$x_axis %||% "none"
      group_by <- input$group_by %||% "none"
      x_choices <- x_axis_choices()
      group_choices <- group_by_choices()

      x_value <- if (identical(x_axis, "none") || !x_axis %in% names(df)) {
        rep(input$feature_term, nrow(df))
      } else {
        values <- as.character(df[[x_axis]])
        values[is.na(values) | !nzchar(values)] <- "unlabelled"
        values
      }

      colour_value <- if (identical(group_by, "none") || !group_by %in% names(df)) {
        rep("Expression", nrow(df))
      } else {
        values <- as.character(df[[group_by]])
        values[is.na(values) | !nzchar(values)] <- "unlabelled"
        values
      }

      plot_df <- df |>
        dplyr::mutate(
          x_value = x_value,
          colour_value = colour_value,
          feature_label = feature_labels(df)
        )
      plot_df <- plot_df[!is.na(plot_df$expression_value), , drop = FALSE]
      validate(need(nrow(plot_df) > 0, "No expression values are available for the selected gene."))

      x_title <- names(x_choices)[match(x_axis, unname(x_choices))]
      if (is.na(x_title) || !nzchar(x_title)) x_title <- "Selected gene"
      group_title <- names(group_choices)[match(group_by, unname(group_choices))]
      if (is.na(group_title) || !nzchar(group_title)) group_title <- "Group by"

      palette <- c("#5B8FF9", "#61DDAA", "#F6BD16", "#E8684A", "#6DC8EC", "#9270CA")
      colour_levels <- unique(plot_df$colour_value)
      colour_map <- stats::setNames(rep_len(palette, length(colour_levels)), colour_levels)

      p <- plotly::plot_ly()
      for (colour_name in colour_levels) {
        group_df <- plot_df[plot_df$colour_value == colour_name, , drop = FALSE]
        marker_colour <- colour_map[[colour_name]]
        p <- p |>
          plotly::add_trace(
            x = group_df$x_value,
            y = group_df$expression_value,
            type = "violin",
            name = colour_name,
            legendgroup = colour_name,
            scalegroup = colour_name,
            box = list(visible = isTRUE(input$show_box)),
            meanline = list(visible = TRUE),
            points = FALSE,
            line = list(color = marker_colour),
            fillcolor = grDevices::adjustcolor(marker_colour, alpha.f = 0.25),
            showlegend = !identical(group_by, "none"),
            hovertemplate = paste0("<b>", colour_name, "</b><br>Expression: %{y:.3f}<extra></extra>")
          ) |>
          plotly::add_trace(
            x = group_df$x_value,
            y = group_df$expression_value,
            type = "scatter",
            mode = "markers",
            name = colour_name,
            legendgroup = colour_name,
            showlegend = FALSE,
            marker = list(size = 5, opacity = 0.55, color = marker_colour),
            text = paste0(
              "<b>", group_df$feature_label, "</b><br>",
              "obs: ", group_df$obs, "<br>",
              x_title, ": ", group_df$x_value,
              if (!identical(group_by, "none")) paste0("<br>", group_title, ": ", group_df$colour_value) else "",
              "<br>Expression: ", signif(group_df$expression_value, 3)
            ),
            hoverinfo = "text"
          )
      }

      p |>
        plotly::layout(
          violinmode = if (identical(group_by, "none")) "overlay" else "group",
          legend = list(
            title = list(text = if (identical(group_by, "none")) "" else group_title),
            orientation = if (identical(group_by, "none")) "h" else "v",
            y = if (identical(group_by, "none")) -0.15 else 1
          ),
          xaxis = list(title = x_title, tickangle = -30),
          yaxis = list(title = "Expression level"),
          showlegend = !identical(group_by, "none")
        )
    })
  })
}

`%||%` <- function(a, b) if (!is.null(a)) a else b
