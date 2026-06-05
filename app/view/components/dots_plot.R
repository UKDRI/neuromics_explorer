# ─────────────────────────────────────────────────────────────────────────────
# Dots Plot Module
# ─────────────────────────────────────────────────────────────────────────────
# GOI-focused dot plot for sc/snRNA datasets.
# Features:
#   - Dot size = % expressing
#   - Dot color = average counts
#   - Grouping is driven by canonical metadata returned with feature-level rows

box::use(
  shiny[moduleServer, NS, reactive, req, tagList, selectInput, updateSelectInput, validate, need, observe],
  plotly[plotlyOutput, renderPlotly, plot_ly, layout, add_trace, add_annotations, subplot],
  dplyr[mutate, summarise, group_by],
  stringi[stri_sort],
  app/logic/api/api_client[fetch_expression_feature_values],
)

#' @export
dots_ui <- function(id) {
  ns <- NS(id)
  tagList(
    selectInput(ns("group_by"), "Group by", choices = character(0)),
    plotlyOutput(ns("dots_plot"), height = "620px")
  )
}

#' @param selected_dataset Reactive list describing the active dataset and selected terms.
#' @export
dots_server <- function(id, selected_dataset) {
  moduleServer(id, function(input, output, session) {
    group_label_map <- c(
      cluster_id = "Cluster",
      cell_type = "Cell type",
      tissue = "Tissue",
      age = "Age",
      sex = "Sex",
      sample_a = "Sample A",
      sample_b = "Sample B",
      condition_a = "Condition A",
      condition_b = "Condition B"
    )
    detection_limit <- 1

    has_multiple_values <- function(df, col) {
      if (!col %in% names(df)) return(FALSE)
      values <- as.character(df[[col]])
      values <- values[!is.na(values) & nzchar(trimws(values))]
      length(unique(values)) > 1
    }

    feature_labels <- function(df) {
      label <- rep(NA_character_, nrow(df))
      for (candidate in c("gene_symbol", "human_gene", "protein_id")) {
        if (!candidate %in% names(df)) next
        values <- as.character(df[[candidate]])
        keep <- is.na(label) & !is.na(values) & nzchar(values)
        label[keep] <- values[keep]
      }
      label[is.na(label) | !nzchar(label)] <- "feature"
      label
    }

    is_single_cell <- reactive({
      ds <- selected_dataset()
      !is.null(ds) && isTRUE(ds$omic_type %in% c("scrna", "snrna"))
    })

    selected_terms <- reactive({
      ds <- selected_dataset()
      if (is.null(ds)) return(list(genes = character(0), proteins = character(0)))

      genes <- unique(trimws(as.character(ds$genes %||% character(0))))
      genes <- genes[nzchar(genes)]
      proteins <- unique(trimws(as.character(ds$proteins %||% character(0))))
      proteins <- proteins[nzchar(proteins)]

      list(genes = genes, proteins = proteins)
    })

    feature_rows <- reactive({
      ds <- selected_dataset()
      terms <- selected_terms()
      req(ds, is_single_cell(), length(c(terms$genes, terms$proteins)) > 0)

      fetch_expression_feature_values(
        lab_source = ds$lab_source,
        study_id = ds$study_id,
        genes = terms$genes,
        proteins = terms$proteins,
        assay = "counts",
        limit = 300000L
      )
    })

    group_choices <- reactive({
      df <- feature_rows()
      choices <- c()
      for (candidate in names(group_label_map)) {
        if (has_multiple_values(df, candidate)) {
          choices[[group_label_map[[candidate]]]] <- candidate
        }
      }
      choices
    })

    observe({
      choices <- group_choices()
      if (length(choices) == 0) return()
      selected <- input$group_by %||% ""
      preferred <- c("cluster_id", "cell_type", "tissue", "age", "sex")
      if (!selected %in% unname(choices)) {
        selected <- unname(choices)[1]
        for (candidate in preferred) {
          if (candidate %in% unname(choices)) {
            selected <- candidate
            break
          }
        }
      }
      updateSelectInput(session, "group_by", choices = choices, selected = selected)
    })

    dot_data <- reactive({
      df <- feature_rows()
      req(nrow(df) > 0)

      group_col <- input$group_by %||% ""
      req(group_col %in% names(df))

      labels <- feature_labels(df)
      group_values <- as.character(df[[group_col]])
      group_values[is.na(group_values) | !nzchar(group_values)] <- "unlabelled"

      plot_df <- df |>
        dplyr::mutate(
          feature_label = labels,
          group_value = group_values
        ) |>
        dplyr::group_by(feature_label, group_value) |>
        dplyr::summarise(
          Average = mean(expression_value, na.rm = TRUE),
          pct_expressing = 100 * mean(expression_value > detection_limit, na.rm = TRUE),
          NumDetected = sum(expression_value > detection_limit, na.rm = TRUE),
          .groups = "drop"
        )

      plot_df
    })

    output$dots_plot <- renderPlotly({
      validate(need(is_single_cell(), "Dot plot is only available for scRNA-seq and snRNA-seq datasets."))
      terms <- selected_terms()
      validate(need(length(c(terms$genes, terms$proteins)) > 0, "Select one or more genes or proteins to draw the dot plot."))

      df <- dot_data()
      validate(need(nrow(df) > 0, "No feature-level rows are available for the selected terms."))

      max_size <- 17
      max_pct <- suppressWarnings(max(df$pct_expressing, na.rm = TRUE))
      if (!is.finite(max_pct) || max_pct <= 0) max_pct <- 1
      sizeref <- 2 * max_pct / (max_size^2)
      if (!is.finite(sizeref) || sizeref <= 0) sizeref <- 1

      colouring <- c("#440154", "#21908C", "#FDE725")
      plot_width <- max(900, length(unique(df$group_value)) * 45)
      plot_height <- max(600, length(unique(df$feature_label)) * 40)

      x_sort <- stringi::stri_sort(unique(df$group_value), numeric = TRUE)  #x-axis numeric ordering

      p <- plotly::plot_ly(
        data = df,
        x = ~group_value,
        y = ~feature_label,
        type = "scatter",
        mode = "markers",
        size = ~pct_expressing,
        color = ~Average,
        colors = colouring,
        marker = list(
          sizemode = "area",
          sizeref = sizeref,
          sizemin = 2,
          opacity = 0.85,
          line = list(width = 0)
        ),
        text = ~paste(
          "Feature:", feature_label,
          "<br>Group:", group_value,
          "<br>Average:", round(Average, 3),
          "<br>% Expressing:", round(pct_expressing, 1),
          "<br>NumDetected:", NumDetected
        ),
        hoverinfo = "text"
      ) |>
        plotly::layout(
          autosize = TRUE,
          plot_bgcolor = "white",
          paper_bgcolor = "white",
          width = plot_width,
          height = plot_height,
          xaxis = list(
            title = "Group",
            tickmode = "array",
            tickvals = unique(df$group_value),
            ticktext = unique(df$group_value),
            tickangle = 45,
            automargin = TRUE,
            categoryorder = "array", categoryarray = x_sort
          ),
          yaxis = list(
            title = "Feature",
            automargin = TRUE
          )
        )

      legend_pct <- c(0, 25, 50, 75, 100)
      size_panel <- plotly::plot_ly() |>
        plotly::layout(
          xaxis = list(visible = FALSE, range = c(0, 1)),
          yaxis = list(visible = FALSE, range = c(0, length(legend_pct) + 1)),
          showlegend = FALSE,
          plot_bgcolor = "rgba(0,0,0,0)",
          paper_bgcolor = "rgba(0,0,0,0)"
        )

      for (i in seq_along(legend_pct)) {
        s <- legend_pct[i]
        size_panel <- size_panel |>
          plotly::add_trace(
            x = 0.5,
            y = i,
            type = "scatter",
            mode = "markers+text",
            marker = list(
              size = if (s == 0) 5 else sqrt(s / sizeref) * 1.5,
              color = "black",
              opacity = 0.7,
              sizemode = "area",
              sizeref = sizeref,
              line = list(width = 0)
            ),
            text = paste0(" ", s, "%"),
            textposition = "middle right",
            hoverinfo = "skip",
            showlegend = FALSE
          )
      }

      size_panel <- size_panel |>
        plotly::add_annotations(
          x = 0.5,
          y = length(legend_pct) + 0.8,
          text = "% Expressed",
          showarrow = FALSE,
          xref = "x",
          yref = "y",
          font = list(size = 12)
        )

      plotly::subplot(
        p,
        size_panel,
        widths = c(0.85, 0.15),
        shareY = FALSE,
        titleX = TRUE,
        titleY = TRUE
      )
    })
  })
}

#' @noRd
`%||%` <- function(a, b) if (!is.null(a)) a else b
