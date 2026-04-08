# Targeted feature scatter plot for genes/proteins of interest.

box::use(
  shiny[moduleServer, NS, reactive, renderUI, req, validate, need, tagList],
  plotly[plotlyOutput, renderPlotly, plot_ly, layout],
  bslib[card, card_body, card_header],
  dplyr[mutate, case_when],
  app/logic/api/api_client[fetch_expression_goi],
)

#' @export
feature_scatter_ui <- function(id) {
  ns <- NS(id)
  tagList(
    card(
      full_screen = FALSE,
      style = "margin-bottom: 1rem;",
      card_header("Selected gene/protein scatter"),
      card_body(
        shiny::uiOutput(ns("term_note"))
      )
    ),
    plotlyOutput(ns("scatter_plot"), height = "520px")
  )
}

#' @param selected_dataset Reactive list describing the active dataset and modal selections.
#' @param padj_thresh Reactive numeric significance threshold used for colouring.
#' @param lfc_thresh Reactive numeric fold-change threshold used for colouring.
#' @export
feature_scatter_server <- function(id, selected_dataset, padj_thresh, lfc_thresh) {
  moduleServer(id, function(input, output, session) {

    selected_terms <- reactive({
      ds <- selected_dataset()
      req(ds)
      terms <- unique(c(ds$genes %||% character(0), ds$proteins %||% character(0)))
      terms[nzchar(terms)]
    })

    plot_data <- reactive({
      ds <- selected_dataset()
      req(ds)
      terms <- selected_terms()
      req(length(terms) > 0)

      fetch_expression_goi(
        lab_source = ds$lab_source,
        study_id = ds$study_id,
        genes = ds$genes %||% character(0),
        proteins = ds$proteins %||% character(0),
        limit = 2000L
      )
    })

    output$term_note <- renderUI({
      terms <- selected_terms()
      if (length(terms) == 0) {
        return("Select one or more genes or proteins in the modal to inspect them here.")
      }

      paste(
        "Showing rows for:",
        paste(terms, collapse = ", "),
        "This plot uses the GOI endpoint so it stays focused on selected features."
      )
    })

    output$scatter_plot <- renderPlotly({
      df <- plot_data()
      validate(need(nrow(df) > 0, "No rows are available for the selected genes/proteins."))

      x_col <- if ("abundance_a" %in% names(df) && any(!is.na(df$abundance_a))) "abundance_a" else "log2fc"
      y_col <- if ("abundance_b" %in% names(df) && any(!is.na(df$abundance_b))) "abundance_b" else "pvalue"
      df <- df |>
        dplyr::mutate(
          feature_label = ifelse(!is.na(gene_symbol) & nzchar(gene_symbol), gene_symbol, protein_id),
          sig = dplyr::case_when(
            !is.na(padj) & padj < padj_thresh() & log2fc >  lfc_thresh() ~ "Up",
            !is.na(padj) & padj < padj_thresh() & log2fc < -lfc_thresh() ~ "Down",
            TRUE ~ "NS"
          )
        )
      x_values <- df[[x_col]]
      df$y_value <- if (identical(y_col, "pvalue")) -log10(pmax(df$pvalue, 1e-300)) else df[[y_col]]

      plotly::plot_ly(
        df,
        x = x_values,
        y = ~y_value,
        type = "scatter",
        mode = "markers",
        color = ~sig,
        colors = c(Up = "#C0392B", Down = "#2980B9", NS = "#BDC3C7"),
        text = ~paste0(
          "<b>", feature_label, "</b><br>",
          x_col, ": ", signif(x_values, 3), "<br>",
          if (identical(y_col, "pvalue")) "-log10(pvalue)" else y_col, ": ", signif(y_value, 3), "<br>",
          "cell_type: ", ifelse(is.na(cell_type), "NA", cell_type), "<br>",
          "padj: ", signif(padj, 3)
        ),
        hoverinfo = "text",
        marker = list(size = 8, opacity = 0.75)
      ) |>
        plotly::layout(
          xaxis = list(title = x_col),
          yaxis = list(title = if (identical(y_col, "pvalue")) "-log10(pvalue)" else y_col),
          legend = list(title = list(text = "Significance"), orientation = "h", y = -0.15)
        )
    })
  })
}

#' @noRd
`%||%` <- function(a, b) if (!is.null(a)) a else b
