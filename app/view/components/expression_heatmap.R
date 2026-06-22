# Top N DE genes across canonical metadata groupings.

box::use(
  shiny[NS, moduleServer, reactive, req, tagList, selectInput, updateSelectInput,
        observe, uiOutput, renderUI, checkboxGroupInput, tags],
  plotly[plotlyOutput, renderPlotly, plot_ly, layout],
  stringi[stri_sort],
  tidyr[pivot_wider],
  tibble[column_to_rownames],
  app/logic/api/api_client[fetch_top_de, fetch_expression_goi],
)

#' @export
heatmap_ui <- function(id) {
  ns <- NS(id)
  tagList(
    selectInput(ns("x_axis"), "X-axis", choices = character(0)),
    uiOutput(ns("gene_selector_ui")),
    plotlyOutput(ns("plot"), height = "600px")
  )
}

#' @param selected_dataset reactive list(lab_source, study_id, ...)
#' @param padj_thresh reactive numeric
#' @param lfc_thresh reactive numeric
#' @param n_genes reactive integer (top N)
#' @export
heatmap_server <- function(id, selected_dataset,
                           padj_thresh, lfc_thresh, n_genes) {
  moduleServer(id, function(input, output, session) {
    
    has_values <- function(df, col) {
      col %in% names(df) &&
        any(!is.na(df[[col]]) & nzchar(trimws(as.character(df[[col]]))))
    }

    has_multiple_values <- function(df, col) {
      if (!col %in% names(df)) return(FALSE)
      values <- as.character(df[[col]])
      values <- values[!is.na(values) & nzchar(trimws(values))]
      length(unique(values)) > 1
    }

    build_de_category <- function(df) {
      if (has_values(df, "de_category")) {
        values <- as.character(df$de_category)
        values[is.na(values) | !nzchar(values)] <- "unlabelled"
        return(values)
      }
      rep("All", nrow(df))
    }

    group_label_map <- c(
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

    top_genes <- reactive({
      ds <- selected_dataset()
      req(ds)
      df <- fetch_top_de(
        lab_source = ds$lab_source,
        study_id = ds$study_id,
        n = n_genes(),
        padj_thresh = padj_thresh(),
        lfc_thresh = lfc_thresh()
      )
      df
    })

    searched_gene_terms <- reactive({
      ds <- selected_dataset()
      if (is.null(ds)) return(character(0))
      genes <- unique(trimws(as.character(ds$genes %||% character(0))))
      genes[nzchar(genes)]
    })

    output$gene_selector_ui <- renderUI({
      terms <- searched_gene_terms()
      if (length(terms) == 0) return(NULL)

      tags$details(
        style = "margin-bottom: 14px;",
        tags$summary(
          style = "cursor: pointer; font-weight: 600; margin-bottom: 8px;",
          "Genes to display on the heatmap"
        ),
        checkboxGroupInput(
          session$ns("heatmap_terms"),
          label = NULL,
          choices = terms,
          selected = terms
        )
      )
    })

    heatmap_data <- reactive({
      top_df <- top_genes()

      req(nrow(top_df) > 0)

      top_gene_symbols <- unique(top_df$gene_symbol)
      selected_terms <- input$heatmap_terms %||% character(0)
      selected_terms <- selected_terms[nzchar(selected_terms)]
      all_genes <- unique(c(top_gene_symbols, selected_terms))

      ds <- selected_dataset()
      req(ds)
      fetch_expression_goi(
        lab_source = ds$lab_source,
        study_id = ds$study_id,
        genes = all_genes,
        limit = 5000L
      )

    })

    x_axis_choices <- reactive({
      df <- heatmap_data()
      # Ensure GOI and top-de rows have de_category column
      df$de_category <- build_de_category(df)
      choices <- c()
      for (candidate in names(group_label_map)) {
        if (has_multiple_values(df, candidate)) {
          choices[[group_label_map[[candidate]]]] <- candidate
        } else if (
          has_values(df, "de_category") ||
          (all(c("padj", "log2fc") %in% names(df)) &&
          any(!is.na(df$padj)) &&
          any(!is.na(df$log2fc)))
        ) {
          choices[["DE category"]] <- "de_category"
        }
      }
      # or
      # for (candidate in names(group_label_map)) {
      #   if (candidate == "de_category") {
      #     if (has_values(df, "de_category")) {
      #       choices[[group_label_map[[candidate]]]] <- candidate
      #     }
      #     next
      #   }
      #   if (has_multiple_values(df, candidate)) {
      #     choices[[group_label_map[[candidate]]]] <- candidate  #choices[["DE category"]] <- "de_category"
      #   }
      # }
      choices
    })

    observe({
      choices <- x_axis_choices()
      if (length(choices) == 0) return()
      selected <- input$x_axis %||% ""
      preferred <- c("de_category", "cell_type", "cluster_id", "condition_a", "condition_b", "sample_a")
      if (!selected %in% unname(choices)) {
        selected <- unname(choices)[1]
        for (candidate in preferred) {
          if (candidate %in% unname(choices)) {
            selected <- candidate
            break
          }
        }
      }
      updateSelectInput(session, "x_axis", choices = choices, selected = selected)
    })

    output$plot <- renderPlotly({
      df <- heatmap_data()
      req(nrow(df) > 0)

      df$de_category <- build_de_category(df)

      group_col <- input$x_axis
      if (!group_col %in% names(df)) {
        choices <- x_axis_choices()
        req(length(choices) > 0)
        group_col <- unname(choices)[1]
      }

      plot_source <- df[, intersect(c("gene_symbol", group_col, "log2fc"), names(df)), drop = FALSE]
      # plot_source <- df[, c("gene_symbol", group_col, "log2fc"), drop = FALSE]
      names(plot_source)[names(plot_source) == group_col] <- "group"
      plot_source <- plot_source[!is.na(plot_source$gene_symbol) & (!is.na(plot_source$group)), , drop = FALSE]
      req(nrow(plot_source) > 0)

      selected_terms <- input$heatmap_terms %||% character(0)
      selected_terms <- selected_terms[selected_terms %in% plot_source$gene_symbol]
      row_order <- c(selected_terms, setdiff(unique(plot_source$gene_symbol), selected_terms))

      mat_df <- plot_source |>
        tidyr::pivot_wider(
          id_cols = gene_symbol,
          names_from = group,
          values_from = log2fc,
          values_fn = mean, #value
          values_fill = NA_real_  #filters out genes or gene-cluster combos that have been dropped due to significance filter, shows up as a neutral gaps/transparent 
        ) |>
        tibble::column_to_rownames("gene_symbol")

      req(nrow(mat_df) > 0)

      mat_df <- mat_df[row_order[row_order %in% rownames(mat_df)], , drop = FALSE]

      mat <- as.matrix(mat_df)
      group_title <- names(x_axis_choices())[match(group_col, unname(x_axis_choices()))]

      plotly::plot_ly(
        z = mat,
        x = colnames(mat),
        y = rownames(mat),
        type = "heatmap",
        colorscale = list(
          c(0, "#2980B9"),   # blue  (down)
          c(0.5, "#FFFFFF"), # white (0)
          c(1, "#C0392B")    # red   (up)
        ),
        zmid = 0,
        hovertemplate = "%{y} · %{x}<br>log2FC: %{z:.3f}<extra></extra>"
      ) |>
        plotly::layout(
          xaxis = list(title = group_title %||% "X-axis", tickangle = -45,
            type = "category",
            categoryorder = "array",
            categoryarray = stringi::stri_sort(colnames(mat)),
            tickmode = "array",
            tickvals = colnames(mat),
            ticktext = colnames(mat),
            tickangle = 45),
          yaxis = list(title = "",
                       automargin = TRUE,
                       tickmode   = "array",
                       tickvals   = seq_along(rownames(mat)) - 1,
                       ticktext   = rownames(mat),
                       tickfont   = list(size = 10))
        )
    })
  })
}

`%||%` <- function(a, b) if (!is.null(a)) a else b
