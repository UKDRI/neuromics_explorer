box::use(
  shiny[NS, bindCache, moduleServer, reactive, req, tagList, selectInput, updateSelectInput,
        observe, uiOutput, renderUI, checkboxGroupInput, tags],
  shinycssloaders[withSpinner],
  plotly[plotlyOutput, renderPlotly, plot_ly, layout],
  stringi[stri_sort],
  tidyr[pivot_wider],
  tibble[column_to_rownames],
  app/logic/api/api_client[fetch_top_de, fetch_expression_goi],
  app/view/components/helpers/de_helpers[build_de_category, pick_strongest],
)

#' Collapsible context explaining how one heatmap cell is produced.
#'
#' @export
heatmap_info_ui <- function() {
  tags$details(
    class = "alert alert-info",
    style = "font-size: 12px; line-height: 1.5; padding: 8px 12px; margin: 10px 0 0;",
    tags$summary(
      style = "cursor: pointer; font-weight: 600; font-size: 13px;",
      "How each heatmap cell is calculated"
    ),
    tags$div(
      style = "margin-top: 8px;",

      tags$p(
        style = "margin: 0 0 6px;",
        tags$b("One cell = one log\u2082FC."),
        " A gene can appears more than once inside the same DE category (X-axis) e.g. the same gene
         measured across several cell types in single-cell dataset, or across several samples,
         replicates or contrasts. Those values have to be collapsed into the single number the
         cell can show."
      ),

      tags$p(
        style = "margin: 0 0 6px;",
        tags$b("Strongest change used."),
        " Of the duplicate values, the heatmap cells display the largest absolute log\u2082FC, i.e.
         (the largest change), whether up or down. The ", tags$i("mean/ average"), " is avoided due to a bias towards",
        tags$i("0"), ", which would render it white and read as \u201cno change\u201d instead of \u201cmixed\u201d. The",
        tags$i("most statistically significant"),
        " was also avoided due to bias towards tiny but significant changes, which may not be biologically meaningful"
      ),

      tags$p(
        style = "margin: 0 0 6px; padding: 6px 8px; background: rgba(255,255,255,0.55); border-radius: 4px;",
        tags$b("Bias to be aware of: "),
        "the strongest value wins even when it is the least reliable. If a gene shifts
         0.3 log\u2082FC across 5,000 astrocytes (padj 1e-20) but 4.2 log\u2082FC across just
         12 microglia (padj 0.4), the cell shows ", tags$b("4.2"),
        " \u2014 a large change supported by a handful of cells."
      ),

      tags$p(
        style = "margin: 0 0 6px;",
        tags$b("So for a dataset with multiple variables"),
        ", you may want to switch the X-axis to something more appropriate than the default 'DE category'. That way
         nothing is collapsed and each column is shown and you can see where the signal actually comes from."
      ),

      tags$p(
        style = "margin: 0;",
        tags$b("How the DE labels are categorised: "),
        "where a lab supplied its own contrast labels, those are shown as-is (which is why some
         datasets show contrast names such as ",
        tags$code("APOE3__APOE4"), " which signifies", tags$code("APOE3 vs APOE4"), " rather than Up/Down).
         Otherwise each row is labelled from the sidebar's significant threshold filters: \n ",
        tags$b("Up"), " = significant and increased, ",
        tags$b("Down"), " = significant and decreased, ",
        tags$b("No change"), " = everything else."
      )
    )
  )
}

#' @export
heatmap_ui <- function(id) {
  ns <- NS(id)
  tagList(
    selectInput(ns("x_axis"), "X-axis", choices = character(0)),
    uiOutput(ns("gene_selector_ui")),
    plotlyOutput(ns("plot"), height = "600px") |> withSpinner(
      type = 1, caption = "Loading plot...", color = "#5b5b5b"),
    heatmap_info_ui()
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
    }) |>
      bindCache(
        selected_dataset()$lab_source,
        selected_dataset()$study_id,
        n_genes(),
        padj_thresh(),
        lfc_thresh()
      )

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

    }) |>
      bindCache(
        selected_dataset()$lab_source,
        selected_dataset()$study_id,
        n_genes(),
        padj_thresh(),
        lfc_thresh(),
        paste(input$heatmap_terms %||% character(0), collapse = ",")
      )

    x_axis_choices <- reactive({
      df <- heatmap_data()
      # Ensure GOI and top-de rows have de_category column
      df$de_category <- build_de_category(df, padj_thresh(), lfc_thresh())
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

      df$de_category <- build_de_category(df, padj_thresh(), lfc_thresh())

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
          values_fn = pick_strongest,   # mean, value
          values_fill = NA_real_  #filters out genes or gene-cluster combos that have been dropped due to significance filter, shows up as a neutral gaps/transparent 
        )

      req(nrow(mat_df) > 0)

      # Replaces `pivot_wider |> column_to_rownames("gene_symbol")` to prevent issues with values bleeding through instead of NA_real_ on hover tip
      gene_col <- mat_df$gene_symbol
      mat_df$gene_symbol <- NULL
      mat <- as.matrix(mat_df)
      rownames(mat) <- gene_col

      row_idx <- match(row_order, rownames(mat))
      row_idx <- row_idx[!is.na(row_idx)]
      mat <- mat[row_idx, , drop = FALSE]

      # Symmetric limits so 0 always sits on white and red/blue intensity is comparable across tabs and datasets (rather than auto-ranging).
      z_limit <- suppressWarnings(max(abs(mat), na.rm = TRUE))
      if (!is.finite(z_limit) || z_limit == 0) z_limit <- 1

      group_title <- names(x_axis_choices())[match(group_col, unname(x_axis_choices()))]

      plotly::plot_ly(
        z = mat,
        x = colnames(mat),
        y = rownames(mat),
        type = "heatmap",
        zsmooth=FALSE, hoverongaps = FALSE,   # prevents values bleeding into hovertip instead of showing NA/null (ie 'Cluster' grouping on scrna)
        colorscale = list(
          c(0, "#2980B9"),   # blue  (down)
          c(0.5, "#FFFFFF"), # white (0)
          c(1, "#C0392B")    # red   (up)
        ),
        zmid = 0, zmin = -z_limit, zmax = z_limit,
        colorbar = list(title = list(text = "log\u2082FC", side = "right", font = list(size = 11)),
                        thickness = 14, len = 0.6, ticks = "outside",
                        tickfont = list(size = 10), outlinewidth = 0),
        hovertemplate = "%{y} · %{x}<br>log2FC: %{z:.3f}<extra></extra>"
        # hovertemplate = ifelse(
        #   is.na(df$condition_b),
        #   paste0("%{y} · %{x} (vs. rest)<br>log2FC: %{z:.3f}<extra></extra>"),
        #   paste0("%{y} · %{x} (vs. ", df$condition_b, ")<br>log2FC: %{z:.3f}<extra></extra>")
        # )
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
    }) |>
      bindCache(
        selected_dataset()$lab_source,
        selected_dataset()$study_id,
        n_genes(),
        padj_thresh(),
        lfc_thresh(),
        input$x_axis,
        paste(input$heatmap_terms %||% character(0), collapse = ",")
      )
  })
}

`%||%` <- function(a, b) if (!is.null(a)) a else b
