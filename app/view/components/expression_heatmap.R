box::use(
  shiny[NS, bindCache, moduleServer, reactive, req, tagList, selectInput, updateSelectInput,
        observe, uiOutput, renderUI, checkboxGroupInput, tags, validate, need],
  shinycssloaders[withSpinner],
  plotly[plotlyOutput, renderPlotly, plot_ly, layout],
  stringi[stri_sort],
  tidyr[pivot_wider],
  app/logic/api/api_client[fetch_top_de, fetch_expression_heatmap,
                           fetch_expression_grouping_options],
  app/view/components/helpers/de_helpers[pick_strongest],
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
        " A gene can appears more than once inside the same Contrast / DE category (X-axis) e.g. the same gene
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
        ", you may want to switch the X-axis to something more appropriate than the default 'Contrast / DE category'. That way
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
    
    group_label_map <- c(
      de_category = "Contrast / DE category",
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


          # # VS
          # # Follows Compare tab's "Show me only the cells that passed my thresholds"
          # # Thresholds control only which cells are drawn; Good when there are a few columns e.g. Up / Down / No change
          # ds <- selected_dataset()
          # req(ds)
          # goi_df <- if (length(selected_terms) > 0) {
          #   fetch_expression_goi(
          #     lab_source = ds$lab_source,
          #     study_id = ds$study_id,
          #     genes = selected_terms,
          #     limit = 5000L
          #   )
          # } else NULL
          # if (is.null(goi_df) || nrow(goi_df) == 0) return(top_df)

          # # Align columns from '/expression/top-de' vs '/expression/goi'
          # # Duplicate gene x group rows are expected and are collapsed later by pick_strongest().
          # common_cols <- union(names(top_df), names(goi_df))
          # for (col in setdiff(common_cols, names(top_df))) top_df[[col]] <- NA
          # for (col in setdiff(common_cols, names(goi_df))) goi_df[[col]] <- NA
          # rbind(top_df[, common_cols, drop = FALSE], goi_df[, common_cols, drop = FALSE])
    
    # Gene set the heatmap plots: top-DE picks which genes are interesting, the searched terms
    # are always included. No req() on top_df - '/top-de' filters on `WHERE padj < ?`, so data
    # carrying only p-values (eg hong proteomics has pvalue but no padj) returns zero rows.
    # Gating = empty x-axis dropdown = blank plot
    plot_genes <- reactive({
      selected_terms <- input$heatmap_terms %||% character(0)
      selected_terms <- selected_terms[nzchar(selected_terms)]

      top_df <- top_genes()
      top_gene_symbols <- if (nrow(top_df) > 0) unique(top_df$gene_symbol) else character(0)

      all_genes <- unique(c(top_gene_symbols, selected_terms))
      req(length(all_genes) > 0)
      all_genes
    })

    # Single row of counts per candidate column (eg n_cell_type, n_de_category etc.)
    # Used to populate X-axis dropdown to filter data by grouping col.)
    # TODO: change in compare tab, also
    grouping_opts <- reactive({
      ds <- selected_dataset()
      req(ds)
      fetch_expression_grouping_options(
        lab_source = ds$lab_source,
        study_id = ds$study_id,
        genes = plot_genes()
      )
    }) |>
      bindCache(
        selected_dataset()$lab_source,
        selected_dataset()$study_id,
        paste(plot_genes(), collapse = ",")
      )

    # input$x_axis is empty on the first render (the observe() below populates the picker afterwards)
    # and can hold a stale column after a dataset switch - prevents what looks like a hung plot.
    #  UI: X-axis dropdown is populated by grouping_opts() and the first valid choice is selected.
    active_group_col <- reactive({
      choices <- x_axis_choices()
      req(length(choices) > 0)
      col <- input$x_axis
      if (!isTRUE(col %in% unname(choices))) col <- unname(choices)[1]
      col
    })

    # One row per heatmap cell: gene_symbol, group_label, log2fc. DuckDB applies the same
    # max-abs collapse as pick_strongest(), so nothing is truncated by the API row cap.
    heatmap_data <- reactive({
      ds <- selected_dataset()
      req(ds)
      group_col <- active_group_col()

      fetch_expression_heatmap(
        lab_source = ds$lab_source,
        study_id = ds$study_id,
        genes = plot_genes(),
        group_by = group_col,
        padj_thresh = padj_thresh(),
        lfc_thresh = lfc_thresh()
      )
    }) |>
      bindCache(
        selected_dataset()$lab_source,
        selected_dataset()$study_id,
        paste(plot_genes(), collapse = ","),
        active_group_col(),
        padj_thresh(),
        lfc_thresh()
      )

    # Candidate grouping columns, e.g. 'Contrast / DE category' is offered when the lab supplied labels, from
    # our pipelines, or there are padj + log2fc to derive Up / Down / No change from - matching build_de_category labelling.
    MAX_GROUPS <- 100L   # gaurds against heatmaps w more columns than is unreadable
    x_axis_choices <- reactive({
      opts <- grouping_opts()
      req(nrow(opts) > 0)

      n <- function(col) {
        v <- opts[[paste0("n_", col)]]
        if (is.null(v) || length(v) == 0 || is.na(v[1])) 0L else as.integer(v[1])
      }

      choices <- c()
      if (n("de_category") > 0 || (n("padj") > 0 && n("log2fc") > 0)) {
        choices[[group_label_map[["de_category"]]]] <- "de_category"
      }
      for (candidate in setdiff(names(group_label_map), "de_category")) {
        count <- n(candidate)
        if (count > 1 && count <= MAX_GROUPS) {
          choices[[group_label_map[[candidate]]]] <- candidate
        }
      }
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
      plot_source <- heatmap_data()
      validate(need(
        nrow(plot_source) > 0,
        "No expression rows for these genes. Search for a gene, or relax the significance thresholds."
      ))

      group_col <- active_group_col()
      names(plot_source)[names(plot_source) == "group_label"] <- "group"
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
                       autorange  = "reversed",    # searched genes at the top without touching the goi-first ordering that builds `mat`
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
        active_group_col(),   # replaces input$x_axis - plot renders the resolved column
        paste(input$heatmap_terms %||% character(0), collapse = ",")
      )
  })
}

`%||%` <- function(a, b) if (!is.null(a)) a else b
