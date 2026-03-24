# ─────────────────────────────────────────────────────────────────────────────
# This is for the "Explore Data" page for gene/dataset selection and visualisation
#  — ties gene_dataset_selector modal popup, sidebar, explore page, and all plot
# components together via shared `selected_dataset` reactiveVal.
#
# Layout:
#   Side panel:       explore_sidebar (thresholds and plot controls)
#   Left column (9):  Results /dataset table, plots section, dataset-level summaries
#   Right column (3): Project info section
#
# Data flow:
#   gene_dataset_selector → selected_dataset reactiveVal
#   selected_dataset + sidebar thresholds → de_data reactive
#   de_data → plot components
# ─────────────────────────────────────────────────────────────────────────────

box::use(
  shiny[...],
  shinydashboard[valueBoxOutput, renderValueBox, valueBox],
  # shinydashboardPlus[flipBox],
  shinyjs[runjs],
  bslib[...],
  plotly[plotlyOutput, renderPlotly, plot_ly, layout, add_trace],
  DT[DTOutput, renderDT, datatable, dataTableProxy, selectRows],
  app/view/components/dataset_table[dataset_table_ui, dataset_table_server],
  app/view/components/expression_heatmap[heatmap_ui, heatmap_server],
  app/view/components/results_table[results_ui, results_server],
  app/view/components/violin_plot[violin_ui, violin_server],
  app/view/components/volcano_plot[volcano_ui, volcano_server],
  # app/view/components/umap_plot[umap_ui, umap_server],
  app/view/pages/gene_dataset_selector[gene_selector_ui, gene_selector_server],
  app/view/pages/explore_sidebar[sidebar_ui, sidebar_server],
  app/logic/api/api_client[fetch_de_for_terms, fetch_de_multi_dataset, fetch_dataset_stats,
                           fetch_metadata_filter_options, fetch_top_de],
)


#' @export
explorer_ui <- function(id) {
  ns <- NS(id)

  fluidRow(

    # ── LEFT collapsible sidebar (filters)   ───────────────────────────────
    page_sidebar(
      sidebar = sidebar(
        title = div(tags$h6("Data Filters and Threshold Options", style = "margin-top: 0; color: #667eea;"), tags$hr()),
        collapsible = TRUE,
        open = TRUE,
        fillable = TRUE, #FALSE,
        # width = 300,
        # bg = "#f8f9fa",
        # style = "background-color: #f8f9fa; padding: 15px; min-height: 100vh;",

        # Embed sidebar controls from explore_sidebar.R module
        sidebar_ui(ns("filters"))
      ),
      
      # Hero banner for modal popup
      div(
        class = "text-center",
        style = "background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
                   border-radius: 10px; padding: 24px; margin-bottom: 16px; color: white;",
        tags$h5("Find and explore all available datasets by clicking the button below to get started.",
                style = "color: white; margin-bottom: 8px;"),
        gene_selector_ui(ns("gene_selector"))
      ),
      # # Everything below is hidden until a dataset is selected
      # conditionalPanel(
      #   condition = paste0("output['", ns("has_selection"), "']"),
      

      # ── Main content column ─────────────────────────────────────────────
      layout_columns(
        col_widths = c(9, 3),
        gap = "1rem",
        
        # ── Gene search modal trigger ─────────────────────────────────────
        div(
        #   style = "margin-bottom: 10px;",
        #   # gene_selector_ui(ns("gene_selector")),  # render "Search" button

          # ──  Left column - top card: Datasets results table ─────────────
          class = "explore-content",
          card(
            full_screen = TRUE,
            style = "height: auto;",
            card_header(
              div(tags$h4("Dataset Listings", class = "mb-0"))
            ),
            card_body(
              style = "padding-bottom: 0.75rem; height: auto;",
              uiOutput(ns("dataset_listing_ui"))
            )
          ),

          # ── Left column - middle card: Main visualisations ──────────────
          card(
            full_screen = TRUE,
            card_header(
              class = "d-flex justify-content-between align-items-center",
              div(
                tags$h4("Data Visualisation", class = "mb-0"),
                tags$small(class = "text-muted", textOutput(ns("plot_subtitle")))
              ),
              div(
                style = "display: flex; gap: 8px; align-items: center;",
                downloadButton(
                  ns("results-dl_csv"),
                  label = "Export CSV",
                  class = "btn btn-sm btn-outline-secondary"
                ),
                tags$button(
                  class = "btn btn-sm btn-outline-primary",
                  icon("download"),
                  "Export Plot",
                  onclick = sprintf("Shiny.setInputValue('%s', Math.random())", ns("export_plot"))
                )
              )
            ),
            card_body(
              min_height = "500px",

              # Tab navigation for expression and various other plots
              navset_card_tab(
                id = ns("exploration_tabs"),
                
                nav_panel(
                  title = "Expression data",
                  icon = icon("chart-area"),
                  results_ui(ns("results"))
                ),

                nav_panel(
                  title = "Plots",
                  icon = icon("chart-line"),
                  uiOutput(ns("plot_ui"))
                ),

                nav_panel(
                  title = "Compare",
                  icon = icon("table-columns"),
                  uiOutput(ns("compare_ui"))
                )
              )
            )
          ),

          # ── Left column - bottom row of cards: Quick Stats ─────────────
          tags$div(
            style = "margin-top: 20px;",
            # tags$h5("Quick Statistics", class = "mb-3"),
            layout_columns(
              fill = FALSE,
              # col_widths = c(2, 2, 2, 2, 2, 2),
              # full_screen = FALSE,
              style = "text-align:center; font-size:1.2rem;
        background-color: #f9f9f9; padding: 10px; border-radius: 8px;",
              card(valueBoxOutput(ns("box_datasets"))),
              card(valueBoxOutput(ns("box_genes"))),
              card(valueBoxOutput(ns("box_sig_genes"))),
              card(valueBoxOutput(ns("box_cells"))),
              card(valueBoxOutput(ns("box_cell_types"))),
              card(valueBoxOutput(ns("box_samples"))),
              card(valueBoxOutput(ns("box_conditions")))
              # card(valueBoxOutput("selected_datasets_box")),
              # card(valueBoxOutput("total_genes_box")),
              # card(valueBoxOutput("selected_genes_box")),
              # card(valueBoxOutput("total_cells_box")),
              # card(valueBoxOutput("cell_types_box")),
              # card(valueBoxOutput("total_samples_box")),
              # card(valueBoxOutput("qc_rate_box"))
            )
          )
        ),

        # ── Right column - Project info accordion ────────────────────────
        card(
          max_height = "90vh",
          style = "overflow-y: auto;", #"margin-top: 20px; position: sticky; top: 1rem;",
          card_header("Project Information"),
          card_body(
            style = "padding: 0;",
            accordion(
              id = ns("project_accordion"),
              multiple = TRUE,
              open = "Project Details",

              accordion_panel(
                title = "Project Details", icon = icon("info-circle"),
                div(
                  tags$h6("Description", class = "fw-bold mt-2"),
                  uiOutput(ns("project_desc")),

                  tags$h6("Authorship", class = "fw-bold mt-3"),
                  uiOutput(ns("project_citation")),

                  tags$h6("Contact", class = "fw-bold mt-3"),
                  uiOutput(ns("project_contact"))
                )
              ),

              accordion_panel(
                title = "Metadata", icon = icon("table"),
                uiOutput(ns("metadata_table"))
              ),

              accordion_panel(
                title = "Data Quality", icon = icon("chart-bar"),
                uiOutput(ns("qc_metrics"))
              ),

              accordion_panel(
                title = "Quick Actions", icon = icon("bolt"),
                div(
                  class = "d-grid gap-2",
                  tags$button(
                    class = "btn btn-outline-primary",
                    icon("external-link-alt"),
                    "View in Data Nexus",
                    onclick = sprintf("Shiny.setInputValue('%s', Math.random())", ns("datanexus_link"))
                  ),
                  tags$button(
                    class = "btn btn-outline-success mt-2",
                    icon("download"),
                    "Export Data",
                    onclick = sprintf("Shiny.setInputValue('%s', Math.random())", ns("export_data"))
                  )
                )
              )
            )
          )
        )
      )
    ),

    # Spacer at the bottom
    tags$div(style = "height: 30px;")
  )
}


#' @export
explorer_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    dataset_key <- function(df) {
      paste(df$lab_source, df$study_id, sep = "::")
    }

    compare_group_col <- function(df) {
      if ("cell_type" %in% names(df) && any(!is.na(df$cell_type) & nzchar(df$cell_type))) {
        return("cell_type")
      }
      if ("condition_a" %in% names(df) && any(!is.na(df$condition_a) & nzchar(df$condition_a))) {
        return("condition_a")
      }
      "gene_symbol"
    }

    render_compare_volcano <- function(df, dataset_name, padj_thresh, lfc_thresh) {
      plot_df <- df |>
        dplyr::mutate(
          neg_log10p = -log10(pmax(pvalue, 1e-300)),
          sig = dplyr::case_when(
            !is.na(padj) & padj < padj_thresh & log2fc >  lfc_thresh ~ "Up",
            !is.na(padj) & padj < padj_thresh & log2fc < -lfc_thresh ~ "Down",
            TRUE ~ "NS"
          )
        )

      plotly::plot_ly(
        plot_df,
        x = ~log2fc,
        y = ~neg_log10p,
        type = "scatter",
        mode = "markers",
        color = ~sig,
        colors = c(Up = "#C0392B", Down = "#2980B9", NS = "#BDC3C7"),
        text = ~paste0(
          "<b>", gene_symbol, "</b><br>",
          "log2FC: ", round(log2fc, 3), "<br>",
          "padj: ", signif(padj, 3)
        ),
        hoverinfo = "text",
        marker = list(size = 6, opacity = 0.7)
      ) |>
        plotly::layout(
          title = list(text = dataset_name, x = 0.02),
          xaxis = list(title = "log2 Fold Change"),
          yaxis = list(title = "-log10(p-value)"),
          showlegend = FALSE,
          margin = list(t = 50)
        )
    }

    render_compare_violin <- function(df, dataset_name) {
      group_col <- compare_group_col(df)

      plotly::plot_ly(
        df,
        x = df[[group_col]],
        y = df$log2fc,
        type = "violin",
        box = list(visible = TRUE),
        meanline = list(visible = TRUE),
        points = "outliers",
        hovertemplate = paste0(
          "<b>%{x}</b><br>log2FC: %{y:.3f}<extra></extra>"
        )
      ) |>
        plotly::layout(
          title = list(text = dataset_name, x = 0.02),
          xaxis = list(title = group_col, tickangle = -25),
          yaxis = list(title = "log2FC"),
          showlegend = FALSE,
          margin = list(t = 50)
        )
    }

    render_compare_heatmap <- function(df, dataset_name) {
      group_col <- compare_group_col(df)
      agg <- stats::aggregate(
        df$log2fc,
        by = list(gene_symbol = df$gene_symbol, group = df[[group_col]]),
        FUN = mean
      )
      names(agg)[3] <- "log2fc"
      mat <- xtabs(log2fc ~ gene_symbol + group, data = agg)
      mat[is.na(mat)] <- 0

      plotly::plot_ly(
        z = unclass(mat),
        x = colnames(mat),
        y = rownames(mat),
        type = "heatmap",
        colorscale = list(
          c(0, "#2980B9"),
          c(0.5, "#FFFFFF"),
          c(1, "#C0392B")
        ),
        zmid = 0,
        hovertemplate = "%{y} · %{x}<br>log2FC: %{z:.3f}<extra></extra>"
      ) |>
        plotly::layout(
          title = list(text = dataset_name, x = 0.02),
          xaxis = list(title = "", tickangle = -35),
          yaxis = list(title = "", automargin = TRUE),
          margin = list(t = 50)
        )
    }

    load_active_cell_types <- function(lab_source, study_id) {
      opts <- tryCatch(
        fetch_metadata_filter_options(lab_source, study_id),
        error = function(e) NULL
      )

      if (is.null(opts) || !"cell_types" %in% names(opts) || length(opts$cell_types) == 0) {
        return(character(0))
      }

      cts <- opts$cell_types[[1]]
      cts[!is.na(cts) & nzchar(cts)]
    }

    set_active_dataset <- function(current_state, row) {
      current_state$lab_source   <- row$lab_source[1]
      current_state$study_id     <- row$study_id[1]
      current_state$dataset_name <- row$dataset_name[1]
      current_state$omic_type    <- row$omic_type[1]
      current_state$cell_types   <- load_active_cell_types(
        row$lab_source[1],
        row$study_id[1]
      )
      current_state
    }

    # This reactive exposes the currently confirmed dataset set to both the
    # Dataset Listings table and the Compare tab.
    compare_source_rows <- reactive({
      ds <- selected_dataset()
      if (is.null(ds) || is.null(ds$selected_datasets) || nrow(ds$selected_datasets) == 0) {
        return(data.frame())
      }
      ds$selected_datasets
    })

    # This reactive performs the cross-dataset fetch used by the Compare tab.
    # It reruns when selected datasets, search terms, or thresholds change.
    compare_plot_data <- reactive({
      datasets <- compare_source_rows()
      req(nrow(datasets) > 1)

      ds <- selected_dataset()
      plot_type <- sidebar_vals$plot_type()

      if (identical(plot_type, "Heatmap")) {
        rows <- lapply(seq_len(nrow(datasets)), function(i) {
          row <- datasets[i, , drop = FALSE]
          top_df <- tryCatch(
            fetch_top_de(
              lab_source = row$lab_source[1],
              study_id = row$study_id[1],
              n = 50L,
              padj_thresh = sidebar_vals$padj_thresh(),
              lfc_thresh = sidebar_vals$lfc_thresh_min()
            ),
            error = function(e) data.frame()
          )

          if (nrow(top_df) == 0) return(NULL)
          top_df$lab_source <- row$lab_source[1]
          top_df$study_id <- row$study_id[1]
          top_df$dataset_name <- row$dataset_name[1]
          top_df
        })

        return(dplyr::bind_rows(rows))
      }

      fetch_de_multi_dataset(
        gene = ds$genes,
        proteins = ds$proteins,
        dataset_list = datasets,
        padj_thresh = sidebar_vals$padj_thresh(),
        lfc_thresh = sidebar_vals$lfc_thresh_min()
      )
    })
    
    # ── Datasets & sub-modules ────────────────────────────────────────────
    # selected_dataset holds: list(
    #   genes, lab_source, study_id, dataset_name, omic_type,
    #   selected_datasets, cell_types
    # )
    selected_dataset <- reactiveVal(NULL)
    gene_selector_server("gene_selector", selected_dataset)
    sidebar_vals <- sidebar_server("filters", selected_dataset)
    listing_proxy <- DT::dataTableProxy(session$ns("dataset_listing"))

    # ── Plot area UI follows the sidebar plot selector while the active dataset
    # row in Dataset Listings controls which dataset is rendered. ────────────
    # This render block swaps the single-dataset plot module based on the
    # sidebar plot selector while keeping the active dataset row in control.
    output$plot_ui <- renderUI({
      plot_type <- sidebar_vals$plot_type()

      if (is.null(selected_dataset())) {
        return(
          tags$div(
            class = "alert alert-info",
            role = "alert",
            tags$strong("NB: "),
            "Select one or more datasets in the modal, then choose a row in Dataset Listings to render a plot."
          )
        )
      }

      switch(
        plot_type,
        Heatmap = heatmap_ui(session$ns("heatmap")),
        Violin = violin_ui(session$ns("violin")),
        volcano_ui(session$ns("volcano"))
      )
    })
    
    # ── Plot subtitle ────────────────────────────────────────────────────
    output$plot_subtitle <- renderText({
      ds <- selected_dataset()
      if (is.null(ds)) "Select a gene or protein to get started..."
      else {
        n_selected <- if (!is.null(ds$selected_datasets)) nrow(ds$selected_datasets) else 0
        extra <- if (n_selected > 1) paste0(" (+", n_selected - 1, " more)") else ""
        terms <- c(ds$genes, ds$proteins)
        paste0(
          ds$dataset_name, extra, " · ", ds$omic_type, " · search terms: ",
          paste(terms, collapse = ", ")
        )
      }
    })
    
    # ── DE data reactive ─────────────────────────────────────────────────
    # This reactive is the active-dataset query backing the expression table
    # and single-dataset plot modules. It reruns when terms or thresholds change.
    de_data <- reactive({
      ds <- selected_dataset()
      req(ds, length(c(ds$genes, ds$proteins)) > 0)

      fetch_de_for_terms(
        genes       = ds$genes,
        proteins    = ds$proteins,
        lab_source  = ds$lab_source,
        study_id    = ds$study_id,
        padj_thresh = sidebar_vals$padj_thresh(),
        lfc_thresh  = sidebar_vals$lfc_thresh_min()
      )
    })

    # This render block lays out one plot card per selected dataset for the
    # Compare tab, allowing side-by-side visual inspection.
    output$compare_ui <- renderUI({
      datasets <- compare_source_rows()
      if (nrow(datasets) < 2) {
        return(
          tags$div(
            class = "alert alert-info",
            role = "alert",
            tags$strong("NB: "),
            "Select at least two datasets in the modal to compare them side-by-side."
          )
        )
      }

      tags$div(
        style = "display: grid; grid-template-columns: repeat(auto-fit, minmax(420px, 1fr)); gap: 14px;",
        lapply(seq_len(nrow(datasets)), function(i) {
          row <- datasets[i, , drop = FALSE]
          card(
            full_screen = TRUE,
            card_header(
              paste0(row$dataset_name[1], " · ", row$lab_source[1], " · ", row$omic_type[1])
            ),
            card_body(
              plotlyOutput(session$ns(paste0("compare_plot_", i)), height = "420px")
            )
          )
        })
      )
    })

    # This observe keeps the Compare tab plot outputs in sync with the current
    # dataset set and selected plot type by binding one renderPlotly per card.
    observe({
      datasets <- compare_source_rows()
      plot_type <- sidebar_vals$plot_type()
      compare_df <- compare_plot_data()

      if (nrow(datasets) < 2) {
        return(invisible(NULL))
      }

      for (i in seq_len(nrow(datasets))) {
        local({
          idx <- i
          row <- datasets[idx, , drop = FALSE]
          output_id <- paste0("compare_plot_", idx)

          output[[output_id]] <- renderPlotly({
            req(nrow(compare_df) > 0)
            current_key <- dataset_key(row)
            plot_df <- compare_df[dataset_key(compare_df) == current_key, , drop = FALSE]

            validate(
              need(
                nrow(plot_df) > 0,
                "No rows matched the current search terms and thresholds for this dataset."
              )
            )

            switch(
              plot_type,
              Heatmap = render_compare_heatmap(plot_df, row$dataset_name[1]),
              Violin = render_compare_violin(plot_df, row$dataset_name[1]),
              render_compare_volcano(
                plot_df,
                row$dataset_name[1],
                sidebar_vals$padj_thresh(),
                sidebar_vals$lfc_thresh_min()
              )
            )
          })
        })
      }
    })
    
    # ── Dataset Listing section ───────────────────────────────────────────
    output$dataset_listing_ui <- renderUI({
      ds <- selected_dataset()
      if (is.null(ds) || is.null(ds$selected_datasets) || nrow(ds$selected_datasets) == 0) {
        return(tagList(
          tags$div(
            class = "alert alert-info",
            role = "alert",
            tags$strong("NB: "),
            "Use the search modal to select one or more datasets."
          ),
          tags$div(
            style = "border: 1px solid #ddd; padding: 15px; border-radius: 8px; background: white; box-shadow: 0 2px 4px rgba(0,0,0,0.1); margin: 10px 0;",
            tags$h5(style = "margin-top: 0; color: #2196F3;", "Default Behaviour"),
            tags$p(
              style = "margin-bottom: 0;",
              "Choose one or more datasets in the modal, then click a row here to make it the active dataset for plots and summaries."
            )
          )
        ))
      }

      DTOutput(session$ns("dataset_listing"))
    })

    output$dataset_listing <- renderDT({
      ds <- selected_dataset()
      req(ds, !is.null(ds$selected_datasets), nrow(ds$selected_datasets) > 0)

      rows <- ds$selected_datasets
      display <- rows[, intersect(
        c("lab_source", "dataset_name", "omic_type", "matched_terms",
          "total_features", "n_sig_features", "total_samples", "total_cells",
          "n_cell_types", "n_conditions"),
        names(rows)
      ), drop = FALSE]

      names(display) <- c(
        "Lab", "Dataset", "Modality", "Matched terms",
        "Total features", "Total significant", "Samples", "Cells",
        "Cell types", "Conditions"
      )[seq_along(display)]

      datatable(
        display,
        rownames = FALSE,
        selection = "single",
        options = list(
          pageLength = min(8L, nrow(display)),
          paging = nrow(display) > 8,
          info = nrow(display) > 8,
          dom = if (nrow(display) > 8) "ftip" else "t",
          scrollX = TRUE,
          autoWidth = TRUE
        )
      )
    })

    # Keep the DT row highlight aligned with whichever dataset is currently active.
    observeEvent(selected_dataset(), {
      ds <- selected_dataset()
      req(ds, !is.null(ds$selected_datasets), nrow(ds$selected_datasets) > 0)
      active_idx <- which(
        ds$selected_datasets$lab_source == ds$lab_source &
        ds$selected_datasets$study_id == ds$study_id
      )[1]
      if (length(active_idx) == 0 || is.na(active_idx)) {
        active_idx <- 1L
      }
      DT::selectRows(listing_proxy, active_idx)
    }, ignoreInit = TRUE)

    # ── Clicking a row in "Dataset Listings" switches an active dataset for plots, stats, and cell-type options
    observeEvent(input$dataset_listing_rows_selected, {
      row_idx <- input$dataset_listing_rows_selected
      req(length(row_idx) == 1)

      current <- selected_dataset()
      req(current, !is.null(current$selected_datasets), nrow(current$selected_datasets) >= row_idx)

      row <- current$selected_datasets[row_idx, , drop = FALSE]
      if (!is.null(current$lab_source) &&
          !is.null(current$study_id) &&
          current$lab_source == row$lab_source[1] &&
          current$study_id == row$study_id[1]) {
        return(invisible(NULL))
      }

      selected_dataset(set_active_dataset(current, row))
    }, ignoreInit = TRUE)
    
    # ── Results table & plot servers ──────────────────────────────────────
    results_server("results",  de_data) # results table displaying expression data
    volcano_server("volcano",  de_data,
                   padj_thresh = sidebar_vals$padj_thresh,
                   lfc_thresh  = sidebar_vals$lfc_thresh_min,
                   gene        = reactive({
                     ds <- selected_dataset()
                     terms <- c(ds$genes, ds$proteins)
                     if (is.null(ds) || length(terms) == 0) return(NULL)
                     terms[1]
                   }))
    heatmap_server("heatmap",  selected_dataset,
                   padj_thresh = sidebar_vals$padj_thresh,
                   lfc_thresh  = sidebar_vals$lfc_thresh_min,
                   n_genes     = reactive(50L))
    violin_server("violin",    de_data)
    
    # ── Value boxes (tied to dataset_stats) ─────────────────────────────
    # stats_row <- reactive({
    #   ds <- selected_dataset()
    #   req(ds)
    #   fetch_dataset_stats(registry_con(), lab_source = ds$lab_source,
    #                       study_id = ds$study_id)
    # })
    
    # output$box_datasets   <- renderValueBox(valueBox(
    #   length(selected_dataset()$genes %||% 0), "Gene(s) selected",
    #   # icon = icon("dna"),
    #   color = "purple"))
    # 
    # output$box_genes      <- renderValueBox(valueBox(
    #   stats_row()$total_features %||% "—", "Total features",
    #   # icon = icon("list"),
    #   color = "blue"))
    # 
    # output$box_sig_genes  <- renderValueBox(valueBox(
    #   stats_row()$n_sig_features %||% "—", "Significant",
    #   # icon = icon("star"),
    #   color = "red"))
    # 
    # output$box_cells      <- renderValueBox(valueBox(
    #   stats_row()$total_cells %||% "—", "Total cells",
    #   icon = icon("circle-nodes"),
    #   color = "teal"))
    # 
    # output$box_cell_types <- renderValueBox(valueBox(
    #   stats_row()$n_cell_types %||% "—", "Cell types",
    #   icon = icon("tags"),
    #   color = "olive"))
    # 
    # output$box_samples    <- renderValueBox(valueBox(
    #   stats_row()$total_samples %||% "—", "Samples",
    #   icon = icon("vials"),
    #   color = "navy"))
    # 
    # output$box_conditions <- renderValueBox(valueBox(
    #   stats_row()$n_conditions %||% "—", "Conditions",
    #   # icon = icon("flask"),
    #   color = "maroon"))
    stats_row <- reactive({
      ds <- selected_dataset()
      if (is.null(ds)) return(data.frame())   # return empty, don't req() here
      tryCatch(
        fetch_dataset_stats(
          lab_source = ds$lab_source,
          study_id = ds$study_id
        ),
        error = function(e) data.frame()
      )
    })
    
    output$box_datasets <- renderValueBox({
      ds    <- selected_dataset()
      count <- if (!is.null(ds) && !is.null(ds$selected_datasets)) nrow(ds$selected_datasets) else "—"
      valueBox(count, "Datasets selected", color = "purple")
    })
    
    output$box_genes <- renderValueBox({
      row <- stats_row()   # stats_row already has req(ds) inside it
      val <- if (nrow(row) > 0) row$total_features[1] %||% "—" else "—"
      valueBox(val, "Total features", color = "blue")
    })
    
    output$box_sig_genes <- renderValueBox({
      row <- stats_row()
      val <- if (nrow(row) > 0) row$n_sig_features[1] %||% "—" else "—"
      valueBox(val, "Significant (padj<0.05)", color = "red")
    })
    
    output$box_cells <- renderValueBox({
      row <- stats_row()
      val <- if (nrow(row) > 0) row$total_cells[1] %||% "—" else "—"
      valueBox(val, "Total cells", icon = icon("circle-nodes"), color = "teal")
    })
    
    output$box_cell_types <- renderValueBox({
      row <- stats_row()
      val <- if (nrow(row) > 0) row$n_cell_types[1] %||% "—" else "—"
      valueBox(val, "Cell types", icon = icon("tags"), color = "olive")
    })
    
    output$box_samples <- renderValueBox({
      row <- stats_row()
      val <- if (nrow(row) > 0) row$total_samples[1] %||% "—" else "—"
      valueBox(val, "Samples", icon = icon("vials"), color = "navy")
    })
    
    output$box_conditions <- renderValueBox({
      row <- stats_row()
      val <- if (nrow(row) > 0) row$n_conditions[1] %||% "—" else "—"
      valueBox(val, "Conditions", color = "maroon")
    })
    
    # observeEvent(input$go_explore, {
    #   # example navigation
    #   # runjs("window.location.hash = '#Explore Data'")
    # })

  })
}
# ── Small null helper used by value boxes ──────────────────────────────────
`%||%` <- function(a, b) if (!is.null(a) && !is.na(a)) a else b
