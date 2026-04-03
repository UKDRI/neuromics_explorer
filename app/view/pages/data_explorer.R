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
  htmlwidgets[JS],
  app/view/components/dataset_table[dataset_table_ui, dataset_table_server],
  app/view/components/expression_heatmap[heatmap_ui, heatmap_server],
  app/view/components/results_table[results_ui, results_server],
  app/view/components/violin_plot[violin_ui, violin_server],
  app/view/components/volcano_plot[volcano_ui, volcano_server],
  # app/view/components/umap_plot[umap_ui, umap_server],
  app/view/pages/gene_dataset_selector[gene_selector_ui, gene_selector_server],
  app/view/pages/explore_sidebar[sidebar_ui, sidebar_server],
  app/logic/api/api_client[fetch_dataset_expression, fetch_expression_multi_dataset,
                           fetch_metadata_filter_options],
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
                tags$h4("Data Visualisation", class = "mb-0")
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
              tags$div(
                class = "alert alert-info",
                role = "alert",
                style = "margin-bottom: 12px;",
                tags$strong("NB: "),
                "Select one dataset row in **Dataset Listings** above to drive the Expression and Plot tab. Select two or more rows to activate **Compare** tab."
              ),

              # Tab navigation for expression and various other plots
              navset_card_tab(
                id = ns("exploration_tabs"),
                
                nav_panel(
                  title = "Expression data",
                  icon = icon("chart-area"),
                  uiOutput(ns("active_dataset_banner")),
                  results_ui(ns("results"))
                ),

                nav_panel(
                  title = "Plot",
                  icon = icon("chart-line"),
                  uiOutput(ns("active_dataset_banner_plot")),
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

    sum_numeric <- function(x) {
      values <- suppressWarnings(as.numeric(x))
      values <- values[!is.na(values)]
      if (length(values) == 0) return(NA_real_)
      sum(values)
    }

    flatten_json_values <- function(values) {
      unique(unlist(lapply(values, function(value) {
        if (is.null(value) || is.na(value) || !nzchar(value)) return(character(0))
        parsed <- tryCatch(jsonlite::fromJSON(value), error = function(e) character(0))
        parsed <- as.character(parsed)
        parsed[!is.na(parsed) & nzchar(parsed) & parsed != "null"]
      }), use.names = FALSE))
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
        plotly::add_segments(
          x = -max(abs(plot_df$log2fc), na.rm = TRUE) * 1.1,
          xend = max(abs(plot_df$log2fc), na.rm = TRUE) * 1.1,
          y = -log10(padj_thresh),
          yend = -log10(padj_thresh),
          line = list(dash = "dot", color = "#7F8C8D", width = 1),
          showlegend = FALSE,
          hoverinfo = "skip"
        ) |>
        plotly::add_segments(
          x = lfc_thresh, xend = lfc_thresh,
          y = 0, yend = max(plot_df$neg_log10p, na.rm = TRUE) * 1.05,
          line = list(dash = "dot", color = "#7F8C8D", width = 1),
          showlegend = FALSE,
          hoverinfo = "skip"
        ) |>
        plotly::add_segments(
          x = -lfc_thresh, xend = -lfc_thresh,
          y = 0, yend = max(plot_df$neg_log10p, na.rm = TRUE) * 1.05,
          line = list(dash = "dot", color = "#7F8C8D", width = 1),
          showlegend = FALSE,
          hoverinfo = "skip"
        ) |>
        plotly::layout(
          title = list(text = dataset_name, x = 0.02),
          xaxis = list(title = "log2 Fold Change"),
          yaxis = list(title = "-log10(p-value)"),
          showlegend = FALSE,
          margin = list(t = 50)
        )
    }

    render_compare_violin <- function(df, dataset_name, padj_thresh, lfc_thresh) {
      group_col <- compare_group_col(df)
      plot_df <- df |>
        dplyr::mutate(
          sig = dplyr::case_when(
            !is.na(padj) & padj < padj_thresh & log2fc >  lfc_thresh ~ "Up",
            !is.na(padj) & padj < padj_thresh & log2fc < -lfc_thresh ~ "Down",
            TRUE ~ "NS"
          )
        )

      plotly::plot_ly(
        plot_df,
        x = plot_df[[group_col]],
        y = plot_df$log2fc,
        type = "violin",
        box = list(visible = TRUE),
        meanline = list(visible = TRUE),
        points = FALSE,
        hovertemplate = paste0(
          "<b>%{x}</b><br>log2FC: %{y:.3f}<extra></extra>"
        )
      ) |>
        plotly::add_markers(
          data = plot_df,
          x = plot_df[[group_col]],
          y = plot_df$log2fc,
          color = ~sig,
          colors = c(Up = "#C0392B", Down = "#2980B9", NS = "#BDC3C7"),
          marker = list(size = 6, opacity = 0.65),
          text = ~paste0(
            "<b>", gene_symbol, "</b><br>",
            "log2FC: ", round(log2fc, 3), "<br>",
            "padj: ", signif(padj, 3), "<br>",
            "class: ", sig
          ),
          hoverinfo = "text",
          inherit = FALSE
        ) |>
        plotly::layout(
          title = list(text = dataset_name, x = 0.02),
          xaxis = list(title = group_col, tickangle = -25),
          yaxis = list(title = "log2FC"),
          legend = list(title = list(text = "Significance"), orientation = "h",
                        y = -0.15),
          showlegend = TRUE,
          margin = list(t = 50)
        )
    }

    render_compare_heatmap <- function(df, dataset_name, padj_thresh, lfc_thresh) {
      group_col <- compare_group_col(df)
      ranked_genes <- df |>
        dplyr::mutate(
          sig = dplyr::case_when(
            !is.na(padj) & padj < padj_thresh & log2fc >  lfc_thresh ~ "Up",
            !is.na(padj) & padj < padj_thresh & log2fc < -lfc_thresh ~ "Down",
            TRUE ~ "NS"
          ),
          rank_bucket = ifelse(sig == "NS", 1L, 0L),
          abs_lfc = abs(log2fc)
        ) |>
        dplyr::arrange(rank_bucket, dplyr::desc(abs_lfc), padj)
      ranked_genes <- unique(ranked_genes$gene_symbol)
      ranked_genes <- ranked_genes[seq_len(min(50L, length(ranked_genes)))]

      df <- df[df$gene_symbol %in% ranked_genes, , drop = FALSE]
      agg <- stats::aggregate(
        df$log2fc,
        by = list(gene_symbol = df$gene_symbol, group = df[[group_col]]),
        FUN = mean
      )
      names(agg)[3] <- "log2fc"
      mat <- stats::xtabs(log2fc ~ gene_symbol + group, data = agg)
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

    # This reactiveVal tracks the current row selection inside Dataset Listings.
    # The same selected row set powers the combined table, compare tab, and
    # aggregate value boxes.
    listing_selection <- reactiveVal(integer())
    last_dataset_keys <- reactiveVal(character())

    # This reactive exposes the currently selected dataset rows from the
    # Dataset Listings table.
    compare_source_rows <- reactive({
      ds <- selected_dataset()
      if (is.null(ds) || is.null(ds$selected_datasets) || nrow(ds$selected_datasets) == 0) {
        return(data.frame())
      }

      idx <- listing_selection()
      idx <- idx[idx >= 1 & idx <= nrow(ds$selected_datasets)]
      if (length(idx) == 0) {
        return(ds$selected_datasets[0, , drop = FALSE])
      }

      ds$selected_datasets[idx, , drop = FALSE]
    })

    # This reactive performs the cross-dataset fetch used by the Compare tab.
    # It reruns when selected datasets, search terms, or thresholds change.
    compare_plot_data <- reactive({
      datasets <- compare_source_rows()
      req(nrow(datasets) > 1)

      fetch_expression_multi_dataset(
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
    
    # ── Active dataset banners ────────────────────────────────────────────────────
    output$active_dataset_banner <- renderUI({
      ds <- selected_dataset()
      if (is.null(ds)) return(NULL)
      tags$div(
        class = "alert alert-secondary",
        role = "alert",
        style = "margin-bottom: 12px;",
        tags$strong("Expression data is showing: "),
        paste(ds$dataset_name, "·", ds$lab_source, "·", ds$omic_type)
      )
    })

    output$active_dataset_banner_plot <- renderUI({
      ds <- selected_dataset()
      if (is.null(ds)) return(NULL)
      tags$div(
        class = "alert alert-secondary",
        role = "alert",
        style = "margin-bottom: 12px;",
        tags$strong("Plot is showing: "),
        paste(ds$dataset_name, "·", ds$lab_source, "·", ds$omic_type)
      )
    })

    # This reactive fetches the combined DE / expression rows for every dataset
    # currently selected in Dataset Listings. It powers the Expression data tab.
    expression_data <- reactive({
      ds <- selected_dataset()
      datasets <- compare_source_rows()
      req(ds, nrow(datasets) > 0, length(c(ds$genes, ds$proteins)) > 0)

      if (nrow(datasets) == 1) {
        row <- datasets[1, , drop = FALSE]
        single_df <- fetch_dataset_expression(
          lab_source = row$lab_source[1],
          study_id = row$study_id[1],
          padj_thresh = sidebar_vals$padj_thresh(),
          lfc_thresh = sidebar_vals$lfc_thresh_min()
        )

        if (nrow(single_df) == 0) {
          return(single_df)
        }

        single_df$lab_source <- row$lab_source[1]
        single_df$study_id <- row$study_id[1]
        single_df$dataset_name <- row$dataset_name[1]
        single_df$omic_type <- row$omic_type[1]
        return(single_df)
      }

      fetch_expression_multi_dataset(
        dataset_list = datasets,
        padj_thresh = sidebar_vals$padj_thresh(),
        lfc_thresh = sidebar_vals$lfc_thresh_min()
      )
    })

    # ── DE data reactive ─────────────────────────────────────────────────
    # This reactive is the active-dataset query backing the single-dataset
    # Plots tab. It reruns when the active row, terms, or thresholds change.
    de_data <- reactive({
      ds <- selected_dataset()
      req(ds, length(c(ds$genes, ds$proteins)) > 0)

      fetch_dataset_expression(
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
            "Select at least two rows in Dataset Listings to compare those datasets side-by-side."
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
                "No expression rows are available for this dataset."
              )
            )

            switch(
              plot_type,
              Heatmap = render_compare_heatmap(
                plot_df, row$dataset_name[1],
                sidebar_vals$padj_thresh(),
                sidebar_vals$lfc_thresh_min()
              ),
              Violin = render_compare_violin(
                plot_df, row$dataset_name[1],
                sidebar_vals$padj_thresh(),
                sidebar_vals$lfc_thresh_min()
              ),
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
              "Choose one or more datasets in the modal, then use row selection here to drive the combined table, Compare tab, and the active dataset shown in Plots."
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
        selection = "multiple",
        callback = htmlwidgets::JS(
          sprintf(
            "table.on('click.dt', 'tbody tr', function() {
               var idx = table.row(this).index();
               if (idx !== undefined) {
                 Shiny.setInputValue('%s', idx + 1, {priority: 'event'});
               }
             });",
            session$ns("dataset_listing_last_clicked")
          )
        ),
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

    # This observe initialises the DT row selection when a new modal-confirmed
    # dataset set arrives, selecting all confirmed rows by default.
    observe({
      ds <- selected_dataset()
      req(ds, !is.null(ds$selected_datasets), nrow(ds$selected_datasets) > 0)

      dataset_keys <- dataset_key(ds$selected_datasets)
      if (!identical(dataset_keys, last_dataset_keys())) {
        last_dataset_keys(dataset_keys)
        selected_rows <- seq_len(nrow(ds$selected_datasets))
        listing_selection(selected_rows)
        DT::selectRows(listing_proxy, selected_rows)
      }
    })

    # This observeEvent keeps the shared row-selection state aligned with the
    # DT widget so Expression/Compare/value boxes all follow the same rows.
    observeEvent(input$dataset_listing_rows_selected, {
      row_idx <- sort(unique(input$dataset_listing_rows_selected %||% integer()))
      listing_selection(row_idx)

      if (length(row_idx) == 0) {
        return(invisible(NULL))
      }

      current <- selected_dataset()
      req(current, !is.null(current$selected_datasets), nrow(current$selected_datasets) >= row_idx[1])

      active_idx <- which(
        current$selected_datasets$lab_source == current$lab_source &
        current$selected_datasets$study_id == current$study_id
      )[1]

      if (length(active_idx) == 0 || is.na(active_idx) || !(active_idx %in% row_idx)) {
        row <- current$selected_datasets[row_idx[1], , drop = FALSE]
        selected_dataset(set_active_dataset(current, row))
      }
    }, ignoreInit = TRUE)

    # This observeEvent promotes the most recently clicked DT row to the active
    # dataset that drives the single-dataset Plots tab and cell-type filter UI.
    observeEvent(input$dataset_listing_last_clicked, {
      row_idx <- input$dataset_listing_last_clicked
      req(length(row_idx) == 1)
      req(row_idx %in% (input$dataset_listing_rows_selected %||% integer()))

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
    results_server("results", expression_data)
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
    violin_server("violin",    de_data,
                  padj_thresh = sidebar_vals$padj_thresh,
                  lfc_thresh  = sidebar_vals$lfc_thresh_min)
    
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
      rows <- compare_source_rows()
      if (nrow(rows) == 0) return(list())

      list(
        dataset_count = nrow(rows),
        total_features = sum_numeric(rows$total_features),
        n_sig_features = sum_numeric(rows$n_sig_features),
        total_cells = sum_numeric(rows$total_cells),
        total_samples = sum_numeric(rows$total_samples),
        n_cell_types = length(flatten_json_values(rows$cell_types_json)),
        n_conditions = length(flatten_json_values(rows$conditions_json))
      )
    })
    
    output$box_datasets <- renderValueBox({
      row <- stats_row()
      count <- if (length(row) > 0) row$dataset_count else "—"
      valueBox(count, "Datasets selected", color = "purple")
    })
    
    output$box_genes <- renderValueBox({
      row <- stats_row()
      val <- if (length(row) > 0) row$total_features %||% "—" else "—"
      valueBox(val, "Total features", color = "blue")
    })
    
    output$box_sig_genes <- renderValueBox({
      row <- stats_row()
      val <- if (length(row) > 0) row$n_sig_features %||% "—" else "—"
      valueBox(val, "Significant (padj<0.05)", color = "red")
    })
    
    output$box_cells <- renderValueBox({
      row <- stats_row()
      val <- if (length(row) > 0) row$total_cells %||% "—" else "—"
      valueBox(val, "Total cells", icon = icon("circle-nodes"), color = "teal")
    })
    
    output$box_cell_types <- renderValueBox({
      row <- stats_row()
      val <- if (length(row) > 0) row$n_cell_types %||% "—" else "—"
      valueBox(val, "Cell types", icon = icon("tags"), color = "olive")
    })
    
    output$box_samples <- renderValueBox({
      row <- stats_row()
      val <- if (length(row) > 0) row$total_samples %||% "—" else "—"
      valueBox(val, "Samples", icon = icon("vials"), color = "navy")
    })
    
    output$box_conditions <- renderValueBox({
      row <- stats_row()
      val <- if (length(row) > 0) row$n_conditions %||% "—" else "—"
      valueBox(val, "Conditions", color = "maroon")
    })
    
    # observeEvent(input$go_explore, {
    #   # example navigation
    #   # runjs("window.location.hash = '#Explore Data'")
    # })

  })
}
# ── Small null helper used by value boxes ──────────────────────────────────
`%||%` <- function(a, b) {
  if (is.null(a) || length(a) == 0) return(b)
  if (length(a) == 1 && is.na(a)) return(b)
  if (length(a) == 1 && identical(a, "")) return(b)
  a
}
