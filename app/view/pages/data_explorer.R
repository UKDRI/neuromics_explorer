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
#   selected_dataset + sidebar controls → endpoint-specific reactives
#   table / volcano / grouped-summary endpoints → plot components
# ─────────────────────────────────────────────────────────────────────────────

box::use(
  shiny[..., bindCache],
  shinycssloaders[withSpinner],
  shinydashboard[valueBoxOutput, renderValueBox, valueBox],
  shinyjs[runjs],
  bslib[...],
  plotly[plotlyOutput, renderPlotly, plot_ly, layout, add_trace],
  DT[DTOutput, renderDT, datatable, dataTableProxy, selectRows],
  htmlwidgets[JS],
  app/view/components/dataset_table[dataset_table_ui, dataset_table_server],
  app/view/components/expression_heatmap[heatmap_ui, heatmap_server],
  app/view/components/results_table[results_ui, results_server],
  app/view/components/umap_plot[umap_ui, umap_server],
  app/view/components/violin_plot[violin_ui, violin_server],
  app/view/components/volcano_plot[volcano_ui, volcano_server],
  app/view/components/feature_scatter_plot[feature_scatter_ui, feature_scatter_server],
  app/view/components/histogram_plot[histogram_ui, histogram_server],
  app/view/components/dots_plot[dots_ui, dots_server],
  app/view/components/highest_expr_plot[highest_expr_ui, highest_expr_server],
  app/view/components/signature_explorer[signature_explorer_ui, signature_explorer_server],
  app/view/components/signature_adapters/drug_panel_adapter[drug_rank_adapter],
  app/view/pages/gene_dataset_selector[gene_selector_ui, gene_selector_server, parse_json_text],
  app/view/pages/explore_sidebar[sidebar_ui, sidebar_server],
  app/logic/api/api_client[fetch_all_datasets, fetch_datasets_for_terms, fetch_expression_table, fetch_expression_volcano,
                           fetch_expression_multi_dataset, fetch_expression_histogram,
                           fetch_expression_groups, fetch_expression_goi,
                           fetch_dataset_embeddings, fetch_top_de,
                           fetch_metadata_filter_options],
  utils[head, tail],
)


#' @export
explorer_ui <- function(id) {
  ns <- NS(id)

  # Two-column layout: sidebar (320px) + main content (flexible)
  # Uses CSS grid for natural page scrolling (sidebar moves with page, not sticky)
  div(
    style = "display: grid; grid-template-columns: 320px 1fr; gap: 2rem; padding: 1rem;",
    
    # ── LEFT COLUMN: SIDEBAR ──────────────────────────────────────────
    div(
      class = "explore-sidebar",
      sidebar(
        title = div(tags$h6("Data Filters and Threshold Options", class = "sidebar-heading"), tags$hr()),
        collapsible = TRUE,
        open = TRUE,
        fillable = FALSE,
        # Embed sidebar controls from explore_sidebar.R module
        sidebar_ui(ns("filters"))
      )
    ),
    
    # ── RIGHT COLUMN: MAIN CONTENT ────────────────────────────────────
    div(
      # Hero banner for modal popup
      div(
        class = "hero-banner",
        tags$h5("Find and explore all available datasets by clicking the button below to get started.",
                style = "margin-bottom: 8px;"),
        gene_selector_ui(ns("gene_selector"))
        # ,
        # uiOutput(ns("shareable_link_ui"))
      ),
      tags$br(),
      # # Everything below is hidden until a dataset is selected
      # conditionalPanel(
      #   condition = paste0("output['", ns("has_selection"), "']"),

      # ── Main content:  ─────────
      layout_columns(
        # col_widths = c(9, 3), #Originally for 'Two-column layout (75% left, 25% right)' to host project info from Nexus
        gap = "1rem",
        
        # ── LEFT INNER COLUMN: Main content ────────────────────────────
        tags$div(
          class = "explore-content",
          # ──  Top card: Datasets results table ──────────────────────
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

          # ──  Middle card: Main visualisations ──────────────────────
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
                  uiOutput(ns("compare_controls_ui")),
                  uiOutput(ns("compare_ui"))
                ),

                nav_panel(
                  title = "Gene-Drug Explorer",
                  icon = icon("pills"),
                  signature_explorer_ui(ns("gde"),
                  drug_rank_adapter)
                )
              )
            )
          ),

          # ──  Bottom row of cards: Quick Stats ──────────────────
          tags$div(
            style = "margin-top: 20px;",
            # layout_columns(
            #   fill = FALSE,
            #   style = "text-align:center; font-size:1.2rem; background-color: #f9f9f9; padding: 10px; border-radius: 8px;",
            #   card(valueBoxOutput(ns("box_datasets"))),
            #   card(valueBoxOutput(ns("box_genes"))),
            #   card(valueBoxOutput(ns("box_sig_genes"))),
            #   card(valueBoxOutput(ns("box_cells"))),
            #   card(valueBoxOutput(ns("box_cell_types"))),
            #   card(valueBoxOutput(ns("box_samples"))),
            #   card(valueBoxOutput(ns("box_conditions")))
            # )
          )
        ),

        # # ── RIGHT INNER COLUMN: Project info accordion ──────────────
        # card(
        #   max_height = "90vh",
        #   style = "overflow-y: auto;",
        #   card_header("Project Information"),
        #   card_body(
        #     style = "padding: 0;",
        #     accordion(
        #       id = ns("project_accordion"),
        #       multiple = TRUE,
        #       open = "Project Details",

        #       accordion_panel(
        #         title = "Project Details", icon = icon("info-circle"),
        #         div(
        #           tags$h6("Description", class = "fw-bold mt-2"),
        #           uiOutput(ns("project_desc")),

        #           tags$h6("Authorship", class = "fw-bold mt-3"),
        #           uiOutput(ns("project_citation")),

        #           tags$h6("Contact", class = "fw-bold mt-3"),
        #           uiOutput(ns("project_contact"))
        #         )
        #       ),

        #       accordion_panel(
        #         title = "Metadata", icon = icon("table"),
        #         uiOutput(ns("metadata_table"))
        #       ),

        #       accordion_panel(
        #         title = "Data Quality", icon = icon("chart-bar"),
        #         uiOutput(ns("qc_metrics"))
        #       ),

        #       accordion_panel(
        #         title = "Quick Actions", icon = icon("bolt"),
        #         div(
        #           class = "d-grid gap-2",
        #           tags$button(
        #             class = "btn btn-outline-primary",
        #             icon("external-link-alt"),
        #             "View in Data Nexus",
        #             onclick = sprintf("Shiny.setInputValue('%s', Math.random())", ns("datanexus_link"))
        #           ),
        #           tags$button(
        #             class = "btn btn-outline-success mt-2",
        #             icon("download"),
        #             "Export Data",
        #             onclick = sprintf("Shiny.setInputValue('%s', Math.random())", ns("export_data"))
        #           )
        #         )
        #       )
        #     )
        #   )
        # )
      )
    )
  )
}


#' @export
explorer_server <- function(id, initial_link = reactive(NULL)) {
  moduleServer(id, function(input, output, session) {
    # ── Helpers ────────────────────────────────────────────────────────────
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

    clean_terms <- function(values, max_terms = 150L) {
      if (is.null(values) || length(values) == 0) return(character(0))
      values <- unique(trimws(as.character(values)))
      values <- values[!is.na(values) & nzchar(values)]
      utils::head(values, max_terms)
    }

    # ── Build shareable link based on selected dataset and search terms ────
    build_share_query <- function(ds) {
      if (is.null(ds)) return("?page=explore")

      gene_terms <- clean_terms(ds$genes %||% character(0))
      protein_terms <- clean_terms(ds$proteins %||% character(0))

      query_parts <- c(
        "page=explore",
        paste0("lab=", utils::URLencode(ds$lab_source, reserved = TRUE)),
        paste0("study=", utils::URLencode(as.character(ds$study_id), reserved = TRUE)),
        vapply(
          gene_terms,
          function(term) paste0("gene=", utils::URLencode(term, reserved = TRUE)),
          character(1)
        ),
        vapply(
          protein_terms,
          function(term) paste0("protein=", utils::URLencode(term, reserved = TRUE)),
          character(1)
        )
      )

      paste0("?", paste(query_parts, collapse = "&"))
    }

    # Reconstructs the `selected_dataset` payload from URL query parameters for sharing
    # bookmarking, or linking URL (e.g. neuromics-explorer.ukdri.ac.uk/?page=explore&lab=diaz&study=5&gene=GAPDH)
    build_link_selected_payload <- function(params) {

      # Fetch master dataset catalogue to use as lookup table (ie for dropdown filters, URL bookmarking lookup etc.)
      all_rows <- tryCatch(
        fetch_all_datasets(),
        error = function(e) data.frame()
      )
      if (nrow(all_rows) == 0) return(NULL)

      # Filter catalogue rows to specific dataset via URL params using case-insensitive matching.
      # Below if-else statement can cause partial URLs (e.g. no lab, study only) to still work as study_id is the minimum required identifier.
      rows <- all_rows
      if (!is.null(params$lab_source) && nzchar(params$lab_source)) {
        rows <- rows[tolower(rows$lab_source) == tolower(params$lab_source), , drop = FALSE]
      }
      # TODO: allow all datasets to be listed w just genes/proteins (ie ?page=explore&study=1&gene=APOE; ?page=explore&gene=APOE)
      if (!is.null(params$study_id) && !is.na(params$study_id)) {
        rows <- rows[rows$study_id == params$study_id, , drop = FALSE]
      } else {
        return(NULL)
      }

      if (nrow(rows) == 0) return(NULL)

      # Verify which genes actually exist in this dataset (as normally done via modal), and cross-reference hits with url params
      verified_genes <- character(0)
      verified_proteins <- character(0)
      if (length(params$genes) > 0 || length(params$proteins) > 0) {
        hits <- tryCatch(
          fetch_datasets_for_terms(
            genes      = params$genes,
            proteins   = params$proteins,
            omic_type  = NULL,
            lab_source = rows$lab_source[1]
          ),
          error = function(e) data.frame()
        )
        if (nrow(hits) > 0) {
          matched <- hits[hits$study_id == rows$study_id[1], , drop = FALSE]
          verified_genes    <- intersect(params$genes,    unique(matched$gene_symbol))
          verified_proteins <- intersect(params$proteins, unique(unlist(lapply(matched$protein_id, parse_json_text))))
        }
      }
      rows$matched_genes <- if (length(verified_genes) > 0) {
        paste(verified_genes, collapse = ", ")
      } else {
        ""
      }
      rows$matched_proteins <- if (length(verified_proteins) > 0) {
        paste(verified_proteins, collapse = ", ")
      } else {
        ""
      }

      # Active row in dataset listings table
      active_row <- rows[1, , drop = FALSE]
      opts <- tryCatch(
        fetch_metadata_filter_options(active_row$lab_source[1], active_row$study_id[1]),
        error = function(e) list()
      )

      # Returns list matching shape produced by gene_dataset_selector modal
      list(
        genes             = params$genes,
        proteins          = params$proteins,
        lab_source        = active_row$lab_source[1],
        study_id          = active_row$study_id[1],
        dataset_name      = active_row$dataset_name[1],
        omic_type         = active_row$omic_type[1],
        selected_datasets = rows,
        cell_types        = opts$cell_types %||% character(0),  #load_active_cell_types(...)
        available_assays  = opts$available_assays,
        available_reductions = opts$available_reductions
      )
    }

    # ── Helpers to create grouping labels (column names) for plots and tables ─────────────────────────
    
    # Checks presence of column(s) in df while checking for non-empty values
    has_values <- function(df, col, df_names = names(df)) {
      col %in% df_names &&
        any(!is.na(df[[col]]) & nzchar(trimws(as.character(df[[col]]))))
    }

    has_multiple_values <- function(df, col, df_names = names(df)) {
      if (!col %in% df_names) return(FALSE)
      values <- as.character(df[[col]])
      values <- values[!is.na(values) & nzchar(trimws(values))]
      length(unique(values)) > 1
    }

    group_label <- function(col_name) {
      labels <- c(
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
        cell_id = "Cell ID",
        gene_symbol = "Gene symbol",
        obs = "Observation"
      )
      labels[[col_name]] %||% col_name
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

    # Builds DE category column if 'de_category' column is missing
    build_de_category <- function(df, padj_thresh, lfc_thresh, df_names = names(df)) {
      if (has_values(df, "de_category", df_names)) {
        values <- as.character(df$de_category)
        values[is.na(values) | !nzchar(values)] <- "unlabelled"
        return(values)
      }

      for (candidate in c("comparison", "DE", "category", "variable")) {
        if (has_values(df, candidate, df_names)) {
          values <- as.character(df[[candidate]])
          values[is.na(values) | !nzchar(values)] <- "unlabelled"
          return(values)
        }
      }

      dplyr::case_when(
        !is.na(df$padj) & df$padj < padj_thresh & !is.na(df$log2fc) & df$log2fc > lfc_thresh ~ "Up",
        !is.na(df$padj) & df$padj < padj_thresh & !is.na(df$log2fc) & df$log2fc < -lfc_thresh ~ "Down",
        TRUE ~ "No"
      )
    }

    # ── Vectorised choice builders ─────────────────────────────────────────────
    
    # Single grouping for violin if has_multiple_values fails
    compare_violin_group <- function(df, padj_thresh, lfc_thresh, cols = names(df)) {
      if (all(c("padj", "log2fc") %in% cols) &&
          any(!is.na(df$padj)) &&
          any(!is.na(df$log2fc))) {
        return(list(
          name = "DE category",
          values = build_de_category(df, padj_thresh, lfc_thresh, cols)
        ))
      }

      candidates <- c("cluster_id", "cell_type", "condition_a", "condition_b", "sample_a", "sample_b", "tissue", "sex", "age", "cell_id")
      for (candidate in candidates) {
        if (has_values(df, candidate, cols)) {
          values <- as.character(df[[candidate]])
          values[is.na(values) | !nzchar(values)] <- "unlabelled"
          return(list(name = candidate, values = values))
        }
      }

      list(name = "dataset", values = rep("all", nrow(df)))
    }

    # Default grouping col for heatmap x-axis and umap colour if has_multiple_values fails
    compare_group_col <- function(df, cols = names(df)) {
      candidates <- c("cluster_id", "cell_type", "condition_a", "condition_b", "sample_a", "sample_b", "tissue", "sex", "age", "cell_id")
      present    <- candidates[candidates %in% cols]
      for (candidate in present) {
        if (has_multiple_values(df, candidate, cols)) {
          return(candidate)
        }
      }
      for (candidate in present) {
        if (has_values(df, candidate, cols)) {
          return(candidate)
        }
      }
      if ("gene_symbol" %in% cols) {
        return("gene_symbol")
      }
      if ("obs" %in% cols) {
        return("obs")
      }
      cols[1] #NULL as it may fall to study_id while handling NULL with validate()
    }

    compare_metric_choices <- function(df, cols = names(df)) {
      labels <- c(
        log2fc = "log2fc",
        pvalue = "pvalue",
        padj = "padj",
        abundance_a = "abundance_a",
        abundance_b = "abundance_b",
        pct_expressed_a = "% expressed A",
        pct_expressed_b = "% expressed B"
      )
      choices <- c()
      for (candidate in names(labels)) {
        if (has_values(df, candidate, cols)) {
          choices[[labels[[candidate]]]] <- candidate
        }
      }
      choices
    }

    compare_group_choices <- function(df, include_none = FALSE, include_de_category = TRUE, cols = names(df)) {
      choices <- c()
      if (include_none) choices[["None"]] <- "none"

      if (isTRUE(include_de_category) &&
          (has_multiple_values(df, "de_category", cols) ||
           (all(c("padj", "log2fc") %in% cols) &&
            any(!is.na(df$padj)) &&
            any(!is.na(df$log2fc))))) {
        choices[["DE category"]] <- "de_category"
      }

      candidates <- c("cluster_id", "cell_type", "condition_a", "condition_b", "sample_a", "sample_b", "tissue", "sex", "age", "cell_id")
      present    <- candidates[candidates %in% cols]
      # Filter columns, then create a named vector of choices
      available_choices <- present[vapply(present, function(col) has_multiple_values(df, col, cols), logical(1))]
      c(choices, stats::setNames(available_choices, vapply(available_choices, group_label, character(1))))
      # for (candidate in present) {
      #   if (has_multiple_values(df, candidate, cols)) {
      #     choices[[group_label(candidate)]] <- candidate
      #   }
      # }

      # choices
    }

    searched_terms <- reactive({
      ds <- selected_dataset()
      if (is.null(ds)) return(character(0))
      terms <- unique(trimws(c(
        as.character(ds$genes %||% character(0)),
        as.character(ds$proteins %||% character(0))
      )))
      terms[nzchar(terms)]
    })

    # ── Compare-tab plot helpers ─────────────────────────────────────────────
    
    render_compare_volcano <- function(df, dataset_name, padj_thresh, lfc_thresh, gene = NULL) {
      validate(need(nrow(df) > 0, "No volcano rows are available for this dataset."))
      plot_df <- df |>
        dplyr::mutate(
          significance_value = ifelse(!is.na(padj), padj, pvalue),
          neg_log10p = -log10(pmax(significance_value, 1e-300)),
          feature_label = feature_labels(df),
          sig = dplyr::case_when(
            !is.na(padj) & padj < padj_thresh & log2fc >  lfc_thresh ~ "Up",
            !is.na(padj) & padj < padj_thresh & log2fc < -lfc_thresh ~ "Down",
            TRUE ~ "NS"
          )
        )
      x_values <- plot_df$log2fc
      y_values <- plot_df$neg_log10p
      x_limit <- suppressWarnings(max(abs(x_values), na.rm = TRUE))
      y_limit <- suppressWarnings(max(y_values, na.rm = TRUE))
      if (!is.finite(x_limit)) x_limit <- 1
      if (!is.finite(y_limit)) y_limit <- 1
      # hover_text <- paste0(
      #   "<b>", plot_df$feature_label, "</b><br>",
      #   "log2FC: ", round(x_values, 3), "<br>",
      #   "padj: ", signif(plot_df$padj, 3), "<br>",
      #   "pvalue: ", signif(plot_df$pvalue, 3)
      # )

      p <- plotly::plot_ly(
        x = x_values,
        y = y_values,
        type = "scattergl",
        mode = "markers",
        color = plot_df$sig,
        colors = c(Up = "#C0392B", Down = "#2980B9", NS = "#BDC3C7"),
        # text = hover_text,
        # hoverinfo = "text",
          customdata = plot_df$feature_label,
          hovertemplate = paste0(
            "<b>%{customdata}</b><br>",
            "log2FC: %{x:.3f}<br>",
            "-log10(p): %{y:.3f}<extra></extra>"
          ),
        marker = list(size = 6, opacity = 0.7)
      ) |>
        plotly::add_segments(
          x = -x_limit * 1.1,
          xend = x_limit * 1.1,
          y = -log10(padj_thresh),
          yend = -log10(padj_thresh),
          line = list(dash = "dot", color = "#7F8C8D", width = 1),
          showlegend = FALSE,
          hoverinfo = "skip"
        ) |>
        plotly::add_segments(
          x = lfc_thresh, xend = lfc_thresh,
          y = 0, yend = y_limit * 1.05,
          line = list(dash = "dot", color = "#7F8C8D", width = 1),
          showlegend = FALSE,
          hoverinfo = "skip"
        ) |>
        plotly::add_segments(
          x = -lfc_thresh, xend = -lfc_thresh,
          y = 0, yend = y_limit * 1.05,
          line = list(dash = "dot", color = "#7F8C8D", width = 1),
          showlegend = FALSE,
          hoverinfo = "skip"
        ) |>
        plotly::layout(
          title = list(text = dataset_name, x = 0.02),
          xaxis = list(title = "log2 Fold Change"),
          yaxis = list(title = "-log10(padj / p-value)"),
          showlegend = FALSE,
          margin = list(t = 50)
        )
      
      # Annotate searched gene if present
      if (length(gene) > 0 && "gene_symbol" %in% names(plot_df)) {
        for (g in gene) {
          match_idxs <- which(toupper(plot_df$gene_symbol) == toupper(g))[1]
          for (idx in match_idxs) {          # one annotation per row (each cell_type etc.)
            gp <- plot_df[idx, , drop = FALSE]
            p  <- p |> plotly::add_annotations(
              x = gp$log2fc, y = gp$neg_log10p,
              text = paste0("<b>", gp$gene_symbol, "</b>"),
              showarrow = TRUE, arrowhead = 2, arrowsize = 0.8,
              font = list(size = 12, color = "#2C3E50")
            )
          }
        }
      }
      p
    }

    render_compare_violin <- function(df, dataset_name, padj_thresh, lfc_thresh,
                                      y_col = "log2fc", x_axis = "de_category",
                                      show_box = TRUE) {
      # If dataset doesn't contain x_axis (e.g. cell_type from a mixed modality selection), fall back gracefully
      # TODO: checks
      cols <- names(df)
      if (!identical(x_axis, "de_category") &&
          (!x_axis %in% cols || !has_multiple_values(df, x_axis, cols))) {
        # Find best available x-axis grouping for THIS dataset (when multiple are selected)
        grp <- compare_violin_group(df, padj_thresh, lfc_thresh, cols)
        x_axis <- if (has_multiple_values(df, "de_category", cols) ||
                      all(c("padj", "log2fc") %in% cols)) "de_category" else grp$name
      }

      # Validate y_col, grouping column for x_axis, of fallbacks
      if (!y_col %in% cols || !any(!is.na(df[[y_col]]))) {
        fallback_choices <- compare_metric_choices(df)
        y_col <- if ("log2fc" %in% cols) "log2fc" else unname(fallback_choices)[1]
        y_col <- if (length(y_col) == 0 || is.na(y_col)) "log2fc" else y_col
      }

      group_values <- if (identical(x_axis, "de_category")) {
        build_de_category(df, padj_thresh, lfc_thresh, cols)
      } else if (x_axis %in% cols) {
        values <- as.character(df[[x_axis]])
        values[is.na(values) | !nzchar(values)] <- "unlabelled"
        values
      } else {
        compare_violin_group(df, padj_thresh, lfc_thresh, cols)$values
      }
      group_name <- if (identical(x_axis, "de_category")) "DE category" else group_label(x_axis)

      plot_df <- df |>
        dplyr::mutate(
          group_value = group_values,
          feature_label = feature_labels(df),
          sig = dplyr::case_when(
            !is.na(padj) & padj < padj_thresh & log2fc >  lfc_thresh ~ "Up",
            !is.na(padj) & padj < padj_thresh & log2fc < -lfc_thresh ~ "Down",
            TRUE ~ "NS"
          )
        )
      plot_df <- plot_df[!is.na(plot_df[[y_col]]), , drop = FALSE]
      validate(need(nrow(plot_df) > 0, paste("No", y_col, "values are available for this dataset.")))

      group_levels <- unique(plot_df$group_value)
      p <- plotly::plot_ly()
      for (group_level in group_levels) {
        group_df <- plot_df[plot_df$group_value == group_level, , drop = FALSE]
        p <- p |>
          plotly::add_trace(
            x = rep(group_level, nrow(group_df)),
            y = group_df[[y_col]],
            type = "violin",
            name = group_level,
            box = list(visible = isTRUE(show_box)),
            meanline = list(visible = TRUE),
            points = FALSE,
            line = list(color = "#D7DEE5"),
            fillcolor = "rgba(215, 222, 229, 0.45)",
            hovertemplate = paste0(
              "<b>", group_level, "</b><br>", y_col, ": %{y:.3f}<extra></extra>"
            ),
            showlegend = FALSE
          )
      }

      hover_text <- paste0(
        "<b>", plot_df$feature_label, "</b><br>",
        group_name, ": ", plot_df$group_value, "<br>",
        y_col, ": ", round(plot_df[[y_col]], 3), "<br>",
        "padj: ", signif(plot_df$padj, 3), "<br>",
        "class: ", plot_df$sig
      )

      p |>
        plotly::add_trace(
          x = plot_df$group_value,
          y = plot_df[[y_col]],
          type = "scattergl",
          mode = "markers",
          color = plot_df$sig,
          colors = c(Up = "#C0392B", Down = "#2980B9", NS = "#BDC3C7"),
          marker = list(size = 6, opacity = 0.65),
          text = hover_text,
          hoverinfo = "text",
          showlegend = TRUE
        ) |>
        plotly::layout(
          title = list(text = dataset_name, x = 0.02),
          xaxis = list(title = group_name, tickangle = -25),
          yaxis = list(title = y_col),
          legend = list(title = list(text = "Significance"), orientation = "h",
                        y = -0.15),
          showlegend = TRUE,
          margin = list(t = 50)
        )
    }

    render_compare_heatmap <- function(df, dataset_row, dataset_name, padj_thresh, lfc_thresh,
                                       group_col = NULL, selected_terms = character(0),
                                       goi_df = NULL) {
      
      # Append selected terms on heatmap where available
      ds <- selected_dataset()
      goi_terms <- unique(trimws(selected_terms %||% ds$genes %||% character(0)))
      goi_terms <- goi_terms[nzchar(goi_terms)]

      df_is_empty <- nrow(df) == 0
      
      if (length(goi_terms) > 0) {
        # Use pre-fetched goi_df if provided, otherwise fall back to direct fetch
        if (is.null(goi_df)) {
          goi_df <- fetch_expression_goi(
            lab_source = dataset_row$lab_source[1],
            study_id   = dataset_row$study_id[1],
            genes      = goi_terms,
            limit      = 5000L
          )
        }

        group_col <- group_col %||% compare_group_col(if (df_is_empty) goi_df else df)
        cols <- names(df)
        goi_cols <- names(goi_df)
        if (identical(group_col, "de_category")) {
          if (!"de_category" %in% cols) {
            df$de_category <- build_de_category(df, padj_thresh, lfc_thresh)
          }
          if (!"de_category" %in% goi_cols) {
            goi_df$de_category <- build_de_category(goi_df, padj_thresh, lfc_thresh)
          }
        }

        all_groups <- unique(c(
          if (group_col %in% cols) df[[group_col]] else character(0),
          if (group_col %in% goi_cols) goi_df[[group_col]] else character(0)
        ))

        if (nrow(goi_df) > 0) {
          # Ensure grouping column exists
          if (!group_col %in% goi_cols) {
            # goi_df[[group_col]] <- df[[group_col]][1] #"GOI"  - check not forcing goi in wrong ccategory
            goi_df[[group_col]] <- goi_df[[group_col]] %||% goi_df$de_category %||% NA
          }

          # Align columns
          common_cols <- union(cols, goi_cols)
          for (col in setdiff(common_cols, cols)) df[[col]] <- NA
          for (col in setdiff(common_cols, goi_cols)) goi_df[[col]] <- NA

          if (df_is_empty) {
            df <- goi_df
          } else {
            df <- rbind(df[, common_cols, drop = FALSE],
                        goi_df[, common_cols, drop = FALSE])
            # df$gene_symbol <- trimws(df$gene_symbol)
          }
        }
      }

      validate(need(nrow(df) > 0, "No heatmap rows are available for this dataset."))

      # Top genes ranking
      group_col <- group_col %||% compare_group_col(df)
      if (!group_col %in% cols && !identical(group_col, "de_category")) {
        group_col <- compare_group_col(df)
      }
      if (identical(group_col, "de_category")) {
        df$de_category <- build_de_category(df, padj_thresh, lfc_thresh)
      }
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

      # add goi to ranking
      goi_terms_present <- unique(df$gene_symbol[
        tolower(df$gene_symbol) %in% tolower(goi_terms)
      ]) #preserves original casing from df, while allowing for case-insensitivity
      ranked_genes <- unique(c(goi_terms_present, ranked_genes))

      ranked_genes <- ranked_genes[seq_len(min(30L, length(ranked_genes)))]

      df <- df[df$gene_symbol %in% ranked_genes, , drop = FALSE]
      validate(need(nrow(df) > 0, "No heatmap rows remain after ranking/filtering."))
      
      # Reshape by collapsing duplicates per (gene, group)
      agg <- stats::aggregate(
        df$log2fc,
        by = list(gene_symbol = df$gene_symbol, group = df[[group_col]]),
        FUN = function(x) x[which.max(abs(x))]  #OR `FUN = mean)` - decided max due to some groups/categories empty, while mean() can't handle NA and is baised towards 0
      )
      names(agg)[3] <- "log2fc"
      agg <- agg[!is.na(agg$gene_symbol) & !is.na(agg$group), , drop = FALSE]
      validate(need(nrow(agg) > 0, "No grouped heatmap rows remain after aggregation."))
      
      gene_levels <- unique(c(
        goi_terms_present,
        agg$gene_symbol
      ))  # ensure goi included in heatmap and prevents duplicate rows collapsing into one label
      if (!exists("all_groups", inherits = FALSE) || length(all_groups) == 0) {
        all_groups <- unique(agg$group)
      }
      group_levels <- all_groups[!is.na(all_groups)]  # group_levels <- unique(agg$group)
      gene_levels  <- gene_levels[!is.na(gene_levels)]
      mat <- matrix(
        NA_real_,
        nrow = length(gene_levels),
        ncol = length(group_levels),
        dimnames = list(gene_levels, group_levels)
      )
      row_idx <- match(agg$gene_symbol, gene_levels)
      col_idx <- match(agg$group, group_levels)
      mat[cbind(row_idx, col_idx)] <- agg$log2fc
      # Only assign where both indices are valid
      # valid   <- !is.na(row_idx) & !is.na(col_idx)
      # mat[cbind(row_idx[valid], col_idx[valid])] <- agg$log2fc[valid]
      # mat[is.na(mat)] <- 0

      plotly::plot_ly(
        z = unclass(mat),
        x = colnames(mat),
        y = rownames(mat),
        zmin = -max(abs(mat), na.rm = TRUE),  # colour scale
        zmax =  max(abs(mat), na.rm = TRUE),
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
          xaxis = list(title = group_label(group_col), tickangle = -35),
          yaxis = list(title = "", 
                       automargin = TRUE,
                       tickmode   = "array",
                       tickvals   = seq_along(rownames(mat)) - 1,
                       ticktext   = rownames(mat),
                       tickfont   = list(size = 10)
                      ),
          margin = list(t = 50)
        )
    }

    render_compare_histogram <- function(row, metric = "log2fc", group_by = "none", bins = 30L) {
      df <- fetch_expression_histogram(
        lab_source = row$lab_source[1],
        study_id = row$study_id[1],
        metric = metric,
        bins = bins,
        group_by = if (identical(group_by, "none")) NULL else group_by
      )
      validate(need(nrow(df) > 0, "No histogram bins are available for this dataset."))

      groups <- unique(df$group_value)
      p <- plotly::plot_ly()
      for (group in groups) {
        group_df <- df[df$group_value == group, , drop = FALSE]
        midpoints <- (group_df$bin_start + group_df$bin_end) / 2
        p <- p |>
          plotly::add_bars(
            x = midpoints,
            y = group_df$row_count,
            name = group,
            hovertemplate = paste0(
              "<b>", group, "</b><br>",
              "bin: %{x:.3f}<br>",
              "rows: %{y}<extra></extra>"
            )
          )
      }

      p |>
        plotly::layout(
          title = list(text = row$dataset_name[1], x = 0.02),
          barmode = if (length(groups) > 1) "overlay" else "group",
          xaxis = list(title = metric),
          yaxis = list(title = "Row count"),
          margin = list(t = 50)
        )
    }

    render_compare_dots <- function(row, current_state) {
      validate(need(row$omic_type[1] %in% c("scrna", "snrna"),
                    "Dot plot is only available for scRNA-seq and snRNA-seq datasets."))

      # TODO must match dots_plot.R logic!!
      df <- fetch_expression_groups(
        lab_source = row$lab_source[1],
        study_id = row$study_id[1],
        group_by = "cluster_id",
        metric = "log2fc",
        top_n = 20L
      )
      validate(need(nrow(df) > 0, "No grouped feature summaries are available for this dataset."))
      df$mean_pct_expressed[is.na(df$mean_pct_expressed)] <- 0
      df$mean_value[is.na(df$mean_value)] <- 0

      plotly::plot_ly(
        x = df$group_value,
        y = df$feature_label,
        type = "scattergl",
        mode = "markers",
        size = pmax(df$mean_pct_expressed, 0) * 0.18 + 6,
        color = df$mean_value,
        colors = c("#2980B9", "#FFFFFF", "#C0392B"),
        marker = list(showscale = TRUE, colorbar = list(title = "Mean log2FC")),
        text = paste0(
          "<b>", df$feature_label, "</b><br>",
          "group: ", df$group_value, "<br>",
          "mean log2FC: ", signif(df$mean_value, 3), "<br>",
          "mean % expressed: ", signif(df$mean_pct_expressed, 3)
        ),
        hoverinfo = "text"
      ) |>
        plotly::layout(
          title = list(text = row$dataset_name[1], x = 0.02),
          xaxis = list(title = "Cluster"),
          yaxis = list(title = "Feature", autorange = "reversed"),
          margin = list(t = 50)
        )
    }

    render_compare_top_features <- function(row, current_state) {
      validate(need(row$omic_type[1] %in% c("scrna", "snrna"),
                    "Top features is only available for scRNA-seq and snRNA-seq datasets."))

      # TODO: once live in UI make `fetch_expression_groups()` reactive as also used in `render_compare_plots()` with a toggle of top_n and selected genes/terms
      df <- fetch_expression_groups(
        lab_source = row$lab_source[1],
        study_id = row$study_id[1],
        group_by = "cluster_id",
        metric = "abundance_a",
        top_n = 15L,
        genes = current_state$genes %||% character(0),
        proteins = current_state$proteins %||% character(0)
      )
      validate(need(nrow(df) > 0, "No ranked features are available for this dataset."))
      ranked <- stats::aggregate(mean_value ~ feature_label, data = df, FUN = function(x) x[which.max(abs(x))])  #OR `FUN = mean)`
      ranked <- ranked[order(ranked$mean_value, decreasing = TRUE), , drop = FALSE]

      plotly::plot_ly(
        x = ranked$mean_value,
        y = reorder(ranked$feature_label, ranked$mean_value),
        type = "bar",
        orientation = "h",
        text = paste0("<b>", ranked$feature_label, "</b><br>mean value: ", signif(ranked$mean_value, 3)),
        hoverinfo = "text"
      ) |>
        plotly::layout(
          title = list(text = row$dataset_name[1], x = 0.02),
          xaxis = list(title = "Mean value"),
          yaxis = list(title = "Feature"),
          margin = list(l = 160, t = 50)
        )
    }

    render_compare_feature_scatter <- function(row, current_state, padj_thresh, lfc_thresh, goi_df = NULL) {
      terms <- unique(c(current_state$genes %||% character(0), current_state$proteins %||% character(0)))
      validate(need(length(terms) > 0, "Select one or more genes or proteins in the modal to compare GOI scatter plots."))

      df <- goi_df
      if (is.null(df)) {
        df <- fetch_expression_goi(
          lab_source = row$lab_source[1],
          study_id = row$study_id[1],
          genes = current_state$genes %||% character(0),
          proteins = current_state$proteins %||% character(0),
          limit = 2000L
        )
      }
      validate(need(nrow(df) > 0, "No rows are available for the selected genes/proteins."))

      x_col <- if ("abundance_a" %in% names(df) && any(!is.na(df$abundance_a))) "abundance_a" else "log2fc"
      y_col <- if ("abundance_b" %in% names(df) && any(!is.na(df$abundance_b))) "abundance_b" else "pvalue"
      df$feature_label <- feature_labels(df)
      df$sig <- dplyr::case_when(
        !is.na(df$padj) & df$padj < padj_thresh & df$log2fc >  lfc_thresh ~ "Up",
        !is.na(df$padj) & df$padj < padj_thresh & df$log2fc < -lfc_thresh ~ "Down",
        TRUE ~ "NS"
      )
      x_values <- df[[x_col]]
      y_values <- if (identical(y_col, "pvalue")) -log10(pmax(df$pvalue, 1e-300)) else df[[y_col]]

      plotly::plot_ly(
        x = x_values,
        y = y_values,
        type = "scattergl",
        mode = "markers",
        color = df$sig,
        colors = c(Up = "#C0392B", Down = "#2980B9", NS = "#BDC3C7"),
        text = paste0(
          "<b>", df$feature_label, "</b><br>",
          x_col, ": ", signif(x_values, 3), "<br>",
          if (identical(y_col, "pvalue")) "-log10(pvalue)" else y_col, ": ", signif(y_values, 3)
        ),
        hoverinfo = "text",
        marker = list(size = 8, opacity = 0.75)
      ) |>
        plotly::layout(
          title = list(text = row$dataset_name[1], x = 0.02),
          xaxis = list(title = x_col),
          yaxis = list(title = if (identical(y_col, "pvalue")) "-log10(pvalue)" else y_col),
          margin = list(t = 50)
        )
    }

    render_compare_umap <- function(embedding_df, row, current_state, reduction = "umap", embedding_view = "metadata_overview") {
      validate(need(row$omic_type[1] %in% c("scrna", "snrna"),
                    "Embedding plots are only available for scRNA-seq and snRNA-seq datasets."))

      df <- embedding_df
      validate(need(nrow(df) > 0, "No embedding coordinates are available for this dataset."))

      if (!identical(embedding_view, "metadata_overview") &&
          "term" %in% names(df) &&
          any(df$term == embedding_view, na.rm = TRUE)) {
        df <- df[df$term == embedding_view, , drop = FALSE]
        color_values <- log10(pmax(df$expression_value, 0) + 1)
        color_scale <- c("#F7FBFF", "#6BAED6", "#08306B")
        color_title <- paste0("log10(", embedding_view, " + 1)")
      } else {
        color_col <- compare_group_col(df)
        color_values <- as.character(df[[color_col]])
        color_scale <- NULL
        color_title <- color_col
      }

      plotly::plot_ly(
        x = df$dim_1,
        y = df$dim_2,
        type = "scattergl",
        mode = "markers",
        color = color_values,
        colors = color_scale,
        marker = list(size = 4, opacity = 0.7),
        text = paste0("obs: ", df$obs),
        hoverinfo = "text"
      ) |>
        plotly::layout(
          title = list(text = paste(row$dataset_name[1], "·", color_title), x = 0.02),
          xaxis = list(title = paste0(toupper(reduction), " 1")),
          yaxis = list(title = paste0(toupper(reduction), " 2")),
          margin = list(t = 50)
        )
    }

    # load_active_cell_types <- function(lab_source, study_id) {
    #   opts <- tryCatch(
    #     fetch_metadata_filter_options(lab_source, study_id),
    #     error = function(e) NULL
    #   )

    #   if (is.null(opts) || !"cell_types" %in% names(opts) || length(opts$cell_types) == 0) {
    #     return(character(0))
    #   }

    #   cts <- opts$cell_types[[1]]
    #   cts[!is.na(cts) & nzchar(cts)]
    # }

    # set_active_dataset <- function(current_state, row) {
    #   opts <- tryCatch(
    #     fetch_metadata_filter_options(row$lab_source[1], row$study_id[1]),
    #     error = function(e) NULL
    #   )
    #   current_state$lab_source   <- row$lab_source[1]
    #   current_state$study_id     <- row$study_id[1]
    #   current_state$dataset_name <- row$dataset_name[1]
    #   current_state$omic_type    <- row$omic_type[1]
    #   current_state$cell_types           <- opts$cell_types[[1]] %||% character(0)
    #   current_state$available_assays     <- opts$available_assays %||% c("logcounts", "counts")
    #   current_state$available_reductions <- opts$available_reductions %||% c("umap", "pca", "tsne")
    #   current_state
    # }

    # ── Datasets and their reactive state ────────────────────────────────────────────
    
    # selected_dataset() holds all search results and searched terms: 
    # list(
    #   genes, lab_source, study_id, dataset_name, omic_type,
    #   selected_datasets, cell_types
    # )
    selected_dataset <- reactiveVal(NULL)

    # This reactiveVal tracks the current rows selected inside Dataset Listings and drives 'Expression' and 'Compare' tab
    # Can also drive the aggregate value boxes.
    listing_selection <- reactiveVal(integer())
    last_dataset_keys <- reactiveVal(character())

    # Active single-dataset selection for 'Plot' tab.
    active_row <- reactiveVal(NULL)
    active_dataset <- reactive({
      ds <- selected_dataset()
      idx <- active_row()
      if (is.null(ds) || is.null(ds$selected_datasets) || nrow(ds$selected_datasets) == 0) return(NULL)
      if (is.null(idx) || length(idx) == 0 || idx < 1 || idx > nrow(ds$selected_datasets)) return(NULL)
      row <- ds$selected_datasets[idx, , drop = FALSE]
      row_selected <- as.list(row[1, , drop = FALSE])
      row_selected$genes <- ds$genes
      row_selected$proteins <- ds$proteins
      row_selected$available_assays     <- ds$available_assays
      row_selected$available_reductions <- ds$available_reductions
      row_selected
    })

    # Reactive exposes the actively DT selected datasets from the Dataset Listings table driving the 'Compare' tab.
    # i.e. a subset of selected_dataset()$selected_datasets, filtered by listing_selection()
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

    # Only fetch when the relevant tab is selected — avoids API calls on modal confirm.
    is_expression_tab <- reactive({
      grepl("Expression", input$exploration_tabs %||% "Expression", ignore.case = TRUE)
    })
    is_plot_tab <- reactive({
      tab <- input$exploration_tabs %||% ""
      grepl("Plot", tab, ignore.case = TRUE) && !grepl("Compare", tab, ignore.case = TRUE)
    })
    is_compare_tab <- reactive({
      grepl("Compare", input$exploration_tabs %||% "", ignore.case = TRUE)
    })
    is_gene_drug_tab <- reactive({
      grepl("Gene-Drug", input$exploration_tabs %||% "", ignore.case = TRUE)
    })

    gated_active_dataset <- function(type = NULL) {
      reactive({
        req(is_plot_tab())
        if (!is.null(type)) req(identical(sidebar_vals$plot_type(), type))
        active_dataset()
      })
    }
    gated_gene_drug_dataset <- reactive({
      req(is_gene_drug_tab())
      active_dataset()
    })

    # ── Per-idx cached data reactives for Compare tab ──────────────────────────

    compare_row_data_cache <- local({
      cache <- list()

      function(idx) {
        key <- as.character(idx)
        if (is.null(cache[[key]])) {
          # <<- looks for cached rows in its enclosing scope (i.e. `local({})` env NOT .GlobalEnv, also new session per user due to server module's `moduleServer()`)
          cache[[key]] <<- reactive({
            req(is_compare_tab())
            datasets <- compare_source_rows()
            req(datasets, nrow(datasets) >= idx)

            row <- datasets[idx, , drop = FALSE]
            req(nrow(row) > 0)

            plot_type <- sidebar_vals$plot_type()
            switch(
              plot_type,
              Heatmap = fetch_top_de(
                lab_source = row$lab_source[1],
                study_id = row$study_id[1],
                n = 20L,
                padj_thresh = sidebar_vals$padj_thresh(),
                lfc_thresh = sidebar_vals$lfc_thresh_min()
              ),
              Violin = fetch_expression_table(
                lab_source = row$lab_source[1],
                study_id = row$study_id[1],
                limit = 3000L,
                offset = 0L,
                sort_by = "padj",
                sort_dir = "asc"
              ),
              fetch_expression_volcano(
                lab_source = row$lab_source[1],
                study_id = row$study_id[1],
                limit = 20000L,
                offset = 0L
              )
            )
          }) |>
            bindCache(
              compare_source_rows()[idx, , drop = FALSE]$lab_source[1],
              compare_source_rows()[idx, , drop = FALSE]$study_id[1],
              sidebar_vals$plot_type()
            )
        }

        cache[[key]]
      }
    })

    # Cache fetch_expression_goi for heatmap (top-de merge & render_compare_heatmap), violin and scatter in Compare tab, keyed by idx, genes, proteins, limit
    compare_goi_cache <- local({
      cache <- list()

      function(idx, genes, proteins, limit = 5000L) {
        key <- paste0(
          idx, "::",
          compare_source_rows()[idx, , drop = FALSE]$lab_source[1], "::",
          compare_source_rows()[idx, , drop = FALSE]$study_id[1], "::",
          paste(genes, collapse = ","), "::",
          paste(proteins, collapse = ","), "::",
          limit
        )
        if (is.null(cache[[key]])) {
          cache[[key]] <<- reactive({
            req(is_compare_tab())
            datasets <- compare_source_rows()
            req(datasets, nrow(datasets) >= idx)
            row <- datasets[idx, , drop = FALSE]
            req(nrow(row) > 0, length(c(genes, proteins)) > 0)

            fetch_expression_goi(
              lab_source = row$lab_source[1],
              study_id = row$study_id[1],
              genes = genes,
              proteins = proteins,
              limit = limit
            )
          }) |>
            bindCache(
              compare_source_rows()[idx, , drop = FALSE]$lab_source[1],
              compare_source_rows()[idx, , drop = FALSE]$study_id[1],
              paste(genes, collapse = ","),
              paste(proteins, collapse = ","),
              limit
            )
        }

        cache[[key]]
      }
    })

    compare_embedding_cache <- local({
      cache <- list()

      function(idx, reduction, embedding_view) {
        reduction <- reduction %||% "umap"
        embedding_view <- embedding_view %||% "metadata_overview"
        key <- paste(idx, reduction, embedding_view, sep = "::")
        if (is.null(cache[[key]])) {
          cache[[key]] <<- reactive({
            req(is_compare_tab(), identical(sidebar_vals$plot_type(), "UMAP"))
            datasets <- compare_source_rows()
            req(datasets, nrow(datasets) >= idx)
            row <- datasets[idx, , drop = FALSE]
            req(row$omic_type[1] %in% c("scrna", "snrna"))

            ds <- selected_dataset()
            fetch_dataset_embeddings(
              lab_source = row$lab_source[1],
              study_id = row$study_id[1],
              reduction = reduction,
              assay = "counts", #logcounts
              genes = if (!identical(embedding_view, "metadata_overview")) embedding_view else ds$genes %||% character(0),
              proteins = if (identical(embedding_view, "metadata_overview")) ds$proteins %||% character(0) else character(0),  #proteins = character(0)
              max_points = 50000L
            )
          }) |>
            bindCache(
              compare_source_rows()[idx, , drop = FALSE]$lab_source[1],
              compare_source_rows()[idx, , drop = FALSE]$study_id[1],
              reduction, #input$compare_umap_reduction %||% "umap",
              embedding_view,
              paste(selected_dataset()$genes %||% character(0), collapse = ","),
              paste(selected_dataset()$proteins %||% character(0), collapse = ",")
            )
        }

        cache[[key]]
      }
    })

    get_compare_row_data <- function(idx) {
      compare_row_data_cache(idx)
    }

    get_compare_goi_data <- function(idx, genes, proteins, limit = 5000L) {
      compare_goi_cache(idx, genes, proteins, limit)
    }

    get_compare_embedding_data <- function(idx, reduction, embedding_view) {
      compare_embedding_cache(idx, reduction, embedding_view)
    }

    # ── Sync URL with selected dataset (i.e. updates on every checkbox toggle in Dataset Listings) for that session ────
    observe({
      ds <- selected_dataset()
      req(!is.null(ds))

      query_string <- build_share_query(ds)
      current_search <- isolate(session$clientData$url_search %||% "")

      if (!identical(current_search, query_string)) {
        updateQueryString(query_string, mode = "replace", session = session)
      }
    })

    observeEvent(initial_link(), {
      link <- initial_link()
      req(!is.null(link))
      if (!is.null(selected_dataset())) return()

      payload <- build_link_selected_payload(list(
        lab_source = link$lab_source %||% NULL,   # currently not required in url
        study_id = link$study_id %||% NULL,       # currently required in url
        genes = clean_terms(link$genes %||% character(0)),
        proteins = clean_terms(link$proteins %||% character(0))
      ))

      if (!is.null(payload)) {
        selected_dataset(payload)
      } else {
        showNotification(
          "Could not preload the linked dataset. Check the shared lab/study or dataset name.",
          type = "warning",
          duration = 8
        )
      }
    }, ignoreNULL = TRUE)

    # ── Plot tab UI controlled by sidebar selection and row selection in Dataset Listings ─────────────────
    
    # This render block swaps the single-dataset plot module based on the
    # sidebar plot selector while keeping the active dataset row in control.
    gene_selector_server("gene_selector", selected_dataset)
    sidebar_vals <- sidebar_server("filters", selected_dataset)

    output$plot_ui <- renderUI({
      plot_type <- sidebar_vals$plot_type()
      requires_single_cell <- plot_type %in% c("UMAP", "Dots Plot", "Top Features")
      active <- active_dataset()

      if (is.null(active)) {
        return(
          tags$div(
            class = "alert alert-info",
            role = "alert",
            "Select one or more rows from the dataset listing to render plots."
          )
        )
      }

      if (requires_single_cell &&
          !isTRUE(active$omic_type %in% c("scrna", "snrna"))) {
        return(
          tags$div(
            class = "alert alert-info",
            role = "alert",
            paste(plot_type, "is only available for scRNA-seq and snRNA-seq datasets.")
          )
        )
      }

      switch(
        plot_type,
        "Feature Scatter" = feature_scatter_ui(session$ns("feature_scatter")),
        Histogram = histogram_ui(session$ns("histogram")),
        "Dots Plot" = dots_ui(session$ns("dots")),
        "Top Features" = highest_expr_ui(session$ns("highest_expr")),
        Heatmap = heatmap_ui(session$ns("heatmap")),
        Violin = violin_ui(session$ns("violin")),
        UMAP = umap_ui(session$ns("umap")),
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
        paste(ds$dataset_name %||% "", "·", ds$omic_type %||% "", ds$lab_source %||% ""),
        tags$br(),
        tags$small("Preview loads the searched terms sorted by padj.")
      )
    })

    output$active_dataset_banner_plot <- renderUI({
      ds <- active_dataset()
      if (is.null(ds)) return(NULL)
      tags$div(
        class = "alert alert-secondary",
        role = "alert",
        style = "margin-bottom: 12px;",
        tags$strong("Plot is showing: "),
        paste(ds$dataset_name %||% "", "·", ds$omic_type %||% "", ds$lab_source %||% "")
      )
    })

    # output$shareable_link_ui <- renderUI({
    #   ds <- selected_dataset()
    #   if (is.null(ds)) return(NULL)

    #   gene_terms <- clean_terms(ds$genes %||% character(0))
    #   protein_terms <- clean_terms(ds$proteins %||% character(0))

    #   base_url <- paste0(
    #     session$clientData$url_protocol,
    #     "//",
    #     session$clientData$url_hostname,
    #     if (nzchar(session$clientData$url_port)) paste0(":", session$clientData$url_port) else "",
    #     session$clientData$url_pathname
    #   )

    #   query_parts <- c(
    #     "page=explore",
    #     paste0("lab=", utils::URLencode(ds$lab_source, reserved = TRUE)),
    #     paste0("study=", utils::URLencode(as.character(ds$study_id), reserved = TRUE)),
    #     vapply(
    #       gene_terms,
    #       function(term) paste0("gene=", utils::URLencode(term, reserved = TRUE)),
    #       character(1)
    #     ),
    #     vapply(
    #       protein_terms,
    #       function(term) paste0("protein=", utils::URLencode(term, reserved = TRUE)),
    #       character(1)
    #     )
    #   )

    #   share_url <- paste0(base_url, "?", paste(query_parts, collapse = "&"))

    #   tags$div(
    #     style = "margin-top: 12px;",
    #     tags$label(
    #       `for` = ns("shareable_link"),
    #       style = "font-weight: 600; display: block; margin-bottom: 4px;",
    #       "Shareable link for current dataset"
    #     ),
    #     tags$input(
    #       id = ns("shareable_link"),
    #       type = "text",
    #       class = "form-control",
    #       readonly = "readonly",
    #       value = share_url,
    #       onclick = "this.select();"
    #     ),
    #     tags$small(
    #       class = "text-muted",
    #       "This link includes the active dataset and any currently selected genes/proteins."
    #     )
    #   )
    # })

    # ── Tab/PlotType-gated single-dataset reactives ────────────────────────────────────────────────

    # Fetches combined DE / expression rows for every dataset selected in Dataset Listings to
    # power the 'Expression data' & 'Compare' tabs.
    expression_data <- reactive({
      req(is_expression_tab() || is_compare_tab())
      ds <- selected_dataset()
      datasets <- compare_source_rows()
      req(ds, nrow(datasets) > 0, length(c(ds$genes, ds$proteins)) > 0)

      if (nrow(datasets) == 1) {
        row <- datasets[1, , drop = FALSE]
        single_df <- fetch_expression_table(
          lab_source = row$lab_source[1],
          study_id = row$study_id[1],
          limit = 1000L,
          offset = 0L,
          sort_by = "padj",
          sort_dir = "asc",
          genes = ds$genes,
          proteins = ds$proteins
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
        lfc_thresh = sidebar_vals$lfc_thresh_min(),
        genes = ds$genes,
        proteins = ds$proteins
      )
    }) |> bindCache(
      is_expression_tab() || is_compare_tab(),
      paste(compare_source_rows()$lab_source, collapse = ","),
      paste(compare_source_rows()$study_id,   collapse = ","),
      paste(selected_dataset()$genes    %||% character(0), collapse = ","),
      paste(selected_dataset()$proteins %||% character(0), collapse = ",")
    )
    # Causes Error: object '' not found - due to lapply FUN needed on cache key
    #  |> bindCache(
    #         selected_dataset()$lab_source %||% "",
    #         selected_dataset()$study_id %||% "",
    #         paste0(dataset_key(compare_source_rows()), collapse = ","),
    #         paste(selected_dataset()$genes %||% character(0), collapse = ","),     #ds$genes,
    #         paste(selected_dataset()$proteins %||% character(0), collapse = ","),  #ds$proteins,
    #       )

    # Shared slice for Violin / Histogram on the Plot tab.
    plot_table_data <- reactive({
      req(is_plot_tab())
      pt <- sidebar_vals$plot_type()
      req(pt %in% c("Violin", "Histogram"))
      ds <- active_dataset()
      req(ds)

      fetch_expression_table(
        lab_source = ds$lab_source,
        study_id = ds$study_id,
        limit = 3000L,
        offset = 0L,
        sort_by = "padj",
        sort_dir = "asc"
      )
    }) |> bindCache(
      is_plot_tab(),
      sidebar_vals$plot_type(),
      active_dataset()$lab_source %||% "",
      active_dataset()$study_id %||% ""
    )

    volcano_data <- reactive({
      req(is_plot_tab(), identical(sidebar_vals$plot_type(), "Volcano"))
      ds <- active_dataset()
      req(ds)

      fetch_expression_volcano(
        lab_source = ds$lab_source,
        study_id = ds$study_id,
        limit = 20000L,
        offset = 0L
      )
    }) |> bindCache(
      is_plot_tab(),
      sidebar_vals$plot_type(),
      active_dataset()$lab_source %||% "",
      active_dataset()$study_id %||% ""
    )

    violin_data <- reactive({
      req(is_plot_tab(), identical(sidebar_vals$plot_type(), "Violin"))
      plot_table_data()
    })

    expression_data_table <- reactive({
      req(is_expression_tab())
      expression_data()
    })

    # ── Choice reactives for Compare controls ──────────────────────────────────
    # TODO: perhaps add guards is_plot_tab and is_compare_tab
    compare_hist_metric_choices <- reactive({
      df <- expression_data()
      if (is.null(df) || nrow(df) == 0) return(character(0))
      compare_metric_choices(df)
    }) |> bindCache(
      paste(compare_source_rows()$lab_source, collapse=","),
      paste(compare_source_rows()$study_id, collapse=",")
    )

    compare_hist_group_choices <- reactive({
      df <- expression_data()
      if (is.null(df) || nrow(df) == 0) return(c("None" = "none"))
      compare_group_choices(df, include_none = TRUE, include_de_category = TRUE)
    }) |> bindCache(
      paste(compare_source_rows()$lab_source, collapse=","),
      paste(compare_source_rows()$study_id, collapse=",")
    )

    compare_violin_y_choices <- reactive({
      df <- expression_data()
      if (is.null(df) || nrow(df) == 0) return(character(0))
      compare_metric_choices(df)
    }) |> bindCache(
      paste(compare_source_rows()$lab_source, collapse=","),
      paste(compare_source_rows()$study_id, collapse=",")
    )

    compare_violin_x_choices <- reactive({
      df <- expression_data()
      if (is.null(df) || nrow(df) == 0) return(character(0))
      compare_group_choices(df, include_none = FALSE, include_de_category = TRUE)
    }) |> bindCache(
      paste(compare_source_rows()$lab_source, collapse=","),
      paste(compare_source_rows()$study_id, collapse=",")
    )

    compare_heatmap_x_choices <- reactive({
      df <- expression_data()
      if (is.null(df) || nrow(df) == 0) return(character(0))
      compare_group_choices(df, include_none = FALSE, include_de_category = TRUE)
    }) |> bindCache(
      paste(compare_source_rows()$lab_source, collapse=","),
      paste(compare_source_rows()$study_id, collapse=",")
    )

    # ── Compare tab controls and plot grid layout in UI ─────────────────────────────────────────
    output$compare_controls_ui <- renderUI({
      req(is_compare_tab())
      datasets <- compare_source_rows()
      if (nrow(datasets) < 2) return(NULL)

      plot_type <- sidebar_vals$plot_type()

      switch(
        plot_type,
        Histogram = fluidRow(
          column(4, selectInput(session$ns("compare_hist_metric"), "Metric", choices = compare_hist_metric_choices())),
          column(4, selectInput(session$ns("compare_hist_group_by"), "Group by", choices = compare_hist_group_choices())),
          column(4, sliderInput(session$ns("compare_hist_bins"), "Bins", min = 10, max = 60, value = 30, step = 1))
        ),
        Violin = fluidRow(
          column(4, selectInput(session$ns("compare_violin_y_var"), "Y axis", choices = compare_violin_y_choices())),
          column(4, selectInput(session$ns("compare_violin_x_axis"), "X-axis", choices = compare_violin_x_choices())),
          column(4, shiny::checkboxInput(session$ns("compare_violin_show_box"), "Show box plot overlay", value = TRUE))
        ),
        Heatmap = tagList(
          selectInput(session$ns("compare_heatmap_x_axis"), "X-axis", choices = compare_heatmap_x_choices()),
          if (length(searched_terms()) > 0) {
            tags$details(
              style = "margin-bottom: 14px;",
              tags$summary(
                style = "cursor: pointer; font-weight: 600; margin-bottom: 8px;",
                "Genes to display on the heatmap"
              ),
              checkboxGroupInput(
                session$ns("compare_heatmap_terms"),
                label = NULL,
                choices = searched_terms(),
                selected = searched_terms()
              )
            )
          }
        ),
        UMAP = fluidRow(
          column(
            6,
            selectInput(
              session$ns("compare_umap_reduction"),
              "Embedding",
              choices = c("UMAP" = "umap", "PCA" = "pca", "tSNE" = "tsne"),
              selected = "umap"
            )
          ),
          column(
            6,
            selectInput(
              session$ns("compare_umap_term"),
              "Colour by term",
              choices = c("Metadata overview" = "metadata_overview", stats::setNames(searched_terms(), searched_terms())),
              selected = "metadata_overview"
            )
          )
        ),
        NULL
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
              paste0(row$dataset_name[1], " · ", row$omic_type[1], " · ", row$lab_source[1])
            ),
            card_body(
              plotlyOutput(session$ns(paste0("compare_plot_", i)), height = "420px") |> withSpinner(
                type = 1, caption = "Loading plot...", color = "#5b5b5b")
            )
          )
        })
      )
    })

    # ── Compare tab renderPlotly — one per card slot, and cache output ────────────────────────

    # Observe block keeps Compare tab plot outputs in sync with the current dataset set and selected plot type
    observe({
      req(is_compare_tab())
      datasets <- compare_source_rows()
      plot_type <- sidebar_vals$plot_type()
      if (nrow(datasets) < 2) {
        return(invisible(NULL))
      }

      for (i in seq_len(nrow(datasets))) {
        local({
          idx <- i
          row <- datasets[idx, , drop = FALSE]
          output_id <- paste0("compare_plot_", idx)

          output[[output_id]] <- renderPlotly({
            current_state <- selected_dataset()
            req(current_state)

            switch(
              plot_type,
              Heatmap = render_compare_heatmap(
                get_compare_row_data(idx)(),
                row,
                row$dataset_name[1],
                sidebar_vals$padj_thresh(),
                sidebar_vals$lfc_thresh_min(),
                group_col = input$compare_heatmap_x_axis %||% NULL,
                selected_terms = input$compare_heatmap_terms %||% searched_terms(),
                goi_df = if (length(searched_terms()) > 0) {
                  get_compare_goi_data(
                    idx,
                    input$compare_heatmap_terms %||% searched_terms(),
                    character(0)
                  )()
                } else NULL
              ),
              Violin = render_compare_violin(
                get_compare_row_data(idx)(),
                row$dataset_name[1],
                sidebar_vals$padj_thresh(),
                sidebar_vals$lfc_thresh_min(),
                y_col = input$compare_violin_y_var %||% "log2fc",
                x_axis = input$compare_violin_x_axis %||% "de_category",
                show_box = isTRUE(input$compare_violin_show_box)
              ),
              Histogram = render_compare_histogram(
                row,
                metric = input$compare_hist_metric %||% "log2fc",
                group_by = input$compare_hist_group_by %||% "none",
                bins = input$compare_hist_bins %||% 30L
              ),
              "Dots Plot" = render_compare_dots(row, current_state),
              "Top Features" = render_compare_top_features(row, current_state),
              "Feature Scatter" = render_compare_feature_scatter(
                row, current_state,
                sidebar_vals$padj_thresh(),
                sidebar_vals$lfc_thresh_min(),
                goi_df = get_compare_goi_data(
                  idx,
                  current_state$genes %||% character(0),
                  current_state$proteins %||% character(0),
                  limit = 2000L
                )()
              ),
              UMAP = render_compare_umap(
                embedding_df = get_compare_embedding_data(
                  idx,
                  input$compare_umap_reduction,
                  input$compare_umap_term
                  )(),
                row, current_state,
                reduction = input$compare_umap_reduction %||% "umap",
                embedding_view = input$compare_umap_term %||% "metadata_overview"
              ),
              render_compare_volcano(
                get_compare_row_data(idx)(),
                row$dataset_name[1],
                sidebar_vals$padj_thresh(),
                sidebar_vals$lfc_thresh_min(),
                gene = selected_dataset()$genes
              )
            )
          }) |> bindCache(
            compare_source_rows()[idx,,drop=FALSE]$lab_source[1],
            compare_source_rows()[idx,,drop = FALSE]$study_id[1],
            sidebar_vals$plot_type(),
            input$compare_heatmap_x_axis   %||% "",
            paste(input$compare_heatmap_terms %||% character(0), collapse = ","),
            input$compare_violin_y_var     %||% "log2fc",
            input$compare_violin_x_axis    %||% "de_category",
            isTRUE(input$compare_violin_show_box),
            input$compare_hist_metric      %||% "log2fc",
            input$compare_hist_group_by    %||% "none",
            input$compare_hist_bins        %||% 30L,
            input$compare_umap_reduction   %||% "umap",
            input$compare_umap_term        %||% "metadata_overview",
            paste(selected_dataset()$genes %||% character(0), collapse = ","),
            paste(selected_dataset()$proteins %||% character(0), collapse = ",")
          )
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
            "Once the dataset table appears below, select one or more rows to drive the explorer. Selected rows drive the Expression and Compare tabs, and the most recently selected row becomes the active dataset for the single-dataset Plot tab."
          )
        ))
      }

      tagList(
        tags$div(
          class = "alert alert-info",
          role = "alert",
          style = "margin-bottom: 12px;",
          tags$strong("TIP: "),
          "Select which datasets to explore in the visual explorer below. The most recently selected dataset drives the single-dataset 'Plot' tab, while two or more selected datasets activate 'Compare'."
        ),
        DTOutput(session$ns("dataset_listing_table"))
      )
    })

    output$dataset_listing_table <- renderDT({
      ds <- selected_dataset()
      req(ds, !is.null(ds$selected_datasets), nrow(ds$selected_datasets) > 0)
      rows <- ds$selected_datasets
      display <- rows[, intersect(
        c("lab_source", "dataset_name", "omic_type", "matched_genes", "matched_proteins",
          "total_features", "n_sig_features", "total_samples", "total_cells",
          "n_cell_types", "n_conditions"),
        names(rows)
      ), drop = FALSE]

      names(display) <- c(
        "Lab", "Dataset", "Modality", "Matched genes", "Matched proteins",
        "Total features", "Total significant", "Samples", "Cells",
        "Cell types", "Conditions"
      )[seq_along(display)]

      datatable(display, selection="multiple", rownames=FALSE,
        class="table-sm table-hover",
        options=list(dom="t", pageLength=20, scrollX=TRUE))

    }, server=FALSE)

    # ── Reset checkbox state ─────────────────────────────────────────
    # Compare keys to previous dataset(s) to detect any changes in the listing whenever the search modal confirms a fresh dataset list
    observe({
      ds <- selected_dataset()
      req(ds, !is.null(ds$selected_datasets), nrow(ds$selected_datasets) > 0)

      dataset_keys <- dataset_key(ds$selected_datasets)
      if (!identical(dataset_keys, last_dataset_keys())) {
        last_dataset_keys(dataset_keys)
        listing_selection(seq_len(nrow(ds$selected_datasets)))
        dataTableProxy("dataset_listing_table") |> selectRows(seq_len(nrow(ds$selected_datasets)))
        active_row(1) # defaults to first row
      }
    })

    # ── Table selection drives dataset inclusion ─────────────────────
    # The newest selected row becomes active for the single-dataset 'Plot' tab; all selected rows drive 'Expression' and 'Compare' tabs.
    observeEvent(input$dataset_listing_table_rows_selected, {
      # Determines which datasets were recently un/checked by comparing to the previous selection
      row_idx <- input$dataset_listing_table_rows_selected %||% integer(0)
      previous_idx <- isolate(listing_selection())
      if (identical(row_idx, previous_idx)) {
        return(invisible(NULL))
      } # guards against re-triggering when selection hasn't changed

      listing_selection(row_idx)
      if (length(row_idx) == 0) {
        active_row(NULL)
        return(invisible(NULL))
      }
      newly_checked <- setdiff(row_idx, previous_idx)
      if (length(newly_checked) > 0) {
        active_row(tail(newly_checked, 1))
      } else if (is.null(active_row()) || !(active_row() %in% row_idx)) {
        active_row(row_idx[1])
      }
    }, ignoreNULL=FALSE)
    
    # ── Results table & plot servers ──────────────────────────────────────
    results_server("results", expression_data_table)

    volcano_server("volcano", volcano_data,
                   padj_thresh = sidebar_vals$padj_thresh,
                   lfc_thresh  = sidebar_vals$lfc_thresh_min,
                   gene        = reactive({
                     ds <- gated_active_dataset("Volcano")()
                     terms <- c(ds$genes, ds$proteins)
                     if (is.null(ds) || length(terms) == 0) return(NULL)
                     terms[nzchar(trimws(terms))]
                   }))

    heatmap_server("heatmap", gated_active_dataset("Heatmap"),
                   padj_thresh = sidebar_vals$padj_thresh,
                   lfc_thresh  = sidebar_vals$lfc_thresh_min,
                   n_genes     = reactive(20L))
    umap_server("umap", gated_active_dataset("UMAP"))

    violin_server("violin", violin_data,
                  selected_dataset = gated_active_dataset("Violin"),
                  padj_thresh = sidebar_vals$padj_thresh,
                  lfc_thresh  = sidebar_vals$lfc_thresh_min)
    
    feature_scatter_server("feature_scatter", gated_active_dataset("Feature Scatter"),
                           padj_thresh = sidebar_vals$padj_thresh,
                           lfc_thresh = sidebar_vals$lfc_thresh_min)

    histogram_server("histogram", gated_active_dataset("Histogram"), plot_table_data)

    dots_server("dots", gated_active_dataset("Dots Plot"))

    highest_expr_server("highest_expr", gated_active_dataset("Top Features"))

    signature_explorer_server("gde", drug_rank_adapter, gated_gene_drug_dataset,
                             sidebar_vals = sidebar_vals)
    
    # ── Value boxes (tied to dataset_stats) ─────────────────────────────
    # stats_row <- reactive({
    #   rows <- compare_source_rows()
    #   if (nrow(rows) == 0) return(list())

    #   list(
    #     dataset_count = nrow(rows),
    #     total_features = sum_numeric(rows$total_features),
    #     n_sig_features = sum_numeric(rows$n_sig_features),
    #     total_cells = sum_numeric(rows$total_cells),
    #     total_samples = sum_numeric(rows$total_samples),
    #     n_cell_types = length(flatten_json_values(rows$cell_types_json)),
    #     n_conditions = length(flatten_json_values(rows$conditions_json))
    #   )
    # })
    
    # output$box_datasets <- renderValueBox({
    #   row <- stats_row()
    #   count <- if (length(row) > 0) row$dataset_count else "—"
    #   valueBox(count, "Datasets selected", color = "purple")
    # })
    
    # output$box_genes <- renderValueBox({
    #   row <- stats_row()
    #   val <- if (length(row) > 0) row$total_features %||% "—" else "—"
    #   valueBox(val, "Total features", color = "blue")
    # })
    
    # output$box_sig_genes <- renderValueBox({
    #   row <- stats_row()
    #   val <- if (length(row) > 0) row$n_sig_features %||% "—" else "—"
    #   valueBox(val, "Significant (padj<0.05)", color = "red")
    # })
    
    # output$box_cells <- renderValueBox({
    #   row <- stats_row()
    #   val <- if (length(row) > 0) row$total_cells %||% "—" else "—"
    #   valueBox(val, "Total cells", icon = icon("circle-nodes"), color = "teal")
    # })
    
    # output$box_cell_types <- renderValueBox({
    #   row <- stats_row()
    #   val <- if (length(row) > 0) row$n_cell_types %||% "—" else "—"
    #   valueBox(val, "Cell types", icon = icon("tags"), color = "olive")
    # })
    
    # output$box_samples <- renderValueBox({
    #   row <- stats_row()
    #   val <- if (length(row) > 0) row$total_samples %||% "—" else "—"
    #   valueBox(val, "Samples", icon = icon("vials"), color = "navy")
    # })
    
    # output$box_conditions <- renderValueBox({
    #   row <- stats_row()
    #   val <- if (length(row) > 0) row$n_conditions %||% "—" else "—"
    #   valueBox(val, "Conditions", color = "maroon")
    # })
    
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
