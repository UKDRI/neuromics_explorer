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
  shiny[...],
  shinydashboard[valueBoxOutput, renderValueBox, valueBox],
  shinyjs[runjs],
  bslib[...],
  plotly[plotlyOutput, renderPlotly, plot_ly, layout, add_trace],
  DT[DTOutput, renderDT, datatable],
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
  app/view/pages/gene_dataset_selector[gene_selector_ui, gene_selector_server],
  app/view/pages/explore_sidebar[sidebar_ui, sidebar_server],
  app/logic/api/api_client[fetch_all_datasets, fetch_expression_table, fetch_expression_volcano,
                           fetch_expression_multi_dataset, fetch_expression_histogram,
                           fetch_expression_groups, fetch_expression_goi,
                           fetch_dataset_embeddings, fetch_top_de,
                           fetch_metadata_filter_options],
  utils[head],
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
                  uiOutput(ns("compare_ui"))
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

    clean_terms <- function(values, max_terms = 15L) {
      if (is.null(values) || length(values) == 0) return(character(0))
      values <- unique(trimws(as.character(values)))
      values <- values[!is.na(values) & nzchar(values)]
      utils::head(values, max_terms)
    }

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

    build_link_selected_payload <- function(params) {
      all_rows <- tryCatch(
        fetch_all_datasets(),
        error = function(e) data.frame()
      )
      if (nrow(all_rows) == 0) return(NULL)

      rows <- all_rows
      if (!is.null(params$lab_source) && nzchar(params$lab_source)) {
        rows <- rows[tolower(rows$lab_source) == tolower(params$lab_source), , drop = FALSE]
      }
      if (!is.null(params$study_id) && !is.na(params$study_id)) {
        rows <- rows[rows$study_id == params$study_id, , drop = FALSE]
      } else {
        return(NULL)
      }

      if (nrow(rows) == 0) return(NULL)

      rows <- rows[order(rows$lab_source, rows$study_id), , drop = FALSE]
      rows$matched_genes <- if (length(params$genes) > 0) {
        paste(params$genes, collapse = ", ")
      } else {
        ""
      }
      rows$matched_proteins <- if (length(params$proteins) > 0) {
        paste(params$proteins, collapse = ", ")
      } else {
        ""
      }

      active_row <- rows[1, , drop = FALSE]
      list(
        genes = params$genes,
        proteins = params$proteins,
        lab_source = active_row$lab_source[1],
        study_id = active_row$study_id[1],
        dataset_name = active_row$dataset_name[1],
        omic_type = active_row$omic_type[1],
        selected_datasets = rows,
        cell_types = load_active_cell_types(
          active_row$lab_source[1],
          active_row$study_id[1]
        )
      )
    }

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

    build_de_category <- function(df, padj_thresh, lfc_thresh) {
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

      dplyr::case_when(
        !is.na(df$padj) & df$padj < padj_thresh & !is.na(df$log2fc) & df$log2fc > lfc_thresh ~ "Up",
        !is.na(df$padj) & df$padj < padj_thresh & !is.na(df$log2fc) & df$log2fc < -lfc_thresh ~ "Down",
        TRUE ~ "No"
      )
    }

    compare_violin_group <- function(df, padj_thresh, lfc_thresh) {
      if (all(c("padj", "log2fc") %in% names(df)) &&
          any(!is.na(df$padj)) &&
          any(!is.na(df$log2fc))) {
        return(list(
          name = "DE category",
          values = build_de_category(df, padj_thresh, lfc_thresh)
        ))
      }

      for (candidate in c("cluster_id", "cell_type", "condition_a", "condition_b",
                          "sample_a", "sample_b", "tissue", "sex", "age", "cell_id")) {
        if (has_values(df, candidate)) {
          values <- as.character(df[[candidate]])
          values[is.na(values) | !nzchar(values)] <- "unlabelled"
          return(list(name = candidate, values = values))
        }
      }

      list(name = "dataset", values = rep("all", nrow(df)))
    }

    compare_group_col <- function(df) {
      for (candidate in c("de_category", "cell_type", "cluster_id", "condition_a", "condition_b",
                          "sample_a", "sample_b", "tissue", "sex", "age", "cell_id")) {
        if (has_multiple_values(df, candidate)) {
          return(candidate)
        }
      }
      for (candidate in c("de_category", "cell_type", "cluster_id", "condition_a", "condition_b",
                          "sample_a", "sample_b", "tissue", "sex", "age", "cell_id")) {
        if (has_values(df, candidate)) {
          return(candidate)
        }
      }
      if ("gene_symbol" %in% names(df)) {
        return("gene_symbol")
      }
      if ("obs" %in% names(df)) {
        return("obs")
      }
      names(df)[1]
    }

    render_compare_volcano <- function(df, dataset_name, padj_thresh, lfc_thresh, gene = NULL) {
      validate(need(nrow(df) > 0, "No volcano rows are available for this dataset."))
      plot_df <- df |>
        dplyr::mutate(
          neg_log10p = -log10(pmax(pvalue, 1e-300)),
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
      hover_text <- paste0(
        "<b>", plot_df$feature_label, "</b><br>",
        "log2FC: ", round(x_values, 3), "<br>",
        "padj: ", signif(plot_df$padj, 3)
      )

      p <- plotly::plot_ly(
        x = x_values,
        y = y_values,
        type = "scatter",
        mode = "markers",
        color = plot_df$sig,
        colors = c(Up = "#C0392B", Down = "#2980B9", NS = "#BDC3C7"),
        text = hover_text,
        hoverinfo = "text",
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
          yaxis = list(title = "-log10(p-value)"),
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

    render_compare_violin <- function(df, dataset_name, padj_thresh, lfc_thresh) {
      group_info <- compare_violin_group(df, padj_thresh, lfc_thresh)
      plot_df <- df |>
        dplyr::mutate(
          group_value = group_info$values,
          feature_label = feature_labels(df),
          sig = dplyr::case_when(
            !is.na(padj) & padj < padj_thresh & log2fc >  lfc_thresh ~ "Up",
            !is.na(padj) & padj < padj_thresh & log2fc < -lfc_thresh ~ "Down",
            TRUE ~ "NS"
          )
        )
      plot_df <- plot_df[!is.na(plot_df$log2fc), , drop = FALSE]
      validate(need(nrow(plot_df) > 0, "No log2FC values are available for this dataset."))

      group_levels <- unique(plot_df$group_value)
      p <- plotly::plot_ly()
      for (group_name in group_levels) {
        group_df <- plot_df[plot_df$group_value == group_name, , drop = FALSE]
        p <- p |>
          plotly::add_trace(
            x = rep(group_name, nrow(group_df)),
            y = group_df$log2fc,
            type = "violin",
            name = group_name,
            box = list(visible = TRUE),
            meanline = list(visible = TRUE),
            points = FALSE,
            line = list(color = "#D7DEE5"),
            fillcolor = "rgba(215, 222, 229, 0.45)",
            hovertemplate = paste0(
              "<b>", group_name, "</b><br>log2FC: %{y:.3f}<extra></extra>"
            ),
            showlegend = FALSE
          )
      }

      hover_text <- paste0(
        "<b>", plot_df$feature_label, "</b><br>",
        group_info$name, ": ", plot_df$group_value, "<br>",
        "log2FC: ", round(plot_df$log2fc, 3), "<br>",
        "padj: ", signif(plot_df$padj, 3), "<br>",
        "class: ", plot_df$sig
      )

      p |>
        plotly::add_trace(
          x = plot_df$group_value,
          y = plot_df$log2fc,
          type = "scatter",
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
          xaxis = list(title = group_info$name, tickangle = -25),
          yaxis = list(title = "log2FC"),
          legend = list(title = list(text = "Significance"), orientation = "h",
                        y = -0.15),
          showlegend = TRUE,
          margin = list(t = 50)
        )
    }

    render_compare_heatmap <- function(df, dataset_row, dataset_name, padj_thresh, lfc_thresh) {
      
      # Append selected terms on heatmap where available
      ds <- selected_dataset()
      goi_terms <- unique(trimws(ds$genes %||% character(0)))
      goi_terms <- goi_terms[nzchar(goi_terms)]

      df_is_empty <- nrow(df) == 0
      
      if (length(goi_terms) > 0) {
        goi_df <- fetch_expression_goi(
          lab_source = dataset_row$lab_source[1],
          study_id   = dataset_row$study_id[1],
          genes      = goi_terms,
          limit      = 5000L
        )

        group_col <- compare_group_col(
          if (df_is_empty) goi_df else df
        )

        all_groups <- unique(c(
          df[[group_col]],
          goi_df[[group_col]]
        ))

        if (nrow(goi_df) > 0) {
          # Ensure grouping column exists

          if (!group_col %in% names(goi_df)) {
            # goi_df[[group_col]] <- df[[group_col]][1] #"GOI"  - check not forcing goi in wrong ccategory
            goi_df[[group_col]] <- goi_df[[group_col]] %||% goi_df$de_category %||% NA
          }

          # Align columns
          common_cols <- union(names(df), names(goi_df))
          for (col in setdiff(common_cols, names(df))) df[[col]] <- NA
          for (col in setdiff(common_cols, names(goi_df))) goi_df[[col]] <- NA

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
      group_levels <- all_groups  # group_levels <- unique(agg$group)
      mat <- matrix(
        NA_real_,
        nrow = length(gene_levels),
        ncol = length(group_levels),
        dimnames = list(gene_levels, group_levels)
      )
      row_idx <- match(agg$gene_symbol, gene_levels)
      col_idx <- match(agg$group, group_levels)
      mat[cbind(row_idx, col_idx)] <- agg$log2fc
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

    render_compare_histogram <- function(row) {
      df <- fetch_expression_histogram(
        lab_source = row$lab_source[1],
        study_id = row$study_id[1],
        metric = "log2fc",
        bins = 30L
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
          xaxis = list(title = "log2fc"),
          yaxis = list(title = "Row count"),
          margin = list(t = 50)
        )
    }

    render_compare_dots <- function(row, current_state) {
      validate(need(row$omic_type[1] %in% c("scrna", "snrna"),
                    "Dot plot is only available for scRNA-seq and snRNA-seq datasets."))

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
        type = "scatter",
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

    render_compare_feature_scatter <- function(row, current_state, padj_thresh, lfc_thresh) {
      terms <- unique(c(current_state$genes %||% character(0), current_state$proteins %||% character(0)))
      validate(need(length(terms) > 0, "Select one or more genes or proteins in the modal to compare GOI scatter plots."))

      df <- fetch_expression_goi(
        lab_source = row$lab_source[1],
        study_id = row$study_id[1],
        genes = current_state$genes %||% character(0),
        proteins = current_state$proteins %||% character(0),
        limit = 2000L
      )
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
        type = "scatter",
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

    render_compare_umap <- function(row, current_state) {
      validate(need(row$omic_type[1] %in% c("scrna", "snrna"),
                    "Embedding plots are only available for scRNA-seq and snRNA-seq datasets."))

      df <- fetch_dataset_embeddings(
        lab_source = row$lab_source[1],
        study_id = row$study_id[1],
        reduction = "umap",
        assay = "logcounts",
        genes = current_state$genes %||% character(0),
        proteins = current_state$proteins %||% character(0),
        max_points = 50000L
      )
      validate(need(nrow(df) > 0, "No embedding coordinates are available for this dataset."))

      if ("term" %in% names(df) && any(!is.na(df$term) & nzchar(df$term))) {
        first_term <- unique(df$term[!is.na(df$term) & nzchar(df$term)])[1]
        df <- df[df$term == first_term, , drop = FALSE]
        color_values <- df$expression_value
        color_scale <- c("#F7FBFF", "#6BAED6", "#08306B")
        color_title <- first_term
      } else {
        color_col <- compare_group_col(df)
        color_values <- as.character(df[[color_col]])
        color_scale <- NULL
        color_title <- color_col
      }

      plotly::plot_ly(
        x = df$dim_1,
        y = df$dim_2,
        type = "scatter",
        mode = "markers",
        color = color_values,
        colors = color_scale,
        marker = list(size = 4, opacity = 0.7),
        text = paste0("obs: ", df$obs),
        hoverinfo = "text"
      ) |>
        plotly::layout(
          title = list(text = paste(row$dataset_name[1], "·", color_title), x = 0.02),
          xaxis = list(title = "UMAP 1"),
          yaxis = list(title = "UMAP 2"),
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

    # ── Datasets & sub-modules ────────────────────────────────────────────
    # selected_dataset holds: list(
    #   genes, lab_source, study_id, dataset_name, omic_type,
    #   selected_datasets, cell_types
    # )
    selected_dataset <- reactiveVal(NULL)
    gene_selector_server("gene_selector", selected_dataset)
    sidebar_vals <- sidebar_server("filters", selected_dataset)

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
        lab_source = link$lab_source %||% NULL,
        study_id = link$study_id %||% NULL,
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

    # ── Plot area UI follows the sidebar plot selector while the active dataset
    # row in Dataset Listings controls which dataset is rendered. ────────────
    # This render block swaps the single-dataset plot module based on the
    # sidebar plot selector while keeping the active dataset row in control.
    output$plot_ui <- renderUI({
      plot_type <- sidebar_vals$plot_type()
      requires_single_cell <- plot_type %in% c("UMAP", "Dots Plot", "Top Features")

      if (requires_single_cell &&
          !isTRUE(selected_dataset()$omic_type %in% c("scrna", "snrna"))) {
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
        paste(ds$dataset_name, "·", ds$omic_type),
        tags$br(),
        tags$small("Preview loads the searched terms sorted by padj.")
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
        paste(ds$dataset_name, "·", ds$omic_type)
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

    # This reactive fetches the combined DE / expression rows for every dataset
    # currently selected in Dataset Listings. It powers the Expression data tab.
    expression_data <- reactive({
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
    })

    # ── Plot-specific data reactives ──────────────────────────────────────
    # Volcano gets a lean payload; violin uses a wider but still paginated
    # table slice; the other plot modules fetch their own purpose-built shapes.
    volcano_data <- reactive({
      ds <- selected_dataset()
      req(ds)

      fetch_expression_volcano(
        lab_source = ds$lab_source,
        study_id = ds$study_id,
        limit = 20000L,
        offset = 0L
      )
    })

    violin_data <- reactive({
      ds <- selected_dataset()
      req(ds)

      fetch_expression_table(
        lab_source = ds$lab_source,
        study_id = ds$study_id,
        limit = 3000L,
        offset = 0L,
        sort_by = "padj",
        sort_dir = "asc"
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
                fetch_top_de(
                  lab_source = row$lab_source[1],
                  study_id = row$study_id[1],
                  n = 20L,
                  padj_thresh = sidebar_vals$padj_thresh(),
                  lfc_thresh = sidebar_vals$lfc_thresh_min()
                ),
                row,
                row$dataset_name[1],
                sidebar_vals$padj_thresh(),
                sidebar_vals$lfc_thresh_min()
              ),
              Violin = render_compare_violin(
                fetch_expression_table(
                  lab_source = row$lab_source[1],
                  study_id = row$study_id[1],
                  limit = 3000L,
                  offset = 0L,
                  sort_by = "padj",
                  sort_dir = "asc"
                ),
                row$dataset_name[1],
                sidebar_vals$padj_thresh(),
                sidebar_vals$lfc_thresh_min()
              ),
              Histogram = render_compare_histogram(row),
              "Dots Plot" = render_compare_dots(row, current_state),
              "Top Features" = render_compare_top_features(row, current_state),
              "Feature Scatter" = render_compare_feature_scatter(
                row, current_state,
                sidebar_vals$padj_thresh(),
                sidebar_vals$lfc_thresh_min()
              ),
              UMAP = render_compare_umap(row, current_state),
              render_compare_volcano(
                fetch_expression_volcano(
                  lab_source = row$lab_source[1],
                  study_id = row$study_id[1],
                  limit = 20000L,
                  offset = 0L
                ),
                row$dataset_name[1],
                sidebar_vals$padj_thresh(),
                sidebar_vals$lfc_thresh_min(),
                gene = selected_dataset()$genes
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
            "Once the dataset table appears below, tick one or more rows to drive the explorer. Checked rows drive the Expression and Compare tabs, and the most recently checked row becomes the active dataset for the single-dataset Plot tab."
          )
        ))
      }

      tagList(
        tags$div(
          class = "alert alert-info",
          role = "alert",
          style = "margin-bottom: 12px;",
          tags$strong("NB: "),
          "Tick rows to include datasets in the explorer. The most recently ticked row drives the single-dataset Plot tab, while two or more checked rows activate Compare."
        ),
        uiOutput(session$ns("dataset_listing_table"))
      )
    })

    output$dataset_listing_table <- renderUI({
      ds <- selected_dataset()
      req(ds, !is.null(ds$selected_datasets), nrow(ds$selected_datasets) > 0)

      rows <- ds$selected_datasets
      checked_rows <- listing_selection()
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

      tags$div(
        style = "overflow-x: auto;",
        tags$table(
          class = "table table-striped table-hover align-middle",
          style = "margin-bottom: 0;",
          tags$thead(
            tags$tr(
              tags$th("Select", style = "width: 72px;"),
              lapply(names(display), tags$th)
            )
          ),
          tags$tbody(
            lapply(seq_len(nrow(display)), function(i) {
              tags$tr(
                tags$td(
                  checkboxInput(
                    session$ns(paste0("dataset_row_", i)),
                    label = NULL,
                    value = i %in% listing_selection(),
                    width = NULL
                  ),
                  style = "min-width: 64px;"
                ),
                lapply(display[i, , drop = FALSE], function(value) {
                  tags$td(as.character(value %||% ""))
                })
              )
            })
          )
        )
      )
    })

    # Reset checkbox state whenever the modal confirms a fresh dataset list.
    observe({
      ds <- selected_dataset()
      req(ds, !is.null(ds$selected_datasets), nrow(ds$selected_datasets) > 0)

      dataset_keys <- dataset_key(ds$selected_datasets)
      if (!identical(dataset_keys, last_dataset_keys())) {
        last_dataset_keys(dataset_keys)
        listing_selection(seq_len(nrow(ds$selected_datasets)))
      }
    })

    # Simple Shiny checkbox inputs drive dataset inclusion. The newest checked
    # row becomes active for the single-dataset Plot tab; checked rows as a
    # whole drive Expression and Compare.
    observe({
      current <- selected_dataset()
      req(current, !is.null(current$selected_datasets), nrow(current$selected_datasets) > 0)

      row_ids <- seq_len(nrow(current$selected_datasets))
      if (!all(vapply(row_ids, function(i) !is.null(input[[paste0("dataset_row_", i)]]), logical(1)))) {
        return(invisible(NULL))
      }

      row_idx <- which(vapply(row_ids, function(i) isTRUE(input[[paste0("dataset_row_", i)]]), logical(1)))
      previous_idx <- isolate(listing_selection())
      if (identical(row_idx, previous_idx)) {
        return(invisible(NULL))
      }

      listing_selection(row_idx)
      if (length(row_idx) == 0) {
        return(invisible(NULL))
      }

      active_idx <- which(
        current$selected_datasets$lab_source == current$lab_source &
        current$selected_datasets$study_id == current$study_id
      )[1]

      newly_checked <- setdiff(row_idx, previous_idx)
      target_idx <- if (length(newly_checked) > 0) {
        utils::tail(newly_checked, 1)
      } else if (length(active_idx) == 0 || is.na(active_idx) || !(active_idx %in% row_idx)) {
        row_idx[1]
      } else {
        NA_integer_
      }

      if (!is.na(target_idx)) {
        row <- current$selected_datasets[target_idx, , drop = FALSE]
        selected_dataset(set_active_dataset(current, row))
      }
    })
    
    # ── Results table & plot servers ──────────────────────────────────────
    results_server("results", expression_data)
    volcano_server("volcano",  volcano_data,
                   padj_thresh = sidebar_vals$padj_thresh,
                   lfc_thresh  = sidebar_vals$lfc_thresh_min,
                   gene        = reactive({
                     ds <- selected_dataset()
                     terms <- c(ds$genes, ds$proteins)
                     if (is.null(ds) || length(terms) == 0) return(NULL)
                     terms[nzchar(trimws(terms))]
                   }))
    heatmap_server("heatmap",  selected_dataset,
                   padj_thresh = sidebar_vals$padj_thresh,
                   lfc_thresh  = sidebar_vals$lfc_thresh_min,
                   n_genes     = reactive(20L))
    umap_server("umap",       selected_dataset)
    violin_server("violin",    violin_data,
                  selected_dataset = selected_dataset,
                  padj_thresh = sidebar_vals$padj_thresh,
                  lfc_thresh  = sidebar_vals$lfc_thresh_min)
    
    # ── New plot modules ──────────────────────────────────────────────────
    feature_scatter_server("feature_scatter", selected_dataset,
                           padj_thresh = sidebar_vals$padj_thresh,
                           lfc_thresh = sidebar_vals$lfc_thresh_min)
    histogram_server("histogram", selected_dataset)
    dots_server("dots", selected_dataset)
    highest_expr_server("highest_expr", selected_dataset)
    
    # ── Value boxes (tied to dataset_stats) ─────────────────────────────
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
