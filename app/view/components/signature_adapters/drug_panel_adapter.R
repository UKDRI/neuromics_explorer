# adapters/adapter_drug_panel.R — owns data loading, plotly_click, lasso, focal-views plot builders

box::use(
  shiny[NS, moduleServer, bindCache, reactive, reactiveVal, observe, observeEvent, req,
        tagList, fluidRow, column, checkboxInput, checkboxGroupInput, selectizeInput,
        renderUI, uiOutput, textOutput, renderText, tags, updateSelectizeInput, wellPanel, validate, need],
  plotly[plotlyOutput, renderPlotly, plot_ly, layout, event_register, event_data],
  shinycssloaders[withSpinner],
  DT[DTOutput, renderDT, datatable, dataTableProxy, selectRows],
  app/logic/api/api_client[fetch_contrast_options, fetch_gene_drug_summary, fetch_gene_drug_pairs, fetch_expression_signatures, fetch_expression_table],
  app/view/components/helpers/explorer_helpers[breadcrumb_ui, selection_count_text],
  app/view/components/expression_heatmap[heatmap_ui, heatmap_server],
  app/view/components/volcano_plot[volcano_ui, volcano_server],
  utils[head]
)


# This is the gene-first route to visualise the Gene-Drug Explorer's main overview
drug_rank_adapter <- list(
  entity_name = "Drug",

  ui = function(id) {
    ns <- NS(id)
    tagList(
        wellPanel(
            fluidRow(
                column(6, uiOutput(ns("contrast_filter_ui"))),
                # column(3, selectizeInput(ns("drug_search"), "Search drug", choices = NULL, multiple = TRUE, 
                #     options = list(placeholder = "e.g. Imatinib...")))
                # column(3, checkboxInput(ns("sig_only"), "Count only significant hits (padj threshold)", value = TRUE))
            )
        ),
        tags$h3("Genes of interest - drug overview", style = "font-size:28px;font-weight:600;"),
        tags$p("Bubble size = drugs tested; height = fraction significant. Click a gene to see which drugs affect it.",
            style = "color:#666; font-size:13px;"),    #; colour = net direction bias
        fluidRow(
            column(3, uiOutput(ns("breadcrumb"))),
        ),
        plotlyOutput(ns("bubble_overview"), height = "680px") |> withSpinner(
            type = 1, caption = "Loading plot...", color = "#5b5b5b"),
        textOutput(ns("selection_summary")),
        tags$h5("Drugs tested against selected gene(s)", style = "margin-top:20px;font-size:24px;font-weight:600;"),
        DTOutput(ns("gene_drug_table")),
        uiOutput(ns("focal_panel"))
    )
  },

  server = function(id, dataset, sidebar_vals = NULL) {
    moduleServer(id, function(input, output, session) {
        ns <- session$ns

        # ── Thresholds, GOI terms, contrast filters ─────────────────────────────────────────────

        sidebar_vals <- sidebar_vals %||% list(
            padj_thresh = reactive(0.05),
            lfc_thresh_min = reactive(0)
        )

        goi_terms <- reactive({
            ds <- dataset()
            req(ds)
            terms <- unique(c(ds$genes %||% character(0), ds$proteins %||% character(0)))
            terms <- trimws(as.character(terms))
            head(terms[!is.na(terms) & nzchar(terms)], 25L)   # cap irregardless of how many were searched
        })

        # Only the Webber iPSC microglia drug screen is a drug assay today.
        # Expand this check (e.g. by adding more lab_source/study_id pairs) as
        # more drug datasets are registered.
        is_drug_dataset <- function(ds) !is.null(ds) && identical(ds$lab_source, "webber")

        contrast_opts <- reactive({
            ds <- dataset()
            req(ds) #,identical(ds$lab_source, "webber")
            validate(need(is_drug_dataset(ds), "Please select just one drug-study dataset to use the Gene-Drug Explorer."))
            fetch_contrast_options(ds$lab_source, ds$study_id)
        })

        output$contrast_filter_ui <- renderUI({
            opts <- contrast_opts()
            req(nrow(opts) > 0)
            tagList(
                # radioButtons(ns("condition"), "Condition", choices = opts$conditions[[1]], inline = TRUE),
                # radioButtons(ns("timepoint"), "Timepoint", choices = opts$timepoints[[1]], inline = TRUE)
                tags$label("Condition:", style = "font-weight:600; font-size:18px;"),
                checkboxGroupInput(ns("condition"), label = NULL,
                    choices = opts$conditions[[1]], selected = character(0), inline = TRUE),
                tags$label("Timepoint:", style = "font-weight:600; font-size:18px;"),
                checkboxGroupInput(ns("timepoint"), label = NULL,
                    choices = opts$timepoints[[1]], selected = character(0), inline = TRUE)

            )
        })

        # ── Lollipop plot ───────────────────────────────
        # selected_drugs <- reactiveVal(NULL)
        # ranked <- reactive({
        #     ds <- dataset()
        #     req(ds, length(goi_terms()) > 0)
        #     fetch_gene_rank(
        #         lab_source  = ds$lab_source, study_id = ds$study_id,
        #         genes       = goi_terms(),
        #         padj_thresh = sidebar_vals$padj_thresh(),
        #         top_n       = input$top_n,
        #         condition   = input$condition, # NULL
        #         timepoint   = input$timepoint
        #     )
        # })

        # output$lollipop <- renderPlotly({
        #     df <- ranked()
        #     req(nrow(df) > 0)
        #     df$entity_id <- factor(df$entity_id, levels = rev(df$entity_id))  # preserve server-side rank order, reverse orientation so top-ranked drug at the top 
        #     df$hit_frac  <- df$n_goi_hit / pmax(df$n_goi_total, 1)
            
        #     plot_ly(source = "generank") |>
        #     add_segments(data = df, x = 0, xend = ~sig_score, y = ~entity_id, yend = ~entity_id,
        #         line = list(color = "#D8DEE6", width = 2), showlegend = FALSE, hoverinfo = "skip") |>
        #     add_markers(data = df, x = ~sig_score, y = ~entity_id,
        #         marker = list(
        #             size        = ~pmax(n_goi_hit * 2, 10), #*6 #???
        #             color       = ~hit_frac,
        #             colorscale  = list(c(0,"#AEC6E8"), c(1,"#1F4E96")),
        #             showscale   = TRUE,
        #             colorbar    = list(title = "% GOI hit", tickformat = ".0%"),
        #             line        = list(color = "#FFFFFF", width = 1)
        #         ), #type = "scattergl",
        #         text = ~paste0(
        #             "<b>", drug_name, "</b> (", drug_class, ")<br>",
        #             "Score: ", round(sig_score, 2), "<br>",
        #             "Hits: ", n_goi_hit, "/", n_goi_total, "<br>"
        #             # "Genes: ", hit_genes  
        #             # TODO: hit_genes added to sql endpoint 'expression/gene-rank'
        #         ),
        #         hoverinfo = "text") |>
        #     layout(xaxis = list(title = "Signature score"), yaxis = list(title = ""), dragmode = "select", margin = list(l = 140)) |>   #layout(dragmode = "lasso") 
        #     event_register("plotly_click") |> event_register("plotly_selected")
        # })

        # # Bridge between plot clicks and the server retrieving the data (for that selection) - enables cross-talk and event listening
        # observeEvent(event_data("plotly_click", source = "generank"), {
        #     click <- event_data("plotly_click", source = "generank")
        #     req(click)
        #     selected_drugs(click$y)  # extracts y axis value (drug id - factor label)
        # })
        # observeEvent(event_data("plotly_selected", source = "generank"), {
        #     sel <- event_data("plotly_selected", source = "generank")
        #     req(sel)
        #     selected_drugs(unique(sel$y))   # box-select on a categorical y axis extracts multiple drug ids
        # })


        # ── Bubble plot overview: one bubble per GOI gene, aggregated across all tested drugs ──
        active_goi <- reactiveVal(NULL)
        bubble_data <- reactive({
            ds <- dataset()
            genes <- goi_terms()
            req(ds, length(genes) > 0)
            validate(need(is_drug_dataset(ds), "Please select just one drug-study dataset to have explore."))
            fetch_gene_drug_summary(
                lab_source  = ds$lab_source, study_id = ds$study_id,
                genes       = genes,
                padj_thresh = sidebar_vals$padj_thresh(),
                condition   = input$condition, # NULL
                timepoint   = input$timepoint
            )
        }) |> bindCache(
            dataset()$lab_source %||% "none",
            dataset()$study_id %||% "none",
            paste(goi_terms(), collapse = ","),
            sidebar_vals$padj_thresh(),
            paste(input$condition %||% character(0), collapse = ","),
            paste(input$timepoint %||% character(0), collapse = ",")
        )

        output$bubble_overview <- renderPlotly({
            df <- bubble_data()
            validate(need(nrow(df) > 0, "No drug-gene data available for the selected genes."))

            hover_text <- paste0(
                "<b>", df$gene_symbol, "</b><br>",
                "Tested in ", df$n_drugs_tested, " drugs<br>",
                "Significant drug hits: ", df$n_drugs_sig, " (", round(df$frac_sig * 100, 1), "%)<br>",
                "#Up: ", df$n_up, " · #Down: ", df$n_down, "<br>",
                "Net direction: ", ifelse(df$net_change > 0, paste0("+", df$net_change, " up-leaning"),
                    ifelse(df$net_change < 0, paste0(df$net_change, " down-leaning"), "balanced"))
            )

            plot_ly(df, x = ~gene_symbol, y = ~frac_sig, size = ~n_drugs_tested, color = ~net_change, colors = c("#398cc4", "#F5F5F5", "#bd594e"),
                type = "scattergl", mode = "markers", 
                marker = list(
                    sizemode = "area",
                    sizeref = max(df$n_drugs_tested) / 2500,
                    opacity = 0.85,
                    line = list(width = 1, color = "#888"), #color = "white" | color = "#D8DEE6", width = 2 | colorscale  = list(c(0,"#AEC6E8"), c(1,"#1F4E96")),
                    showscale = TRUE,
                    colorbar = list(title = "Significance")
                ),
                text = hover_text, hoverinfo = "text", showlegend = FALSE, source = "genebubble") |>
            layout(
                xaxis = list(title = "Genes"),
                yaxis = list(title = "Fraction of significant drugs"),  #list(title = "Fraction of drugs with significant hit", range = c(0, 1))
                tickformat = ".0%",
                dragmode = "select", margin = list(l = 140),
                showlegend = FALSE) |>   #layout(dragmode = "lasso") 
            event_register("plotly_click") |> event_register("plotly_selected")
        })

        # Bridge between plot clicks and the server retrieving the data (for that selection) - enables cross-talk and event listening
        observeEvent(event_data("plotly_click", source = "genebubble"), {
            click <- event_data("plotly_click", source = "genebubble")
            req(click)
            active_goi(click$x)  # extracts x axis value (genes - factor label)
        })
        observeEvent(event_data("plotly_selected", source = "genebubble"), {
            sel <- event_data("plotly_selected", source = "genebubble")
            req(sel)
            active_goi(unique(sel$x))   # box-select on a categorical y axis extracts multiple drug ids for the selected gene
            # df <- bubble_data()
            # active_goi(unique(df$gene_symbol[sel$pointNumber + 1]))  # lasso returns numeric axis positions for categorical x-axis, not the category label text
        })
        output$selection_summary <- renderText(selection_count_text(length(active_goi()), "gene"))

        output$breadcrumb <- renderUI({
            req(length(active_goi()) > 0)
            breadcrumb_ui(ns)
        })
        observeEvent(input$back_to_landscape, active_goi(character(0))) #NULL


        # ── Repopulate drug search box choices from whichever gene is currently active ───────
        observeEvent(gene_drug_paired_data(), {
            df <- gene_drug_paired_data()
            if (is.null(df) || nrow(df) == 0 || !"drug_name" %in% names(df)) return(invisible(NULL))
            drugs_df <- unique(df[!is.na(df$entity_id) & nzchar(df$entity_id), c("entity_id", "drug_name", "drug_class")])
            choices <- stats::setNames(drugs_df$entity_id, paste0(drugs_df$drug_name, " (", drugs_df$drug_class, ")"))
            updateSelectizeInput(session, "drug_search", choices = choices, server = TRUE)
        })

        # ── DT table for gene x drug pairs — fetched for subsequently clicked genes, not all GOI ───────
        # TODO consider observe block instead of reactive
        gene_drug_paired_data <- reactive({
            g <- active_goi(); req(length(g) > 0)
            ds <- dataset(); req(ds)
            fetch_gene_drug_pairs(ds$lab_source, ds$study_id, genes = g, sidebar_vals$padj_thresh(), input$condition, input$timepoint)
        }) |> bindCache(
            dataset()$lab_source %||% "none",
            dataset()$study_id %||% "none",
            active_goi() %||% "none",
            paste(input$condition %||% character(0), collapse = ","),
            paste(input$timepoint %||% character(0), collapse = ",")
        )

        output$gene_drug_table <- renderDT({
            df <- gene_drug_paired_data()
            validate(need(nrow(df) > 0, "No rows match the current gene or drug filters."))
            # Keep entity_id as reference for selection, but display as Drug ID#
            display <- df[, c("gene_symbol", "drug_name", "drug_class", "log2fc", "padj", "entity_id")]
            display$log2fc <- round(display$log2fc, 3)
            display$padj <- signif(display$padj, 3)
            names(display) <- c("Gene", "Drug", "Class", "log2FC", "padj", "Drug ID#")
            datatable(display, selection = "single", filter = "top", rownames = FALSE,
                options = list(pageLength = 10, scrollX = TRUE))    # scrollY = TRUE
        }, server = TRUE)

        # Now following drug selection from DT
        selected_drugs <- reactiveVal(NULL)
        observeEvent(input$gene_drug_table_rows_selected, {
            df <- gene_drug_paired_data()
            rows <- input$gene_drug_table_rows_selected
            selected_drugs(unique(df$entity_id[rows]))
        })

        # output$selection_summary <- renderText(selection_count_text(length(selected_drugs()), "drug"))
        # output$breadcrumb <- renderUI({
        #     req(length(selected_drugs()) > 0)
        #     breadcrumb_ui(ns)
        # })

        observeEvent(input$back_to_landscape, {
            dataTableProxy("gene_drug_table") |> selectRows(NULL)
        }) #observeEvent(input$back_to_landscape, selected_drugs(character(0)))

        # TODO when genes in plot is reselected, reset, the gene-drug table AND close focal plots to get ready for a refresh


        # ── Focal panel: volcano (single drug only) + heatmap (any number) ────────────────

        # Drives (goi-drug)-paired plots (ie heatmap)
        # Drugs selected in DT filters GOI-based 'expression/table' results
        focal_data <- reactive({
            req(length(selected_drugs()) > 0)
            ds <- dataset()
            df <- fetch_expression_table(
                lab_source = ds$lab_source, study_id = ds$study_id,
                genes = goi_terms() %||% NULL,  # NULL = all genes for this drug
                entity_id = selected_drugs() %||% NULL,
                limit = 5000L
            )
            df$lab_source <- ds$lab_source %||% "unknown" #NA_character_ 
            df$study_id   <- ds$study_id %||% NA_integer_ # NA_character_
            df
        })

        output$heatmap <- renderPlotly({
            df <- focal_data()
            validate(need(nrow(df) > 0, "No expression data available."))
            plot_ly(df, x = ~drug_name, y = ~gene_symbol, z = ~log2fc, type = "heatmap",
                customdata = ~padj,
                colorscale = list(c(0,"#2980B9"), c(0.5,"#FFFFFF"), c(1,"#C0392B")), zmid = 0,
                hovertemplate = "%{y} · %{x}<br>log2FC: %{z:.3f}<br>padj: %{customdata:.2e}<extra></extra>"
            )
        })

        # Drives full data plots (eg volcano) requiring all genes for selected entities
        full_data <- reactive({
            req(length(selected_drugs()) > 0)
            ds <- dataset()
            df <- fetch_expression_signatures(ds$lab_source, ds$study_id, selected_drugs())
            validate(need(nrow(df) > 0, "No expression data available."))
            df$lab_source <- ds$lab_source %||% "unknown"   # volcano_server's bindCache needs these to prevent Error: object '' not found
            df$study_id   <- ds$study_id %||% NA_integer_
            df
        })

        observe({
            df <- full_data()
            message("volcano rows = ", nrow(df))
        })
        observe({
            print(selected_drugs())
        }) #[1] "ID_1501198"

        volcano_server("volcano", full_data,
            padj_thresh = sidebar_vals$padj_thresh,
            lfc_thresh  = sidebar_vals$lfc_thresh_min
        )

        # Render focal panel
        output$focal_panel <- renderUI({
            req(selected_drugs())
            tagList(
                plotlyOutput(ns("heatmap")),
                volcano_ui(ns("volcano"))
            )
        })
    })
  }
)

`%||%` <- function(a, b) if (!is.null(a)) a else b

# TODO ad lasso-results focal view
# upset, heatmap w dendrograms


# # This is the drug-first route to visualise the panel as a clustered drug scatter
# drug_panel_adapter <- list(
#   entity_name   = "Drug Clusters",                     # used in UI labels: "Selected: N drugs"
#   ui = function(id) {
#     ns <- NS(id)
#     tagList(
#       fluidRow(
#         column(3, radioButtons(ns("colour_by"), "Colour by", c("Cluster", "Significance"))),
#         column(3, radioButtons(ns("size_by"), "Point size", c("#GOI" = "goi", "#sigDEGs" = "sig"), selected = "goi")),
#         column(3, checkboxInput(ns("goi_filter"), "Filter drugs by GOI", TRUE)) # FALSE
#       ),
#       uiOutput(ns("breadcrumb")),
#       plotlyOutput(ns("scatter"), height = "480px"),
#       textOutput(ns("selected_count")),
#       DTOutput(ns("summary_table")),
#       uiOutput(ns("focal_panel"))
#     )
#   },


#   server = function(id, dataset) {
#     moduleServer(id, function(input, output, session) {
#         ns <- session$ns

#         # embed_df <- reactive(fetch_drug_panel_embeddings(lab_source, study_id))
#         embed_df <- reactive({
#             ds <- dataset()
#             req(ds)
#             fetch_dataset_embeddings(
#             lab_source = ds$lab_source,
#             study_id   = ds$study_id,
#             reduction  = "umap",
#             assay      = "counts"
#             )
#         })
#         selected_drugs <- reactiveVal(character(0)) # holds current click/lasso selection

#         output$scatter <- renderPlotly({
#             df <- embed_df()
#             colour_var <- if (input$colour_by == "Cluster") df$cluster_id else df$n_sig_degs
#             plot_ly(df, x = ~dim_1, y = ~dim_2, color = colour_var,
#                     type = "scattergl", mode = "markers", text = ~obs, source = "drugscatter") |>
#             layout(dragmode = "lasso") |>
#             event_register("plotly_selected") |> event_register("plotly_click")
#             #   highlight(on = "plotly_selected", off = "plotly_deselect") |> event_register("plotly_click")
#                     #   plot_ly(sd, x = ~PC1, y = ~PC2, size = ~size_val, color = colour_var,
#                     #       type = "scattergl", mode = "markers",
#                     #       marker = list(sizemode = "area", sizeref = 0.5, opacity = 0.8),
#                     #       text = ~drug, hoverinfo = "text")
#         })

#         observeEvent(event_data("plotly_click", source = "drugscatter"), {
#             click <- event_data("plotly_click", source = "drugscatter")
#             req(click)
#             selected_drugs(embed_df()$obs[click$pointNumber + 1])
#         })
#         observeEvent(event_data("plotly_selected", source = "drugscatter"), {
#             sel <- event_data("plotly_selected", source = "drugscatter")
#             req(sel)
#             selected_drugs(embed_df()$obs[sel$pointNumber + 1])
#         })

#         output$selected_count <- renderText(selection_count_text(length(selected_drugs()), "drug"))

#         output$breadcrumb <- renderUI({
#             req(length(selected_drugs()) > 0)
#             breadcrumb_ui(ns)
#         })
#         observeEvent(input$back_to_landscape, selected_drugs(character(0)))


#         # # ---- selection handling: click (1 drug) vs lasso (many) --------------
#         # sd <- SharedData$new(scatter_data, key = ~drug, group = "drugpanel")
#         # click_data <- reactive(event_data("plotly_click", source = "A"))
#         # selected_keys <- reactive({
#         #     sel <- sd$origData()[sd$selection() %||% rep(FALSE, nrow(scatter_data())), ]
#         #     sel$drug
#         # })
#         # output$summary_table <- renderDT({
#         #     datatable(sd, options = list(pageLength = 5), rownames = FALSE)
#         # }, server = FALSE)

#         focal_data <- reactive({
#             req(length(selected_drugs()) > 0)
#             fetch_drug_panel_expression(lab_source, study_id, drug_ids = selected_drugs())
#         })

#         output$focal_panel <- renderUI({
#             n <- length(selected_drugs())
#             if (n == 0) return(NULL)
#             if (n == 1) tagList(plotlyOutput(ns("volcano")), plotlyOutput(ns("heatmap_single")))
#             else tagList(plotlyOutput(ns("heatmap_multi")), plotlyOutput(ns("upset_plot")))
#         })

        
#         output$volcano <- renderPlotly({
#             d <- focal_data()
#             plot_ly(d, x = ~log2fc, y = ~-log10(padj), text = ~gene_symbol,
#                     type = "scattergl", mode = "markers")
#         })
#         output$heatmap_single <- output$heatmap_multi <- renderPlotly({
#             d <- focal_data()
#             plot_ly(d, x = ~entity_id, y = ~gene_symbol, z = ~log2fc, type = "heatmap")
#         })
#         # upset_plot: stub — ComplexUpset/upsetjs
#         })
#     }
# )
