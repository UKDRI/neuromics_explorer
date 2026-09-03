# adapters/adapter_drug_panel.R — owns data loading, plotly_click, lasso, focal-views plot builders

box::use(
  shiny[NS, moduleServer, bindCache, reactive, reactiveVal, observe, observeEvent, req,
        tagList, fluidRow, column, checkboxInput, checkboxGroupInput, radioButtons, selectizeInput,
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
        uiOutput(ns("steps_guide")),
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
        uiOutput(ns("gene_drug_table_heading")),   # heading only once a gene bubble is clicked
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
            head(terms[!is.na(terms) & nzchar(terms)], 25L)   # cap to 25 for performance
        })

        # Only the Webber iPSC microglia drug screen is a drug assay today.
        # This will be scaled to include check for more lab_source/study_id pairs as more drug datasets are registered.
        is_drug_dataset <- function(ds) !is.null(ds) && identical(ds$lab_source, "webber")

        contrast_opts <- reactive({
            ds <- dataset()
            req(ds) #,identical(ds$lab_source, "webber")
            validate(need(is_drug_dataset(ds), "Please select just one drug-study dataset to use the Gene-Drug Explorer."))     # prevents raw 404s
            fetch_contrast_options(ds$lab_source, ds$study_id)
        })

        # ── Guide ──────────────────────────────────────────────
        # Each step is ticked off once satisfied, and the next one becomes the active (bold) instruction.
        output$steps_guide <- renderUI({
            has_gene            <- length(active_goi()) > 0
            has_drug            <- length(selected_drugs()) > 0
            # Step 1 (filtering by contrasts) is optional also counts as done once the user has moved straight to Step 2 (selecting gene bubble)
            has_contrast        <- length(input$condition) > 0 || length(input$timepoint) > 0 || has_gene

            steps <- list(
                list(done = has_contrast,
                    text = tagList(
                        "Narrow the overview by the available contrasts such as: ", tags$b("condition"), " or ", tags$b("timepoint"),
                        ", or leave them clear to summarise across all contrasts.")),
                list(done = has_gene,
                    text = tagList(
                        "Click a gene bubble to list every drug tested against it. ",
                        "Higher bubbles have a larger fraction of significant drug hits.")),
                list(done = has_drug,
                    text = tagList(
                        "Pick one gene x drug row in the table to open the focal views for that drug.")),
                list(done = FALSE,      # Final step (changing focal contrast buttons from defaults) never ticks and remains bold
                    text = tagList(
                        "Choose a single contrast beneath the table to compare your genes of interest ",
                        "(heatmap) against the drug's whole transcriptome (volcano)."))
            )

            # Finds which step the user is currently on
            active_idx <- which(!vapply(steps, `[[`, logical(1), "done"))[1]    # pulls `done` flag from each step into a logical vector, e.g. c(TRUE, TRUE, FALSE, FALSE); `!` flips so which()[1] returns the first position of outstanding steps, e.g. step 3 in this example
            if (is.na(active_idx)) active_idx <- length(steps)      # NA means every step is done so hold highlight on final step rather than indexing out of bounds

            tags$div(
                class = "gene-drug-guide", role = "note",
                tags$div(style = "font-weight:700; margin-bottom:6px;",
                    "How to explore gene-drug relationships"),
                # Originally used custom.css file for styling (w .step-* classes) but issue with syncing w UI therefore inline styling used instead
                    #   done    -> faded and strike-through text, completed steps
                    #   active  -> bold, the current instruction to follow
                    #   todos   -> dimmed/opaque, steps to come that aren't yet reachable
                lapply(seq_along(steps), function(i) {
                    done   <- steps[[i]]$done
                    active <- (i == active_idx)
                    row_style <- paste0(
                        "display:flex; gap:8px; align-items:baseline; padding:3px 0; line-height:1.45;",
                        if (done) " opacity:0.55;" else if (active) " opacity:1; font-weight:600;" else " opacity:0.7;"
                    )
                    tags$div(
                        style = row_style,
                        tags$span(style = "flex:0 0 auto; width:1.25rem;",
                            if (done) "✓" else paste0(i, ".")),
                        tags$span(
                            style = if (done) "text-decoration:line-through;" else "",
                            steps[[i]]$text)
                    )
                })
            )
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
        # "Back to Landscape View" function is a full undo back to solely the bubble plot: clears gene selection to hide DT + its
        # heading, and hide focal panel (volcano + heatmap) and its contrast radios. The focal panel is also cleared when the user
        # clicks a different gene bubble, which triggers a new DT query and refreshes the volcano/heatmap for the new drug selection.
        observeEvent(input$back_to_landscape, {
            active_goi(character(0))
            selected_drugs(NULL)
            dataTableProxy("gene_drug_table") |> selectRows(NULL)
        })


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
            df$contrast <- paste(df$condition, df$timepoint, sep = " · ")
            display <- df[, c("gene_symbol", "drug_name", "drug_class", "contrast", "log2fc", "padj", "entity_id")]
            display$log2fc <- round(display$log2fc, 3)
            display$padj <- signif(display$padj, 3)
            names(display) <- c("Gene", "Drug", "Class", "Contrast", "log2FC", "padj", "Drug ID#")
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

        # New gene selection in bubble plot requires resetting gene x drug DT and removing selected drug and the stale focal plots; guide also falls back to step 3
        observeEvent(active_goi(), {
            selected_drugs(NULL)
            dataTableProxy("gene_drug_table") |> selectRows(NULL)
        }, ignoreInit = TRUE)

        output$gene_drug_table_heading <- renderUI({
            req(length(active_goi()) > 0)
            tags$h5("Drugs tested against selected gene(s)",
                style = "margin-top:20px;font-size:24px;font-weight:600;")
        })

        # TODO: multiple drug selections
        # TODO: consider 2x2 subplot grid for all contrasts vs radio buttons
        # TODO: fix initial heatmap collapsing multiple contrasts for the selected drug; consider combining drug_name w contrast into a composite x-axis label vs radio buttons


        # ── Focal panel: volcano (single drug only) + heatmap (any number) ────────────────

        # Drives (goi-drug)-paired plots (ie heatmap)
        # Drugs selected in DT subsets the volcano superset (full_data) for the selected drug
        focal_data <- reactive({
            df <- full_data()
            goi <- toupper(goi_terms())
            df[toupper(df$gene_symbol) %in% goi, , drop = FALSE]   # case-insensitive matches the search endpoints
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
            req(length(selected_drugs()) > 0, input$focal_condition, input$focal_timepoint)
            ds <- dataset()
            df <- fetch_expression_signatures(
                ds$lab_source, ds$study_id, selected_drugs(),
                condition = input$focal_condition, timepoint = input$focal_timepoint,
                limit = 100000L    # TODO: remove? # limit raised from the default 20k to prevent silent truncation of GOI from heatmap as one drug spans multiple contrasts
            )
            validate(need(nrow(df) > 0, "No expression data available for this drug with the selected contrast."))
            df$lab_source <- ds$lab_source %||% "unknown"           # volcano_server's bindCache needs these to prevent Error: object '' not found
            df$study_id   <- ds$study_id %||% NA_integer_
            df
        }) |> bindCache(
            dataset()$lab_source %||% "none",
            dataset()$study_id %||% "none",
            paste(selected_drugs() %||% "none", collapse = ","),
            input$focal_condition %||% "none",
            input$focal_timepoint %||% "none"
        )

        volcano_server("volcano", full_data,
            padj_thresh = sidebar_vals$padj_thresh,
            lfc_thresh  = sidebar_vals$lfc_thresh_min
        )

        # Focal plot contrast selector
        # NB. separate from the multi-select checkboxes at the top, which scope the bubble plot/DT
        output$focal_contrast_filter_ui <- renderUI({
            opts <- contrast_opts()
            req(nrow(opts) > 0)
            conds <- opts$conditions[[1]]
            tmps   <- opts$timepoints[[1]]
            wellPanel(
                fluidRow(
                    column(6, radioButtons(ns("focal_condition"), "Contrast condition:",
                        choices = conds, selected = conds[1], inline = TRUE)),
                    column(6, radioButtons(ns("focal_timepoint"), "Contrast timepoint:",
                        choices = tmps, selected = tmps[1], inline = TRUE))
                )
            )
        })

        # Render focal panel
        output$focal_panel <- renderUI({
            req(selected_drugs())
            tagList(
                uiOutput(ns("focal_contrast_filter_ui")),
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
