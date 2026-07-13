# adapters/adapter_drug_panel.R — owns data loading, plotly_click, lasso, focal-views plot builders

box::use(
  shiny[NS, moduleServer, reactive, reactiveVal, observeEvent, req,
        tagList, fluidRow, column, radioButtons, checkboxInput, checkboxGroupInput, sliderInput,
        renderUI, uiOutput, textOutput, renderText, tags, wellPanel],
  plotly[plotlyOutput, renderPlotly, plot_ly, add_markers, add_segments, layout, event_register, event_data],
#   DT[DTOutput, renderDT, datatable],
  app/logic/api/api_client[fetch_gene_rank, fetch_contrast_options, fetch_expression_table],
  app/view/components/helpers/explorer_helpers[breadcrumb_ui, selection_count_text],
  app/view/components/expression_heatmap[heatmap_ui, heatmap_server],
  app/view/components/results_table[results_ui, results_server],
  app/view/components/volcano_plot[volcano_ui, volcano_server],
  utils[head]
)


# This is the gene-first route to visualise the Gene-Drug Explorer's panel as a ranked lollipop
drug_rank_adapter <- list(
  entity_name = "Drug",

  ui = function(id) {
    ns <- NS(id)
    tagList(
        fluidRow(
            column(3, uiOutput(ns("breadcrumb"))),
        ),
        
        wellPanel(
            fluidRow(
                column(6, uiOutput(ns("contrast_filter_ui"))),
                column(3, sliderInput(ns("top_n"), "Show top N drugs", 5, 100, 25, step = 5)),
                column(3, checkboxInput(ns("scope_to_goi"), "Show only GOI", value = TRUE)), # value = length(input$goi) > 10 - prevent too many focal plots
            )
        ),
        tags$h2("Ranking drugs affecting selected genes of interest", style="font-size:28px;font-weight:600;"),
        plotlyOutput(ns("lollipop"), height = "680px"),
        textOutput(ns("selection_summary")),
        uiOutput(ns("focal_panel"))
    )
  },

  server = function(id, dataset, sidebar_vals = NULL) {
    moduleServer(id, function(input, output, session) {
        ns <- session$ns

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

        # output$goi_display <- renderUI({
        #     terms <- goi_terms()
        #     req(length(terms) > 0)
        #         # tagList(
        #         #     tags$label("Ranking drugs by effect on:", style = "font-weight:600;"),
        #         #     tags$div(lapply(terms, function(t) tags$span(t, class = "badge",
        #         #     style = "background:#667eea;color:white;margin-right:4px;padding:4px 8px;border-radius:10px;")))
        #         # )
        #     tags$div(
        #         style="display:flex;align-items:center;gap:10px;flex-wrap:wrap;margin-bottom:10px;",
        #         tags$span("Ranking drugs affecting", style="font-size:28px;font-weight:600;"),
        #         lapply(terms, function(t)
        #             tags$span(
        #                 t, class="badge",
        #                 style="background:#667eea;color:white;padding:5px 10px;border-radius:12px;font-size:15px;"
        #             )
        #         )
        #     )
        # })


        # Server-side guard for "too many GOI selected" — the observer auto-enables it 
        # once GOI count crosses the threshold
        observeEvent(goi_terms(), {
            if (length(goi_terms()) > 10 && !isTRUE(input$scope_to_goi)) {
            shiny::updateCheckboxInput(session, "scope_to_goi", value = TRUE)
            }
        })

        ranked <- reactive({
            ds <- dataset()
            req(ds, length(goi_terms()) > 0)
            fetch_gene_rank(
                lab_source  = ds$lab_source, study_id = ds$study_id,
                genes       = goi_terms(),
                padj_thresh = sidebar_vals$padj_thresh(),
                top_n       = input$top_n,
                condition   = input$condition, # NULL
                timepoint   = input$timepoint
            )
        })

        selected_drugs <- reactiveVal(NULL)

        contrast_opts <- reactive({
            ds <- dataset()
            req(ds) #,identical(ds$lab_source, "webber")
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

        output$lollipop <- renderPlotly({
            df <- ranked()
            req(nrow(df) > 0)
            df$entity_id <- factor(df$entity_id, levels = rev(df$entity_id))  # preserve server-side rank order, reverse orientation so top-ranked drug at the top 
            plot_ly(source = "generank") |>
            add_segments(data = df, x = 0, xend = ~sig_score, y = ~entity_id, yend = ~entity_id,
                line = list(color = "#BDC3C7"), showlegend = FALSE, hoverinfo = "skip") |>
            add_markers(data = df, x = ~sig_score, y = ~entity_id,
                marker = list(size = 10, color = "#2980B9"), #type = "scattergl",
                text = ~paste0(drug_name, " (", drug_class, ")<br>#GOI hit: ", n_goi_hit, "/", n_goi_total),
                hoverinfo = "text") |>
            layout(xaxis = list(title = "Combined signature score"), yaxis = list(title = ""), dragmode = "select") |>   #layout(dragmode = "lasso") 
            event_register("plotly_click") |> event_register("plotly_selected")
        })

        # Bridge between plot clicks and the server retrieving the data (for that selection) - enables cross-talk and event listening
        observeEvent(event_data("plotly_click", source = "generank"), {
            click <- event_data("plotly_click", source = "generank")
            req(click)
            selected_drugs(click$y)  # extracts y axis value (drug id - factor label)
        })
        observeEvent(event_data("plotly_selected", source = "generank"), {
            sel <- event_data("plotly_selected", source = "generank")
            req(sel)
            selected_drugs(unique(sel$y))   # box-select on a categorical y axis extracts multiple drug ids
        })

        output$selection_summary <- renderText(selection_count_text(length(selected_drugs()), "drug(s)"))

        output$breadcrumb <- renderUI({
            req(length(selected_drugs()) > 0)
            breadcrumb_ui(ns)
        })
        observeEvent(input$back_to_landscape, selected_drugs(character(0))) #NULL

        # entity_col/ entity_values filters expression/table to just the clicked drug
        focal_data <- reactive({
            req(length(selected_drugs()) > 0)
            ds <- dataset()
            goi <- goi_terms()
            fetch_expression_table(
                lab_source = ds$lab_source, study_id = ds$study_id,
                entity_id = selected_drugs(),
                genes = if (isTRUE(input$scope_to_goi)) goi else NULL,  # NULL = all genes for this drug
                limit = 5000L
            )
        })

        output$focal_panel <- renderUI({
            req(selected_drugs())
            tagList(
                volcano_ui(ns("volcano")),
                plotlyOutput(ns("heatmap")),
                results_ui(ns("gene_table"))
            )
        })

        volcano_server("volcano", focal_data,
            padj_thresh = sidebar_vals$padj_thresh,
            lfc_thresh  = sidebar_vals$lfc_thresh_min
        )

        output$heatmap <- renderPlotly({
            df <- focal_data()
            req(nrow(df) > 0)
            plot_ly(df, x = ~entity_id, y = ~gene_symbol, z = ~log2fc, type = "heatmap",
                colorscale = list(c(0,"#2980B9"), c(0.5,"#FFFFFF"), c(1,"#C0392B")), zmid = 0,
                hovertemplate = "%{y} · %{x}<br>log2FC: %{z:.3f}<extra></extra>"
            )
        })

        results_server("gene_table", focal_data)
    })
  }
)

`%||%` <- function(a, b) if (!is.null(a)) a else b

# TODO ad lasso-results focal view
# upset, heatmap w dendrograms


# # This is the drug-first route to visualise the panel as a clustered scatter
# drug_panel_adapter <- list(
#   entity_name   = "Drug",                     # used in UI labels: "Selected: N drugs"
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
