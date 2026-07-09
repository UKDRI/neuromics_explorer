# adapters/adapter_drug_panel.R — owns data loading, plotly_click, lasso, focal-views plot builders

box::use(
  shiny[NS, moduleServer, reactive, reactiveVal, observeEvent, req,
        tagList, fluidRow, column, radioButtons, checkboxInput, sliderInput,
        renderUI, uiOutput, textOutput, renderText],
  plotly[plotlyOutput, renderPlotly, plot_ly, layout, event_register, event_data],
  DT[DTOutput, renderDT, datatable],
  app/logic/api/api_client[fetch_drug_panel_embeddings, fetch_drug_panel_expression],
  app/view/components/explorer_helpers[breadcrumb_ui, selection_count_text],
)

drug_panel_adapter <- list(
  entity_name   = "Drug",                     # used in UI labels: "Selected: N drugs"
  ui = function(id) {
    ns <- NS(id)
    tagList(
      fluidRow(
        column(3, radioButtons(ns("colour_by"), "Colour by", c("Cluster", "Significance"))),
        column(3, radioButtons(ns("size_by"), "Point size", c("#GOI" = "goi", "#sigDEGs" = "sig"), selected = "goi")),
        column(3, checkboxInput(ns("goi_filter"), "Filter drugs by GOI", TRUE)) # FALSE
      ),
      uiOutput(ns("breadcrumb")),
      plotlyOutput(ns("scatter"), height = "480px"),
      textOutput(ns("selected_count")),
      DTOutput(ns("summary_table")),
      uiOutput(ns("focal_panel"))
    )
  },


  server = function(id, lab_source = "webber", study_id = 1) {
    moduleServer(id, function(input, output, session) {
        ns <- session$ns

        embed_df <- reactive(fetch_drug_panel_embeddings(lab_source, study_id))
        selected_drugs <- reactiveVal(character(0))

        output$scatter <- renderPlotly({
            df <- embed_df()
            colour_var <- if (input$colour_by == "Cluster") df$cluster_id else df$n_sig_degs
            plot_ly(df, x = ~dim_1, y = ~dim_2, color = colour_var,
                    type = "scatter", mode = "markers", text = ~obs, source = "drugscatter") |>
            layout(dragmode = "lasso") |>
            event_register("plotly_selected") |> event_register("plotly_click")
            #   highlight(on = "plotly_selected", off = "plotly_deselect") |> event_register("plotly_click")
                    #   plot_ly(sd, x = ~PC1, y = ~PC2, size = ~size_val, color = colour_var,
                    #       type = "scatter", mode = "markers",
                    #       marker = list(sizemode = "area", sizeref = 0.5, opacity = 0.8),
                    #       text = ~drug, hoverinfo = "text")
        })

        observeEvent(event_data("plotly_click", source = "drugscatter"), {
            click <- event_data("plotly_click", source = "drugscatter")
            req(click)
            selected_drugs(embed_df()$obs[click$pointNumber + 1])
        })
        observeEvent(event_data("plotly_selected", source = "drugscatter"), {
            sel <- event_data("plotly_selected", source = "drugscatter")
            req(sel)
            selected_drugs(embed_df()$obs[sel$pointNumber + 1])
        })

        output$selected_count <- renderText(selection_count_text(length(selected_drugs()), "drug"))

        output$breadcrumb <- renderUI({
            req(length(selected_drugs()) > 0)
            breadcrumb_ui(ns)
        })
        observeEvent(input$back_to_landscape, selected_drugs(character(0)))


        # # ---- selection handling: click (1 drug) vs lasso (many) --------------
        # sd <- SharedData$new(scatter_data, key = ~drug, group = "drugpanel")
        # click_data <- reactive(event_data("plotly_click", source = "A"))
        # selected_keys <- reactive({
        #     sel <- sd$origData()[sd$selection() %||% rep(FALSE, nrow(scatter_data())), ]
        #     sel$drug
        # })
        # output$summary_table <- renderDT({
        #     datatable(sd, options = list(pageLength = 5), rownames = FALSE)
        # }, server = FALSE)

        focal_data <- reactive({
            req(length(selected_drugs()) > 0)
            fetch_drug_panel_expression(lab_source, study_id, drug_ids = selected_drugs())
        })

        output$focal_panel <- renderUI({
            n <- length(selected_drugs())
            if (n == 0) return(NULL)
            if (n == 1) tagList(plotlyOutput(ns("volcano")), plotlyOutput(ns("heatmap_single")))
            else tagList(plotlyOutput(ns("heatmap_multi")), plotlyOutput(ns("upset_plot")))
        })

        
        output$volcano <- renderPlotly({
            d <- focal_data()
            plot_ly(d, x = ~log2fc, y = ~-log10(padj), text = ~gene_symbol,
                    type = "scatter", mode = "markers")
        })
        output$heatmap_single <- output$heatmap_multi <- renderPlotly({
            d <- focal_data()
            plot_ly(d, x = ~drug_id, y = ~gene_symbol, z = ~log2fc, type = "heatmap")
        })
        # upset_plot: stub — ComplexUpset/upsetjs
        })
    }
)