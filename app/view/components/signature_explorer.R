# generic UI + server shell (scatter, selection, layout switch)

signatureExplorerUI <- function(id, adapter) {
  ns <- NS(id)
  tagList(
    h4(paste("Global", adapter$entity_name, "Landscape")),
    plotlyOutput(ns("scatter"), height = "480px"),
    textOutput(ns("selected_count")),
    uiOutput(ns("focal_panel"))
  )
}

signatureExplorerServer <- function(id, adapter) {
  moduleServer(id, function(input, output, session) {
    embed_df <- adapter$embed_data()
    de_long  <- adapter$long_data()

    # ... same scatter/selection logic as before, entity-agnostic ...
    sd <- SharedData$new(scatter_data, key = ~drug, group = "drugpanel")

    output$scatter <- renderPlotly({
      colour_var <- if (input$colour_by == "Cluster") ~cluster else ~n_sig_degs
      plot_ly(sd, x = ~PC1, y = ~PC2, size = ~size_val, color = colour_var,
              type = "scatter", mode = "markers",
              marker = list(sizemode = "area", sizeref = 0.5, opacity = 0.8),
              text = ~drug, hoverinfo = "text") %>%
        layout(dragmode = "lasso",
               xaxis = list(title = "PC1"), yaxis = list(title = "PC2")) %>%
        highlight(on = "plotly_selected", off = "plotly_deselect") %>%
        event_register("plotly_click")
    })

    output$summary_table <- renderDT({
      datatable(sd, options = list(pageLength = 5), rownames = FALSE)
    }, server = FALSE)

    # ---- selection handling: click (1 drug) vs lasso (many) --------------
    click_data <- reactive(event_data("plotly_click", source = "A"))
    selected_keys <- reactive({
      sel <- sd$origData()[sd$selection() %||% rep(FALSE, nrow(scatter_data())), ]
      sel$drug
    })

    output$selected_count <- renderText({
      n <- length(selected_keys())
      paste0("Selected: ", n, " ", adapter$entity_name, if (n != 1) "s" else "")
    })

    output$focal_panel <- renderUI({
      n <- length(selected_keys())
      if (n == 0) return(NULL)
      if (n == 1) adapter$focal_single(id, selected_keys(), de_long)
      else adapter$focal_multi(id, selected_keys(), de_long)
    })
  })
}

