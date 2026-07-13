# generic UI + server shell

box::use(shiny[NS, moduleServer, tagList, uiOutput, renderUI])

signature_explorer_ui <- function(id, adapter) {
  ns <- NS(id)
  tagList(uiOutput(ns("panel")))
}

signature_explorer_server <- function(id, adapter, dataset, ...) {
  moduleServer(id, function(input, output, session) {
    output$panel <- renderUI(adapter$ui(session$ns("panel_body")))
    adapter$server("panel_body", dataset, ...)
  })
}
