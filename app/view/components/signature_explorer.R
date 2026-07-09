# generic UI + server shell

box::use(shiny[NS, moduleServer, tagList, uiOutput, renderUI])

signatureExplorer_ui <- function(id, adapter) {
  ns <- NS(id)
  tagList(uiOutput(ns("focal_panel")))
}

signatureExplorer_server <- function(id, adapter) {
  moduleServer(id, function(input, output, session) {
    output$panel <- renderUI(adapter$ui(session$ns("panel_body")))
    adapter$server("panel_body")
  })
}
