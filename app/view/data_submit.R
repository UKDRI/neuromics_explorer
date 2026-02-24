## Module for submitting data

box::use(
  shiny[moduleServer, NS, h2, p, fileInput, textInput, actionButton],
  htmltools[tagList]
)

#' @export
submit_ui <- function(id) {
  ns <- NS(id)
  tagList(
    h2("Submit Data"),
    p("Instructions for submitting data."),

    fileInput(ns("upload_data"), "Upload dataset (shinycell2, .rds, .h5ad)"),
    textInput(ns("upload_description"), "Dataset description"),
    fileInput(ns("upload_geneset"), "Pre-load gene sets"),
    # cellxgene Requirements: A gene set must have a unique name. A given gene may be included in multiple gene sets. A gene set may optionally include a description for the entire gene set, and descriptions for each gene included the gene set.
    actionButton(ns("submit_data"), "Submit")
  )
}

#' @export
submit_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    # observeEvent(input$submit_data, {
    #   # Handle data submission
    #   showModal(modalDialog(
    #     title = "Submission Received",
    #     "Thank you for submitting your dataset!",
    #     easyClose = TRUE,
    #     footer = NULL
    #   ))
    # })
  })
}
