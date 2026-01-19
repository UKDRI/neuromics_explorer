## Module for submitting data

box::use(
  shiny[moduleServer, NS, h2, p]
)

submit_ui <- function(id) {
  ns <- NS(id)
  tagList(
    h2("Submit Data"),
    p("Instructions for submitting data.")
  )
}

submit_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    
  })
}
