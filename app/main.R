# Main module that ties together UI and server components of the app

# Load necessary modules
box::use(
  shiny,  # ie shiny[bootstrapPage, div, moduleServer, NS, renderUI, tags, uiOutput, observeEvent],
  app/view/landing_page[homepage_ui, homepage_server],
  app/view/explore_sidebar[sidebar_ui, sidebar_server]
)

#' @export
ui <- fluidPage(
  homepage_ui("home"), #landing_page$
  sidebar_ui("filters")
  
)

#' @export
server <- function(input, output, session) {
  homepage_server("home")
  sidebar_server("filters")
}

shinyApp(ui, server)