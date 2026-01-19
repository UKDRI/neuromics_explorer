## This is the UI section for the app's homepage

box::use(
  shiny[moduleServer, NS, div, p, actionButton, observeEvent],
  shinydashboardPlus[flipBox],
  shinyjs[runjs]
)


#' @export
homepage_ui <- function(id) {
  ns <- NS(id)
  
  flipBox(id = ns("flip_home_1"),
          front = "Explore datasets",
          trigger = "hover",
          back = div(
            class = "text-center",
            p("Click here to begin exploring datasets submitted by UK DRI researchers."),
            actionButton(
              ns("go_explore"),
              "Explore datasets",
              class = "btn-primary btn-lg"
            ))
        #back = "Click here to begin exploring datasets submitted by UK DRI researchers."
  )
}


#' @export
homepage_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    observeEvent(input$go_explore, {
      # example navigation
      runjs("window.location.hash = '#Explore Data'")
    })
    
  })
}

# box::export(homepage_ui, homepage_server)


# flipBox(id = "flip_home_1", front = "Explore dasatets", #back = "Click here to begin exploring datasets submitted by UK DRI researchers.", 
#         trigger = "hover",
#         back = div(
#           class = "text-center",
#           p("Click here to beging exploring datasets submitted by UK DRI researchers."), 
#           actionButton("go_explore", "Explore datasets", class = "btn-primary btn-lg")
#           # , a(href="")
#           # , link = ""
#         )
#         # , width = 6
# )

# flipBox() <- function(front, back) {
#   div(class = "flip-card",
#       div(class="flip-card-inner",
#           div(class="flip-card-front", front),
#           div(class="flip-card-back", back)
#       )
#   )
# }
# 
# 
# div(
#   class = "home-container",
#   h1("Omics Explorer"),
#   div(class = "card-grid",
#       lapply(c("Explore datasets","Getting started","Submit data","About us","Feedback"),
#              function(title) {
#                div(class = "flip-card",
#                    div(class="flip-card-inner",
#                        div(class="flip-card-front", title),
#                        div(class="flip-card-back", paste("Go to", title))
#                    )
#                )
#              }
#       )
#   )
# )