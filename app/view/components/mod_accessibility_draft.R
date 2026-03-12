# R/mod_accessibility.R

accessibility_ui <- function(id) {
  ns <- NS(id)
  
  card(
    card_header("Accessibility"),
    sliderInput(ns("font_size"), "Font size", 90, 130, 100, post = "%"),
    checkboxInput(ns("high_contrast"), "High contrast mode"),
    checkboxInput(ns("reduce_motion"), "Reduce animations")
  )
}

accessibility_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    observe({
      session$setCurrentTheme(
        bs_theme(
          version = 5,
          base_font = font_google("Inter"),
          font_scale = input$font_size / 100,
          bootswatch = if (input$high_contrast) "darkly" else NULL
        )
      )
    })
    
    observe({
      if (input$reduce_motion) {
        shinyjs::runjs(
          "$('*').css('transition','none')"
        )
      }
    })
  })
}


# add to sidebar:
sidebar = sidebar(
  title = "Data Options",
  collapsible = TRUE,
  
  sidebar_ui("filters"),
  tags$hr(),
  accessibility_ui("a11y")
)

# add to server
accessibility_server("a11y")


# for Motion-safe CSS (respects reduced motion) for animated profile cards WCAG-compliancy
#css
@media (prefers-reduced-motion: reduce) {
  * {
    transition: none !important;
    animation: none !important;
  }
}

# full example
page_sidebar(
  fillable = FALSE,   # critical UX fix
  
  sidebar = sidebar(
    title = "Data Options",
    collapsible = TRUE,
    sidebar_ui("filters"),
    tags$hr(),
    accessibility_ui("a11y")
  ),
  
  main_content_ui()
)


