box::use(
  shiny[moduleServer, NS, div, h1, h3, p, tagList, tags, icon, actionButton, observeEvent],
  bslib[layout_columns, card, card_header, card_body],
  shinyjs[runjs, useShinyjs],
  htmltools[HTML]
)

#' @export
homepage_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    useShinyjs(),
    tags$head(
      tags$style(HTML("
      
        /* Header/ Hero section */
        .hero-section {
          text-align: center;
          padding: 60px 20px;
          background: linear-gradient(135deg, #667eea 80%, #764ba2 0%);
          color: white;
          margin: -20px -20px 40px -20px;
        }
        .hero-section h1 { font-size: 3rem; margin-bottom: 20px; }
        .hero-section p { font-size: 1.3rem; opacity: 0.95; }
        
        /* Flip card container */
        .flip-card {
          perspective: 1000px;
          width: 100%;
          max-width: 320px;
          height: 100%;
          aspect-ratio: 3 / 4;
          margin: auto;
          cursor: pointer;
          transition: transform 0.4s;
        }
        .flip-card:hover { transform: translateY(-5px); }
        
        /* Inner container for flip animation */
        .flip-card-inner {
          position: relative;
          width: 100%;
          height: 100%;
          transition: transform 0.6s;
          transform-style: preserve-3d;
        }
        .flip-card:hover .flip-card-inner { transform: rotateY(180deg); }
        
        /* Front and back faces */
        .flip-card-front, .flip-card-back {
          position: absolute;
          width: 100%;
          height: 100%;
          backface-visibility: hidden;
          border-radius: 12px;
          display: flex;
          flex-direction: column;
          align-items: center;
          justify-content: center;
          padding: 30px;
          box-shadow: 0 4px 15px rgba(0,0,0,0.1);
        }
        
        /* Front face - solid colors */
        .flip-card-front {
          background: white;
          border: 2px solid #e0e0e0;
        }
        .flip-card-front i { font-size: 3rem; margin-bottom: 20px; }
        .flip-card-front h3 { margin: 0; font-size: 1.3rem; }
        
        /* Back face - solid colors */
        .flip-card-back {
          transform: rotateY(180deg);
          padding: 20px;
        }
        .flip-card-back p { font-size: 0.9rem; margin-bottom: 20px; }
        .flip-card-back button {
          background: white;
          color: #667eea;
          border: none;
          padding: 12px 30px;
          border-radius: 25px;
          font-weight: 700;
          cursor: pointer;
          transition: all 0.3s;
        }
        .flip-card-back button:hover {
          transform: scale(1.05);
          box-shadow: 0 4px 15px rgba(255,255,255,0.3);
        }
        
        /* Individual bg colors - COMMENTED OUT
        .card-explore { background: linear-gradient(135deg, #667eea 0%, #764ba2 100%); }
        .card-about { background: linear-gradient(135deg, #f093fb 0%, #f5576c 100%); }
        .card-guide { background: linear-gradient(135deg, #4facfe 0%, #00f2fe 100%); }
        .card-submit { background: linear-gradient(135deg, #fa709a 0%, #fee140 100%); }
        .card-feedback { background: linear-gradient(135deg, #30cfd0 0%, #330867 100%); }
        */
      "))
    ),
    
    # Hero section
    div(class = "hero-section",
        h1("Welcome to the NeurOmics Explorer"),
        p("Your comprehensive platform for exploring, analysing, and visualising multi-omic UK DRI datasets")
    ),
    
    # fluidPage(fluidRow(
    
    layout_columns(
      col_widths = c(3, 3, 3, 3, 3),
      
      # Card 1: Explore Datasets
      div(class = "flip-card card-explore",
          div(class = "flip-card-inner",
              div(class = "flip-card-front",
                  icon("database", style = "color: #667eea;"),
                  h3("Explore Datasets", style = "text-align:center;")
              ),
              div(class = "flip-card-back",
                  p("Browse and analyse multi-omic datasets submitted by UK DRI researchers"),
                  actionButton(
                    ns("go_explore"),
                    "Get Started",
                    onclick = "document.getElementById('nav-Explore Data').click();"
                  )
              )
          )
      ),
      
      # Card 2: About Us
      div(class = "flip-card card-about",
          div(class = "flip-card-inner",
              div(class = "flip-card-front",
                  icon("info-circle", style = "color: #f5576c;"),
                  h3("About Us", style = "text-align:center;")
              ),
              div(class = "flip-card-back",
                  p("Learn about our mission, team, and the technology behind the platform"),
                  actionButton(
                    ns("go_about"),
                    "Learn More",
                    onclick = "document.getElementById('nav-About Us').click();"
                  )
              )
          )
      ),
      
      # Card 3: Getting Started
      div(class = "flip-card card-guide",
          div(class = "flip-card-inner",
              div(class = "flip-card-front",
                  icon("book-open", style = "color: #00f2fe;"),
                  h3("Getting Started", style = "text-align:center;")
              ),
              div(class = "flip-card-back",
                  p("Comprehensive guides and tutorials to help you get the most out of NeurOmics Explorer (NEx)"),
                  actionButton(
                    ns("go_guide"),
                    "View Guides",
                    onclick = "document.getElementById('nav-Getting Started').click();"
                  )
              )
          )
      ),
      
      # Card 4: Submit Data
      div(class = "flip-card card-submit",
          div(class = "flip-card-inner",
              div(class = "flip-card-front",
                  icon("upload", style = "color: #fa709a;"),
                  h3("Submit Data", style = "text-align:center;")
              ),
              div(class = "flip-card-back",
                  p("Share your research with the community and contribute to the platform"),
                  actionButton(
                    ns("go_submit"),
                    "Submit Now",
                    onclick = "document.getElementById('nav-Submit Data').click();"
                  )
              )
          )
      ),
      
      # Card 5: Feedback
      div(class = "flip-card card-feedback",
          div(class = "flip-card-inner",
              div(class = "flip-card-front",
                  icon("comments", style = "color: #30cfd0;"),
                  h3("Feedback", style = "text-align:center;")
              ),
              div(class = "flip-card-back",
                  p("Help us improve the platform by sharing your thoughts and suggestions"),
                  actionButton(
                    ns("go_feedback"),
                    "Send Feedback",
                    onclick = "document.getElementById('nav-Feedback').click();"
                  )
              )
          )
      )
    )
  )
}

#' @export
homepage_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    observeEvent(input$go_explore, {
      # Navigate to explore tab
      # runjs('document.querySelector(\'a[data-value="Explore Data"]\').click();')
      # runjs('$("#main_tabs a[data-value=\'explore_data\']").tab("show")')
      runjs('$("a[data-value=\'Explore Data\']").tab("show");')
    })
    
    observeEvent(input$go_submit, {
      runjs('$("a[data-value=\'Submit Data\']").tab("show");')
    })
    
    observeEvent(input$go_about, {
      runjs('$("a[data-value=\'About Us\']").tab("show");')
    })
    
    observeEvent(input$go_guide, {
      runjs('$("a[data-value=\'Getting Started\']").tab("show");')
    })
    
    observeEvent(input$go_feedback, {
      runjs('$("a[data-value=\'Feedback\']").tab("show");')
    })
    
  })
}