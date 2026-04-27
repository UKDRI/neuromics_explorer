# Main NEx_alpha module that ties together the Shiny UI and the Python API.
#
# Production model:
#   - FastAPI owns DuckDB startup, SQL execution, pooling, and Arrow IPC
#   - Shiny stays a thin HTTP client that renders returned data
API_BASE_URL <- Sys.getenv("NEX_API_BASE_URL", "http://127.0.0.1:7000/api")
message("API: ", API_BASE_URL)


# ── Load modules, define ui and server ─────────────────────────────────────
box::use(
  shiny[addResourcePath, ...],  
  # shiny[bootstrapPage, div, moduleServer, NS, renderUI, tags, uiOutput, observeEvent],
  bslib[...],
  shinyjs[useShinyjs],
  app/logic/api/api_client[set_api_base_url],
  app/view/pages/landing_page[homepage_ui, homepage_server],
  app/view/pages/explore_sidebar[sidebar_ui, sidebar_server],
  app/view/pages/data_explorer[explorer_ui, explorer_server],
  app/view/pages/data_submit[submit_ui, submit_server],
)

# Register app/static as a Shiny resource path so the logo and custom stylesheet are served correctly.
static_path <- "app/static"
if (!dir.exists(static_path)) {
  static_path <- "static"
}
if (!dir.exists(static_path)) {
  stop("Could not locate the app static asset folder. Expected app/static or static.")
}
addResourcePath("static", normalizePath(static_path))

#' @export
ui <- page_navbar(
  title = div(
    style = "display: flex; align-items: center;",
    img(src = "static/images/ukdri_logo.png", height = "30px", style = "margin-right: 10px;"),
    "NeurOmicsExplorer"
  ),
  id = "main_nav",
  theme = bslib::bs_theme(
    bootswatch = "cosmo", # "flatly"
    version = 5,
    primary = "#667eea",
    bg = "#ffffff",
    fg = "#333333"
  ),
  
  # Custom CSS and styles
  header = tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "static/custom.css"),
    tags$style(HTML("
      /* Hide navbar items on homepage */
      .hide-nav .navbar-nav { display: none !important; }
      .hide-nav #navbar-brand { display: flex !important; }
    ")),
    # bs_theme_toggle(),
    # CSS toggleTheme():
    # <button class="theme-toggle" type="button" onclick="toggleTheme()" aria-label="Switch to dark mode">
    #   <span class="theme-toggle-icon">🌙</span>
    #   <span class="theme-toggle-text">Dark</span>
    #   </button>
    # <button class="theme-toggle" type="button" onclick="toggleTheme()" aria-label="Switch to light mode">
    #   <span class="theme-toggle-icon">☀️</span>
    #   <span class="theme-toggle-text">Light</span>
    #   </button>
  ),
  
  # Add shinyjs
  useShinyjs(),
  
  # Nav panels
  nav_panel(
    title = "Homepage",
    icon = icon("home"),
    value = "home",
    homepage_ui("home")
  ),
  
  nav_panel(
    title = "Explore Data",
    icon = icon("chart-line"),
    value = "explore_data",
    explorer_ui("explore")
  ),
  
  nav_panel(
    title = "Submit Data",
    icon = icon("upload"),
    value = "submit_data",
    submit_ui("submit")
  ),
  
  # Dropdown menu with additional pages
  nav_menu(
    title = "Resources",
    icon = icon("book"),
    align = "left",
    
    nav_panel(
      title = "Getting Started",
      icon = icon("play-circle"),
      div(
        class = "container mt-4",
        style = "border: 1px solid #ddd; padding: 15px; border-radius: 8px; background: white; box-shadow: 0 2px 4px rgba(0,0,0,0.1); margin: 10px 0;",
        h2("Getting Started"),
        p("Welcome to the NeurOmics Explorer platform."),
        p("This guide will help you navigate and make the most out of the platform."),
        tags$ol(
          tags$li(tags$b("Search genes or proteins"), " → Search for your genes or proteins of interest across all available datasets"),
          tags$li(tags$b("Pick datasets"), "     → Select one or more datasets to explore - note only 1 sc/snRNAseq dataset can be viewed at a time"),
          tags$li(tags$b("Explore results"), "   → Visualise and interact with the datasets through visualisation plots, expression tables, and filtering controls"),
          tags$li(tags$b("Export"), "        → View and manipulate plots and download relevant data")
        )
      )
    ),
    
    nav_panel(
      title = "Documentation",
      icon = icon("file-alt"),
      div(
        class = "container mt-4",
        h2("COMING SOON"),
        tags$h2("Documentation"),
        tags$p("Comprehensive documentation for the platform...")
      )
    ),
    
    "----",  # Divider
    
    nav_panel(
      title = "Tutorials",
      icon = icon("graduation-cap"),
      div(
        class = "container mt-4",
        h2("COMING SOON"),
        h2("Tutorials"),
        p("Step-by-step guides on what is good data, and what is bad...")
      )
    )
  ),
  
  # Another dropdown for Help/About
  nav_menu(
    title = "About",
    icon = icon("info-circle"),
    
    nav_panel(
      title = "About Us",
      # shiny::uiOutput("about_page") # TODO add for AboutUs.rmd doc - remove below
      icon = icon("users"),
      div(
        class = "container mt-4",
        h2("About the Core Informatics Team @ UK DRI"),
        style = "border: 1px solid #ddd; padding: 15px; border-radius: 8px; background: white; box-shadow: 0 2px 4px rgba(0,0,0,0.1); margin: 10px 0;",
        p(style = "margin-bottom: 0;",
               "This platform can also help teams with:"),
        tags$ul(
          tags$li("Data analytics and collaborative interpretations"),
          tags$li("Conceptualisation through data mining"),
          tags$li("Training sessions")
        ),
        tags$p("For more information, please visit the"), 
        tags$a(href="https://coreinformatics.uk/ukdri", "LINK COMING SOON! - UK DRI Core Informatics")
      )
    ),
    
    nav_panel(
      title = "Contact",
      icon = icon("envelope"),
      div(
        class = "container mt-4",
        h2("COMING SOON"),
        h2("Contact Us"),
        p("Get in touch with our team...")
      )
    ),
    
    nav_panel(
      title = "FAQ",
      icon = icon("question-circle"),
      div(
        class = "container mt-4",
        h2("COMING SOON"),
        h2("Frequently Asked Questions"),
        p("Common questions and answers...")
      )
    )
  ),
  
  # Right-aligned items
  nav_spacer(),
  
  nav_panel(
    title = "Feedback",
    icon = icon("comment-dots"),
    # value = "feedback",
    div(
      class = "container mt-4",
      h2("COMING SOON"),
      h2("Send Feedback"),
      p("We'd love to hear from you...")
    )
  )
)



#' @export
server <- function(input, output, session) {
  # Production mode: Shiny is the HTTP client, Python owns DuckDB and Arrow IPC.
  set_api_base_url(API_BASE_URL)
  shared_link <- reactiveVal(NULL)
  deep_link_applied <- reactiveVal(FALSE)

  # Store in a convenience variable
  cdata <- session$clientData

  # Values from cdata returned as text
  output$clientdataText <- renderText({
    cnames <- names(cdata)

    allvalues <- lapply(cnames, function(name) {
      paste(name, cdata[[name]], sep = " = ")
    })
    paste(allvalues, collapse = "\n")
  })
  
  # Hide navbar items when on homepage
  observe({
    req(input$main_nav)
    
    # if (input$main_nav == "home") {
    #   runjs("$('nav.navbar').addClass('hide-nav');")
    # } else {
    #   runjs("$('nav.navbar').removeClass('hide-nav');")
    # }
  })

  observe({
    if (isTRUE(deep_link_applied())) return()

    search <- cdata$url_search
    if (is.null(search) || !nzchar(search)) return()

    query <- shiny::parseQueryString(search)
    genes <- unname(query[grepl("^gene($|\\.)", names(query))])
    proteins <- unname(query[grepl("^protein($|\\.)", names(query))])
    target <- query[["page"]] %||% query[["target"]] %||% ""

    has_dataset_ref <- nzchar(query[["lab"]] %||% "") ||
      nzchar(query[["lab_source"]] %||% "") ||
      nzchar(query[["study"]] %||% "") ||
      nzchar(query[["study_id"]] %||% "")

    if (!(tolower(target) %in% c("explore", "explore_data")) && !has_dataset_ref) {
      return()
    }

    shared_link(list(
      lab_source = query[["lab_source"]] %||% query[["lab"]] %||% NULL,
      study_id = suppressWarnings(as.integer(query[["study_id"]] %||% query[["study"]] %||% NA)),
      genes = genes,
      proteins = proteins
    ))

    deep_link_applied(TRUE)
    shiny::updateNavbarPage(session, "main_nav", selected = "explore_data")
  })
  
  # ── Page servers ────────────────────────────────────────────────
  homepage_server("home")
  # sidebar_server("filters")
  explorer_server("explore", initial_link = reactive(shared_link()))
  submit_server("submit")
  
  # ── About page — rendered from Rmd ───────────────────────────────
  output$about_page <- renderUI({
    # Iframe full shiny integration
    output$about_page <- renderUI({
      shiny::tags$iframe(
        src    = "AboutUs.Rmd",   # served by shiny::addResourcePath
        width  = "100%",
        height = "800px",
        frameborder = 0
      )
      # OR render html
      # includeHTML(
      #   rmarkdown::render(
      #     "app/view/pages/AboutUs.Rmd",
      #     output_format = rmarkdown::html_fragment(),
      #     quiet         = TRUE
      #   )
      # )
    })
  })
}

`%||%` <- function(a, b) {
  if (is.null(a) || length(a) == 0) return(b)
  a
}

shinyApp(ui, server)
