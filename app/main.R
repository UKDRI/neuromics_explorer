# Main NEx_alpha module that ties together UI and server components of the app

# Run python startup (optional venv)
VENV_PATH <- here(".venv")
PYTHON_PATH <- NA_character_
if (dir.exists(VENV_PATH)) {
  candidates <- c(
    here(".venv", "bin", "python3"),
    here(".venv", "bin", "python3.13"),
    here(".venv", "bin", "python")
  )
  PYTHON_PATH <- candidates[file.exists(candidates)][1]
}
if (is.na(PYTHON_PATH) || !nzchar(PYTHON_PATH)) {
  PYTHON_PATH <- Sys.which("python3")
  message("Venv Python not found, falling back to: ", PYTHON_PATH)
} else {
  Sys.setenv(RETICULATE_PYTHON = PYTHON_PATH)
  reticulate::use_python(PYTHON_PATH, required = TRUE)
  # reticulate::source_python(here("app", "logic", "startup", "main_setup.py")) 
}

# Build paths from project root
STARTUP_SCRIPT <- normalizePath(here("app", "logic", "startup", "main_setup.py"))
DB_PATH        <- normalizePath(here("data", "neuromics_registry.duckdb"))
DB_DIAZ        <- normalizePath(here("data", "diaz_castro.duckdb"))
DB_HONG        <- normalizePath(here("data", "hong.duckdb"))
message("PYTHON:   ", PYTHON_PATH)
message("STARTUP:  ", STARTUP_SCRIPT)
message("DB:       ", DB_PATH)

# Run setup once when app starts
source(normalizePath(here("app", "logic", "startup", "run_startup.R")))
run_python_startup(
  db_path     = DB_PATH,
  script_path = STARTUP_SCRIPT,
  python      = PYTHON_PATH,
  wait        = TRUE   #Python runs to completion before pool::dbPool()
)
# run_python_startup()


# Load modules
box::use(
  shiny[...],  # ie shiny[bootstrapPage, div, moduleServer, NS, renderUI, tags, uiOutput, observeEvent],
  bslib[...],
  shinyjs[useShinyjs],
  app/view/pages/landing_page[homepage_ui, homepage_server],
  app/view/pages/explore_sidebar[sidebar_ui, sidebar_server],
  app/view/pages/data_explorer[explorer_ui, explorer_server],
  app/view/pages/data_submit[submit_ui, submit_server],
  app/logic/startup/run_startup[run_python_startup],
)

#' @export
ui <- page_navbar(
  title = div(
    style = "display: flex; align-items: center;",
    img(src = "static/images/logo.png", height = "30px", style = "margin-right: 10px;"),
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
    # value = "home",
    homepage_ui("home")
  ),
  
  nav_panel(
    title = "Explore Data",
    icon = icon("chart-line"),
    # value = "explore_data",
    explorer_ui("explore")
  ),
  
  nav_panel(
    title = "Submit Data",
    icon = icon("upload"),
    # value = "submit_data",
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
        p("Welcome to the NeurOmicsExplorer platform."),
        p("This guide will help you navigate and make the most out of the platform."),
        tags$ol(
          tags$li(tags$b("Select laboratory"), " → Choose your selected lab as data source"),
          tags$li(tags$b("Pick datasets"), "     → Select one or more studies"),
          tags$li(tags$b("Search genes"), "      → Search for your genes/ proteins of interest"),
          tags$li(tags$b("Explore results"), "   → Navigate to lab-specific, then omic-specific tab"),
          tags$li(tags$b("Visualise & export"), "→ View and manipulate plots and download relevant data")
        )
      )
    ),
    
    nav_panel(
      title = "Documentation",
      icon = icon("file-alt"),
      div(
        class = "container mt-4",
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
        tags$a(href="https://coreinformatics.uk/ukdri", "UK DRI Core Informatics Team website or documentation link.")
      )
    ),
    
    nav_panel(
      title = "Contact",
      icon = icon("envelope"),
      div(
        class = "container mt-4",
        h2("Contact Us"),
        p("Get in touch with our team...")
      )
    ),
    
    nav_panel(
      title = "FAQ",
      icon = icon("question-circle"),
      div(
        class = "container mt-4",
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
      h2("Send Feedback"),
      p("We'd love to hear from you...")
    )
  )
)



#' @export
server <- function(input, output, session) {
  
  # Create pool once per session (DBI::dbGetQuery)
  registry_pool <- pool::dbPool(
    drv    = duckdb::duckdb(),
    dbname = DB_PATH,
    minSize = 1  #???
  )
  # Raw connection for Arrow queries — pool cannot be used with duckdb_fetch_arrow
  registry_arrow_db <- DBI::dbConnect(duckdb::duckdb(), dbname = DB_PATH, read_only = TRUE)

  attach_db <- function(con, alias, path) {
    tryCatch(
      DBI::dbExecute(con, sprintf(
        "ATTACH '%s' AS %s (READ_ONLY)", path, alias
      )),
      error = function(e) {
        if (!grepl("already attach|already exist", e$message, ignore.case=TRUE))
          warning("ATTACH failed for ", alias, ": ", e$message)
      }
    )
  }
  attach_db(registry_pool, "src_diaz", DB_DIAZ)
  attach_db(registry_pool, "src_hong", DB_HONG)
  attach_db(registry_arrow_db, "src_diaz", DB_DIAZ)
  attach_db(registry_arrow_db, "src_hong", DB_HONG)
  
  # Wrap in reactive for modules to receive it as a reactive()
  
  registry_con       <- reactive(registry_pool)        # for DBI::dbGetQuery calls
  registry_arrow_con <- reactive(registry_arrow_db)    # for query_arrow() calls
  
  session$onSessionEnded(function() {
    pool::poolClose(registry_pool)
    DBI::dbDisconnect(registry_arrow_db, shutdown = TRUE)
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
  
  # ── Page servers ────────────────────────────────────────────────
  homepage_server("home")
  # sidebar_server("filters")
  explorer_server("explore", registry_arrow_con, registry_con)
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

shinyApp(ui, server)
