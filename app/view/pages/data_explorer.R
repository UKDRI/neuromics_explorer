## This is the UI section for the "Explore Data" page for gene/dataset selection and plot visualisation

box::use(
  shiny[...],
  shinydashboard[valueBoxOutput, valueBox],
  shinydashboardPlus[flipBox],
  shinyjs[runjs],
  bslib[...],
  plotly[plotlyOutput, renderPlotly, plot_ly, layout, add_trace],
  DT[DTOutput, renderDT, datatable],
  app/view/pages/explore_sidebar[sidebar_ui, sidebar_server],
  # app/logic/data_processing[load_datasets, filter_data, get_metadata],
  # app/logic/plotting[create_expression_plot, create_volcano_plot, create_heatmap]
)


#' @export
explorer_ui <- function(id) {
  ns <- NS(id)

  fluidRow(

    # LEFT collapsible sidebar (filters)
    page_sidebar(
      sidebar = sidebar(
        title = div(tags$h5("Data Options", style = "margin-top: 0; color: #667eea;"), tags$hr()),
        collapsible = TRUE,
        open = TRUE,
        fillable = TRUE, #FALSE,
        # width = 300,
        # bg = "#f8f9fa",
        # style = "background-color: #f8f9fa; padding: 15px; min-height: 100vh;",

        # Embed sidebar controls from explore_sidebar.R module
        sidebar_ui(ns("filters"))
      ),

      # Main content column
      layout_columns(
        col_widths = c(9, 3),
        gap = "1rem",

        # Placeholder for main content
        div(
          class = "explore-content",

          # Top card: Datasets results table
          card(
            full_screen = TRUE,
            card_header(
              div(tags$h4("Dataset Listings", class = "mb-0"))
            ),
            card_body(
              DTOutput(ns("dataset_listing"))
            )
          ),

          # Middle card: Main visualisations
          card(
            full_screen = TRUE,
            card_header(
              class = "d-flex justify-content-between align-items-center",
              div(
                tags$h4("Data Visualisation", class = "mb-0"),
                tags$small(class = "text-muted", textOutput(ns("plot_subtitle")))
              ),
              div(
                tags$button(
                  class = "btn btn-sm btn-outline-primary",
                  icon("download"),
                  "Export Plot",
                  onclick = sprintf("Shiny.setInputValue('%s', Math.random())", ns("export_plot"))
                )
              )
            ),
            card_body(
              min_height = "500px",

              # Tab navigation for expression and various other plots
              navset_card_tab(
                id = ns("exploration_tabs"),

                nav_panel(
                  title = "Expression",
                  icon = icon("chart-area"),
                  plotlyOutput(ns("expression_plot"), height = "600px")
                ),

                nav_panel(
                  title = "Heatmap",
                  icon = icon("th"),
                  plotlyOutput(ns("heatmap_plot"), height = "600px")
                ),

                nav_panel(
                  title = "Volcano",
                  icon = icon("mountain"),
                  plotlyOutput(ns("volcano_plot"), height = "600px")
                )
              )
            )
          ),

          # Bottom row of cards: Quick Stats
          tags$div(
            style = "margin-top: 20px;",
            # tags$h5("Quick Statistics", class = "mb-3"),
            layout_columns(
              fill = FALSE,
              # col_widths = c(2, 2, 2, 2, 2, 2),
              # full_screen = FALSE,
              style = "text-align:center; font-size:1.2rem;
        background-color: #f9f9f9; padding: 10px; border-radius: 8px;",
              card(valueBoxOutput("selected_datasets_box")),
              card(valueBoxOutput("total_genes_box")),
              card(valueBoxOutput("selected_genes_box")),
              card(valueBoxOutput("total_cells_box")),
              card(valueBoxOutput("cell_types_box")),
              card(valueBoxOutput("total_samples_box")),
              card(valueBoxOutput("qc_rate_box"))
            )
          )
        ),

        # Right card: Project info dropdowns
        card(
          max_height = "90vh",
          style = "overflow-y: auto;", #"margin-top: 20px; position: sticky; top: 1rem;",
          card_header("Project Information"),
          card_body(
            style = "padding: 0;",
            accordion(
              id = ns("project_accordion"),
              multiple = TRUE,
              open = "Project",

              accordion_panel(
                title = "Project Details",
                icon = icon("info-circle"),
                div(
                  tags$h6("Description", class = "fw-bold mt-2"),
                  uiOutput(ns("project_desc")),

                  tags$h6("Authorship", class = "fw-bold mt-3"),
                  uiOutput(ns("project_citation")),

                  tags$h6("Contact", class = "fw-bold mt-3"),
                  uiOutput(ns("project_contact"))
                )
              ),

              accordion_panel(
                title = "Metadata",
                icon = icon("table"),
                uiOutput(ns("metadata_table"))
              ),

              accordion_panel(
                title = "Data Quality",
                icon = icon("chart-bar"),
                uiOutput(ns("qc_metrics"))
              ),

              accordion_panel(
                title = "Quick Actions",
                icon = icon("bolt"),
                div(
                  class = "d-grid gap-2",
                  tags$button(
                    class = "btn btn-outline-primary",
                    icon("external-link-alt"),
                    "View in Data Nexus",
                    onclick = sprintf("Shiny.setInputValue('%s', Math.random())", ns("datanexus_link"))
                  ),
                  tags$button(
                    class = "btn btn-outline-success mt-2",
                    icon("download"),
                    "Export Data",
                    onclick = sprintf("Shiny.setInputValue('%s', Math.random())", ns("export_data"))
                  )
                )
              )
            )
          )
        )
      )
    ),

    # Spacer at the bottom
    tags$div(style = "height: 30px;")

  )
}


#' @export
explorer_server <- function(id) {
  moduleServer(id, function(input, output, session) {

    observeEvent(input$go_explore, {
      # example navigation
      # runjs("window.location.hash = '#Explore Data'")
    })

  })
}
