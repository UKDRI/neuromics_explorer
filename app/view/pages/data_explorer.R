# ─────────────────────────────────────────────────────────────────────────────
# This is for the "Explore Data" page for gene/dataset selection and visualisation
#  — ties gene_dataset_selector modal popup, sidebar, explore page, and all plot
# components together via shared `selected_dataset` reactiveVal.
#
# Layout:
#   Side panel:       explore_sidebar (thresholds, filters, modal popup button)
#   Left column (9):  Results /dataset table, plots section, dataset-level summaries
#   Right column (3): Project info section
#
# Data flow:
#   gene_dataset_selector → selected_dataset reactiveVal
#   selected_dataset + sidebar thresholds → de_data reactive
#   de_data → plot components
# ─────────────────────────────────────────────────────────────────────────────

box::use(
  shiny[...],
  shinydashboard[valueBoxOutput, renderValueBox, valueBox],
  # shinydashboardPlus[flipBox],
  shinyjs[runjs],
  bslib[...],
  plotly[plotlyOutput, renderPlotly, plot_ly, layout, add_trace],
  DT[DTOutput, renderDT, datatable],
  app/view/components/dataset_table[dataset_table_ui, dataset_table_server],
  app/view/components/expression_heatmap[heatmap_ui, heatmap_server],
  app/view/components/results_table[results_ui, results_server],
  app/view/components/violin_plot[violin_ui, violin_server],
  app/view/components/volcano_plot[volcano_ui, volcano_server],
  # app/view/components/umap_plot[umap_ui, umap_server],
  app/view/pages/gene_dataset_selector[gene_selector_ui, gene_selector_server],
  app/view/pages/explore_sidebar[sidebar_ui, sidebar_server],
  app/logic/query_data/query_builder[fetch_de_for_gene, fetch_dataset_stats],
)


#' @export
explorer_ui <- function(id) {
  ns <- NS(id)

  fluidRow(

    # ── LEFT collapsible sidebar (filters)   ───────────────────────────────
    page_sidebar(
      sidebar = sidebar(
        title = div(tags$h6("Data Filters and Threshold Options", style = "margin-top: 0; color: #667eea;"), tags$hr()),
        collapsible = TRUE,
        open = TRUE,
        fillable = TRUE, #FALSE,
        # width = 300,
        # bg = "#f8f9fa",
        # style = "background-color: #f8f9fa; padding: 15px; min-height: 100vh;",

        # Embed sidebar controls from explore_sidebar.R module
        sidebar_ui(ns("filters"))
      ),
      
      # Hero banner for modal popup
      div(
        class = "text-center",
        style = "background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
                   border-radius: 10px; padding: 24px; margin-bottom: 16px; color: white;",
        tags$h5("Find and explore all available datasets by clicking the button below to get started.",
                style = "color: white; margin-bottom: 8px;"),
        gene_selector_ui(ns("gene_selector"))
      ),
      # # Everything below is hidden until a dataset is selected
      # conditionalPanel(
      #   condition = paste0("output['", ns("has_selection"), "']"),
      

      # ── Main content column ─────────────────────────────────────────────
      layout_columns(
        col_widths = c(9, 3),
        gap = "1rem",
        
        # ── Gene search modal trigger ─────────────────────────────────────
        div(
        #   style = "margin-bottom: 10px;",
        #   # gene_selector_ui(ns("gene_selector")),  # render "Search" button

          # ──  Left column - top card: Datasets results table ─────────────
          class = "explore-content",
          card(
            full_screen = TRUE,
            card_header(
              div(tags$h4("Dataset Listings", class = "mb-0"))
            ),
            card_body(
              DTOutput(ns("dataset_listing"))  #, height = "580px"  ; icon = icon("table")
            )
          ),

          # ── Left column - middle card: Main visualisations ──────────────
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
                
                # TODO remove individual plot tabs and simply use dropdown list in sidebar to select
                nav_panel(
                  title = "Expression data",
                  icon = icon("chart-area"),
                  plotlyOutput(ns("results"), height = "600px")
                ),

                nav_panel(
                  title = "Heatmap",
                  icon = icon("th"),
                  plotlyOutput(ns("heatmap"), height = "600px")
                ),

                nav_panel(
                  title = "Volcano",
                  icon = icon("mountain"),
                  plotlyOutput(ns("volcano"), height = "600px")
                )
              )
            )
          ),

          # ── Left column - bottom row of cards: Quick Stats ─────────────
          tags$div(
            style = "margin-top: 20px;",
            # tags$h5("Quick Statistics", class = "mb-3"),
            layout_columns(
              fill = FALSE,
              # col_widths = c(2, 2, 2, 2, 2, 2),
              # full_screen = FALSE,
              style = "text-align:center; font-size:1.2rem;
        background-color: #f9f9f9; padding: 10px; border-radius: 8px;",
              card(valueBoxOutput(ns("box_datasets"))),
              card(valueBoxOutput(ns("box_genes"))),
              card(valueBoxOutput(ns("box_sig_genes"))),
              card(valueBoxOutput(ns("box_cells"))),
              card(valueBoxOutput(ns("box_cell_types"))),
              card(valueBoxOutput(ns("box_samples"))),
              card(valueBoxOutput(ns("box_conditions")))
              # card(valueBoxOutput("selected_datasets_box")),
              # card(valueBoxOutput("total_genes_box")),
              # card(valueBoxOutput("selected_genes_box")),
              # card(valueBoxOutput("total_cells_box")),
              # card(valueBoxOutput("cell_types_box")),
              # card(valueBoxOutput("total_samples_box")),
              # card(valueBoxOutput("qc_rate_box"))
            )
          )
        ),

        # ── Right column - Project info accordion ────────────────────────
        card(
          max_height = "90vh",
          style = "overflow-y: auto;", #"margin-top: 20px; position: sticky; top: 1rem;",
          card_header("Project Information"),
          card_body(
            style = "padding: 0;",
            accordion(
              id = ns("project_accordion"),
              multiple = TRUE,
              open = "Project Details",

              accordion_panel(
                title = "Project Details", icon = icon("info-circle"),
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
                title = "Metadata", icon = icon("table"),
                uiOutput(ns("metadata_table"))
              ),

              accordion_panel(
                title = "Data Quality", icon = icon("chart-bar"),
                uiOutput(ns("qc_metrics"))
              ),

              accordion_panel(
                title = "Quick Actions", icon = icon("bolt"),
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
explorer_server <- function(id, registry_con) {
  moduleServer(id, function(input, output, session) {
    
    # ── Datasets & sub-modules ────────────────────────────────────────────
    # selected_dataset holds: list(genes, lab_source, study_id, dataset_name,
    #                               omic_type, available_datasets, cell_types)
    selected_dataset <- reactiveVal(NULL)
    gene_selector_server("gene_selector", registry_con, selected_dataset)
    sidebar_vals <- sidebar_server("filters", registry_con, selected_dataset)
    
    # ── Plot subtitle ────────────────────────────────────────────────────
    output$plot_subtitle <- renderText({
      ds <- selected_dataset()
      if (is.null(ds)) "Select a gene to get started..."
      else paste0(ds$dataset_name, " · ", ds$omic_type, " · genes: ",
                  paste(ds$genes, collapse = ", "))
    })
    
    # ── DE data reactive ─────────────────────────────────────────────────
    de_data <- reactive({
      ds <- selected_dataset()
      req(ds, length(ds$genes) > 0)
      
      # Fetch for all selected genes × all selected datasets
      all_results <- lapply(ds$genes, function(gene) {
        fetch_de_for_gene(
          registry_con(),
          gene        = gene,
          lab_source  = ds$lab_source,
          study_id    = ds$study_id,
          padj_thresh = sidebar_vals$padj_thresh(),
          lfc_thresh  = sidebar_vals$lfc_thresh_min()
        )      # TODO add cell type to fetch_de_for_gene?
      })
      dplyr::bind_rows(all_results)
    })
    
    # ── Dataset listing section ───────────────────────────────────────────
    output$dataset_listing <- renderDT({
      req(registry_con())
      stats <- tryCatch(
        fetch_dataset_stats(registry_con()),
        error = function(e) {
          message("dataset_stats unavailable: ", e$message)
          data.frame(message = "Dataset stats not yet computed — run startup scripts first")
        }
      ) # stats <- fetch_dataset_stats(registry_con())
      datatable(stats, rownames = FALSE, options = list(pageLength = 15, scrollX = TRUE))
    }) # TODO check which dataset it fetches for, should be just a list of what is available after gene selection
    
    # ── Results table & plot servers ──────────────────────────────────────
    results_server("results",  de_data) # results table displaying expression data
    volcano_server("volcano",  de_data,
                   padj_thresh = sidebar_vals$padj_thresh,
                   lfc_thresh  = sidebar_vals$lfc_thresh_min,
                   gene        = reactive(selected_dataset()$genes[1]))
    heatmap_server("heatmap",  selected_dataset, registry_con,
                   padj_thresh = sidebar_vals$padj_thresh,
                   lfc_thresh  = sidebar_vals$lfc_thresh_min,
                   n_genes     = reactive(50L))
    violin_server("violin",    de_data)
    
    # ── Value boxes (tied to dataset_stats) ─────────────────────────────
    # stats_row <- reactive({
    #   ds <- selected_dataset()
    #   req(ds)
    #   fetch_dataset_stats(registry_con(), lab_source = ds$lab_source,
    #                       study_id = ds$study_id)
    # })
    
    # output$box_datasets   <- renderValueBox(valueBox(
    #   length(selected_dataset()$genes %||% 0), "Gene(s) selected",
    #   # icon = icon("dna"),
    #   color = "purple"))
    # 
    # output$box_genes      <- renderValueBox(valueBox(
    #   stats_row()$total_features %||% "—", "Total features",
    #   # icon = icon("list"),
    #   color = "blue"))
    # 
    # output$box_sig_genes  <- renderValueBox(valueBox(
    #   stats_row()$n_sig_features %||% "—", "Significant",
    #   # icon = icon("star"),
    #   color = "red"))
    # 
    # output$box_cells      <- renderValueBox(valueBox(
    #   stats_row()$total_cells %||% "—", "Total cells",
    #   icon = icon("circle-nodes"),
    #   color = "teal"))
    # 
    # output$box_cell_types <- renderValueBox(valueBox(
    #   stats_row()$n_cell_types %||% "—", "Cell types",
    #   icon = icon("tags"),
    #   color = "olive"))
    # 
    # output$box_samples    <- renderValueBox(valueBox(
    #   stats_row()$total_samples %||% "—", "Samples",
    #   icon = icon("vials"),
    #   color = "navy"))
    # 
    # output$box_conditions <- renderValueBox(valueBox(
    #   stats_row()$n_conditions %||% "—", "Conditions",
    #   # icon = icon("flask"),
    #   color = "maroon"))
    stats_row <- reactive({
      ds <- selected_dataset()
      if (is.null(ds)) return(data.frame())   # ← return empty, don't req() here
      tryCatch(
        fetch_dataset_stats(registry_con(), lab = ds$lab_source, study_id = ds$study_id),
        error = function(e) data.frame()
      )
    })
    
    output$box_datasets <- renderValueBox({
      ds    <- selected_dataset()
      count <- if (!is.null(ds)) length(ds$genes) else "—"
      valueBox(count, "Gene(s) selected", color = "purple")
    })
    
    output$box_genes <- renderValueBox({
      row <- stats_row()   # stats_row already has req(ds) inside it
      val <- if (nrow(row) > 0) row$total_features[1] %||% "—" else "—"
      valueBox(val, "Total features", color = "blue")
    })
    
    output$box_sig_genes <- renderValueBox({
      row <- stats_row()
      val <- if (nrow(row) > 0) row$n_sig_features[1] %||% "—" else "—"
      valueBox(val, "Significant (padj<0.05)", color = "red")
    })
    
    output$box_cells <- renderValueBox({
      row <- stats_row()
      val <- if (nrow(row) > 0) row$total_cells[1] %||% "—" else "—"
      valueBox(val, "Total cells", icon = icon("circle-nodes"), color = "teal")
    })
    
    output$box_cell_types <- renderValueBox({
      row <- stats_row()
      val <- if (nrow(row) > 0) row$n_cell_types[1] %||% "—" else "—"
      valueBox(val, "Cell types", icon = icon("tags"), color = "olive")
    })
    
    output$box_samples <- renderValueBox({
      row <- stats_row()
      val <- if (nrow(row) > 0) row$total_samples[1] %||% "—" else "—"
      valueBox(val, "Samples", icon = icon("vials"), color = "navy")
    })
    
    output$box_conditions <- renderValueBox({
      row <- stats_row()
      val <- if (nrow(row) > 0) row$n_conditions[1] %||% "—" else "—"
      valueBox(val, "Conditions", color = "maroon")
    })
    
    # observeEvent(input$go_explore, {
    #   # example navigation
    #   # runjs("window.location.hash = '#Explore Data'")
    # })

  })
}
# null helper
`%||%` <- function(a, b) if (!is.null(a) && !is.na(a)) a else b
