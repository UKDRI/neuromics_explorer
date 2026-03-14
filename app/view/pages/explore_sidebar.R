# ────────────────────────────────────────────────────────────────────────────────────────
# Module contains sidebar menu for the Explorer page
# Users can select genes, re-open gene selector popup modal, apply filters to plots, etc.
# ────────────────────────────────────────────────────────────────────────────────────────

box::use(
  # shiny[...],
  shiny[moduleServer, NS, tagList, selectInput, selectizeInput, updateSelectInput,
        sliderInput, updateSliderInput, checkboxGroupInput, actionButton,
        observeEvent, uiOutput, renderUI, tags, reactive],
  shinyWidgets[pickerInput, sliderTextInput],
  bslib[accordion, accordion_panel],
  app/view/pages/gene_dataset_selector[gene_selector_ui, gene_selector_server],
)

#' @export
sidebar_ui <- function(id) {
  ns <- NS(id)
  tagList(
    
    # ── Gene / protein search (tied to modal popup) ─────────────────────────────
    tags$label("Genes", style = "font-weight: 600; color: #333;"),
    # Read-only display of genes selected via modal — button re-opens modal
    uiOutput(ns("selected_genes_display")),
    actionButton(
      ns("open_gene_modal"), "Search Gene or Protein",
      class = "btn btn-primary btn-block",
      style = "background-color: #667eea; border-color: #667eea; width: 100%; margin-bottom: 10px;"
    ),
    
    tags$hr(),
    
    # ── Dataset selector - populated reactively after gene search (only datasets containing selected genes)
    tags$label("Select Dataset(s)", style = "font-weight: 600; color: #333;"),
    uiOutput(ns("dataset_picker_ui")),
    
    tags$hr(),
    
    # ── Plot type and user-inputs ─────────────────────────────────────────────────────────
    tags$label("Plot type", style = "font-weight: 600; color: #333;"),
    selectInput(ns("plot_type"), NULL,
                choices  = c("", "Dot", "Histogram", "Volcano", "Heatmap", "Violin", 
                             "Bar", "PCA", "UMAP", "Scater", "Feature Scatter", "HighestExpr"),
                selected = ""
    ),
    
    tags$label("Visualisation", style = "font-weight: 600; color: #333;"),
    selectInput(ns("vis_type"), NULL,
                choices  = c("", "By cell type", "By condition", "By sample"),
                selected = ""
    ),
    
    tags$hr(),
    
    # ── Significance threshold filters (collapsible) ─────────────────────────────
    accordion(
      id = ns("filter_accordion"),
      multiple = TRUE,
      open = FALSE,
      
      accordion_panel(
        title = "Significance Filters",
        icon  = shiny::icon("filter"),  # TODO select different icon
        
        tags$label("LogFC", style = "font-weight: 600; color: #333; font-size: 13px;"),
        sliderInput(ns("lfc_thresh"),
                    label = NULL, min = -8, max = 8,
                    value = c(0, 1),
                    step  = 0.1, ticks = FALSE
        ),
        
        tags$label("padj threshold", style = "font-weight: 600; color: #333; font-size: 13px;"),
        sliderInput(ns("padj_thresh"),
                    label = NULL, min = 0.001, max = 0.2,
                    value = 0.05, step = 0.001, ticks = FALSE
        )
      ),
      
      # ── Additional filters (collapsible) - populated reactively from selected dataset's metadata
      accordion_panel(
        title = "Cell Type Filter",
        icon  = shiny::icon("dna"),   # TODO select different icon
        uiOutput(ns("cell_type_ui"))
      ),
      
      accordion_panel(
        title = "Additional Filters",
        icon  = shiny::icon("sliders-h"),
        
        tags$label("Organism", style = "font-weight: 600; font-size: 13px;"),
        selectInput(ns("organism"), NULL,
                    choices  = c("All", "human", "mouse"),
                    selected = "All"
        )
      )
    ),
    
    tags$hr(),
    
    # ── Action buttons  ──────────────────────────────────────────────────────────
    # actionButton(ns("apply_filters"), "Apply Filters",
    #              class = "btn btn-primary btn-block",
    #              style = "background-color: #667eea; border-color: #667eea; width: 100%; margin-bottom: 8px;"
    # ),     # TODO check if needed
    actionButton(ns("reset_filters"), "Reset",
                 class = "btn btn-dark btn-block",
                 style = "width: 100%;"
    )
  )
}

#' @export
sidebar_server <- function(id, registry_con, selected_dataset) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # ── Forward modal open to gene_selector module via side-menu button ────────────────────────
    observeEvent(input$open_gene_modal, {
      # Fire the same trigger that gene_dataset_selector listens for
      # session$sendCustomMessage("open_gene_modal", list())
      shinyjs::click(NS("gene_selector")("open_btn")) #gene_selector_ui?
    })
    
    # ── Display selected genes as tags ────────────────────────────────────
    output$selected_genes_display <- renderUI({
      ds <- selected_dataset()
      if (is.null(ds) || length(ds$genes) == 0) {
        tags$p("No genes selected", style = "color: #aaa; font-size: 12px; margin-bottom: 4px;")
      } else {
        div(
          style = "margin-bottom: 6px;",
          lapply(ds$genes, function(g) {
            tags$span(g,
                      class = "badge",
                      style = "background-color: #667eea; color: white; margin-right: 4px; padding: 4px 8px; border-radius: 10px;"
            )
          })
        )
      }
    })
    
    # ── Dataset picker — only shows datasets containing selected genes ───────────────
    output$dataset_picker_ui <- renderUI({
      ds <- selected_dataset()
      if (is.null(ds) || is.null(ds$available_datasets)) {
        selectInput(ns("selected_datasets"), NULL,
                    choices  = c("Search for a gene first" = ""),
                    selected = ""
        )
      } else {
        choices <- setNames(
          paste0(ds$available_datasets$lab_source, "_", ds$available_datasets$study_id),
          ds$available_datasets$dataset_name
        )
        shinyWidgets::pickerInput(ns("selected_datasets"), NULL,
                                  choices  = choices,
                                  selected = NULL,
                                  multiple = TRUE,
                                  options  = list(
                                    `live-search`  = TRUE,
                                    placeholder    = "Select dataset(s)...",
                                    `selected-text-format` = "count > 2"
                                  )
        )
      }
    })
    
    # ── Cell type picker — dependent on selected dataset ──────────────────────────
    output$cell_type_ui <- renderUI({
      ds <- selected_dataset()
      cts <- ds$cell_types   # populated by gene_dataset_selector
      if (is.null(cts) || length(cts) == 0) {
        tags$p("(select a dataset first)", style = "color:#999; font-size:12px")
      } else {
        checkboxGroupInput(ns("cell_types"), NULL,
                           choices  = cts,
                           selected = cts   # all selected by default
        )
      }
    })
    
    # ── Reset ─────────────────────────────────────────────────────────────
    observeEvent(input$reset_filters, {
      updateSliderInput(session, "lfc_thresh",   value = c(0, 1))
      updateSliderInput(session, "padj_thresh",  value = 0.05)
      updateSelectInput(session, "plot_type",    selected = "")
      updateSelectInput(session, "vis_type",     selected = "")
      updateSelectInput(session, "organism",     selected = "All")
      # session$reload()
    })
    
    # ── Return reactive list for use by data_explore.R ────────────────────
    list(
      plot_type         = reactive(input$plot_type),
      vis_type          = reactive(input$vis_type),
      padj_thresh       = reactive(input$padj_thresh),
      lfc_thresh_min    = reactive(input$lfc_thresh[1]),
      lfc_thresh_max    = reactive(input$lfc_thresh[2]),
      cell_types        = reactive(input$cell_types),
      organism          = reactive(input$organism),
      selected_datasets = reactive(input$selected_datasets)
    )
  })
}




# Claude
#' # view/components/explore_sidebar.R
#' # ─────────────────────────────────────────────────────────────────────────────
#' # Side panel for the "Explore Data" page.
#' # Returns a named list of reactives that drive all plot components.
#' #
#' # Reactive outputs (consumed by data_explore.R):
#' #   $gene          — current searched gene string
#' #   $padj_thresh   — padj significance threshold (slider)
#' #   $lfc_thresh    — |log2FC| minimum (slider)
#' #   $omic_filter   — selected omic type(s)
#' #   $cell_type     — selected cell type filter (NULL = all)
#' #   $direction     — "both" | "up" | "down"
#' #   $n_top_genes   — integer, top N for heatmap
#' # ─────────────────────────────────────────────────────────────────────────────
#' 
#' box::use(
#'   shiny[
#'     NS, moduleServer, reactive, reactiveVal, observeEvent,
#'     tagList, div, hr, tags, p, h5,
#'     textInput, selectInput, sliderInput, numericInput,
#'     checkboxGroupInput, uiOutput, renderUI,
#'     req
#'   ],
#'   shinyWidgets[pickerInput, prettySwitch],
#'   app/logic/query_data/metadata[get_filter_options],
#' )
#' 
#' # ── UI ────────────────────────────────────────────────────────────────────────
#' 
#' #' @export
#' sidebar_ui <- function(id) {
#'   ns <- NS(id)
#'   div(class = "sidebar-panel",
#'       style = "padding: 12px;",
#'       
#'       # ── Active dataset indicator ─────────────────────────────────────────────
#'       uiOutput(ns("dataset_badge")),
#'       hr(style="margin: 8px 0"),
#'       
#'       # ── Significance thresholds ──────────────────────────────────────────────
#'       tags$label("Significance thresholds", style="font-weight:700; font-size:13px"),
#'       
#'       sliderInput(ns("padj_thresh"),
#'                   label   = "padj threshold",
#'                   min     = 0.001, max = 0.2, value = 0.05, step = 0.001,
#'                   ticks   = FALSE
#'       ),
#'       
#'       sliderInput(ns("lfc_thresh"),
#'                   label   = "Minimum |log₂FC|",
#'                   min     = 0, max = 5, value = 0, step = 0.1,
#'                   ticks   = FALSE
#'       ),
#'       
#'       # ── Direction filter ─────────────────────────────────────────────────────
#'       selectInput(ns("direction"), "Show genes",
#'                   choices  = c("Both up & down" = "both", "Upregulated only" = "up", "Downregulated only" = "down"),
#'                   selected = "both"
#'       ),
#'       
#'       hr(style="margin: 8px 0"),
#'       
#'       # ── Cell type filter (populated reactively from dataset metadata) ─────────
#'       tags$label("Cell type filter", style="font-weight:700; font-size:13px"),
#'       uiOutput(ns("cell_type_ui")),
#'       
#'       hr(style="margin: 8px 0"),
#'       
#'       # ── Heatmap settings ─────────────────────────────────────────────────────
#'       tags$label("Heatmap", style="font-weight:700; font-size:13px"),
#'       sliderInput(ns("n_top_genes"),
#'                   label = "Top N DE genes",
#'                   min   = 10, max = 200, value = 50, step = 10,
#'                   ticks = FALSE
#'       ),
#'       
#'       hr(style="margin: 8px 0"),
#'       
#'       # ── Summary counts (re-computed when thresholds change) ──────────────────
#'       uiOutput(ns("sig_count_badge"))
#'   )
#' }
#' 
#' 
#' # ── Server ────────────────────────────────────────────────────────────────────
#' 
#' #' @param registry_con  reactive() returning DBI connection
#' #' @param selected_dataset  reactive() list(lab, study_id, dataset_name, omic_type, gene)
#' #' @return named list of reactives
#' #' @export
#' sidebar_server <- function(id, registry_con, selected_dataset) {
#'   moduleServer(id, function(input, output, session) {
#'     ns <- session$ns
#'     
#'     # ── Dataset badge ─────────────────────────────────────────────────────────
#'     output$dataset_badge <- renderUI({
#'       ds <- selected_dataset()
#'       if (is.null(ds)) {
#'         p("No dataset selected", style="color:#999; font-style:italic; font-size:12px")
#'       } else {
#'         div(class="alert alert-info", style="padding:6px 10px; margin:0; font-size:12px",
#'             tags$b(ds$dataset_name),
#'             tags$br(),
#'             tags$span(paste0(ds$omic_type, " · ", ds$lab, " · study ", ds$study_id),
#'                       style="color:#555")
#'         )
#'       }
#'     })
#'     
#'     # ── Cell type picker — populated from dataset metadata ────────────────────
#'     cell_type_options <- reactive({
#'       ds <- selected_dataset()
#'       req(ds)
#'       tryCatch({
#'         opts <- get_filter_options(registry_con(), ds$lab, ds$study_id)
#'         ct   <- opts$cell_types[[1]]
#'         ct[!is.na(ct)]
#'       }, error = function(e) character(0))
#'     })
#'     
#'     output$cell_type_ui <- renderUI({
#'       cts <- cell_type_options()
#'       if (length(cts) == 0) {
#'         p("(not available for this dataset)", style="color:#999; font-size:12px")
#'       } else {
#'         shinyWidgets::pickerInput(
#'           ns("cell_type"), NULL,
#'           choices  = c("All cell types" = "", cts),
#'           selected = "",
#'           multiple = FALSE,
#'           options  = list(`live-search` = TRUE, size = 8)
#'         )
#'       }
#'     })
#'     
#'     # ── Significant gene count badge ──────────────────────────────────────────
#'     output$sig_count_badge <- renderUI({
#'       ds <- selected_dataset()
#'       req(ds)
#'       # Rough estimate from pre-computed stats; exact count recalculated by plots
#'       div(style="font-size:12px; color:#555",
#'           p(style="margin:2px 0",
#'             tags$b("Dataset features: "),
#'             textOutput(ns("feat_n"), inline=TRUE)
#'           )
#'       )
#'     })
#'     
#'     # Return reactive list — consumed by data_explore.R and plot components
#'     list(
#'       padj_thresh   = reactive(input$padj_thresh),
#'       lfc_thresh    = reactive(input$lfc_thresh),
#'       direction     = reactive(input$direction),
#'       cell_type     = reactive({ ct <- input$cell_type; if (is.null(ct) || ct == "") NULL else ct }),
#'       n_top_genes   = reactive(input$n_top_genes)
#'     )
#'   })
#' }
