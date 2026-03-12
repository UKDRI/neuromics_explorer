box::use(
  shiny[moduleServer, NS, tagList, selectInput, selectizeInput,
        sliderInput, checkboxGroupInput, actionButton, observeEvent]
)

#' @export
sidebar_ui <- function(id) {
  ns <- NS(id)

  tagList(
    selectInput(ns("dataset"), "Select Dataset(s)", choices = NULL, multiple = TRUE),
    # choices = c("Dataset 1" = "dataset_001", "Dataset 2" = "dataset_002"),
    selectInput(ns("plot_type"), "Plot type", choices = NULL),
    # choices = c("QC", "Embedding", "Expression")),
    selectInput(ns("viz_type"), "Visualisation", choices = NULL),
    # choices = c("violin", "histogram")),
    # choices = c("PCA", "UMAP", "t-SNE")),
    # choices = c("dot", "volcano", "heatmap")),
    selectizeInput(ns("genes"), "Genes", choices = NULL, multiple = TRUE),
    checkboxGroupInput(ns("cell_types"), "Cell Types",
                       choices = c("T cells", "B cells", "Monocytes")),
    #choices = NULL),
    actionButton(ns("apply_filters"), "Apply Filters", class = "btn-primary"),
    actionButton(ns("reset_inputs"), " Reset", class = "btn-secondary mt-2"),
    sliderInput(ns("lfc"), "LogFC", -8, 8, c(-0.5, 0.5)),
    sliderInput(ns("pval"), "Adjusted p-val", 0, 0.05, c(0, 0.05), step = 0.001)
  )
}


#' @export
sidebar_server <- function(id) {
  moduleServer(id, function(input, output, session) {

    # Reset inputs to their original values
    observeEvent(input$reset_inputs, {
      session$reload()
    })

    # # Apply filters
    # observeEvent(input$apply_filters, {
    #   # Access input values
    #   selected_dataset <- input$dataset
    # })

  })
}

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
