# ────────────────────────────────────────────────────────────────────────────────────────
# Module contains the Explorer page sidebar.
# It shows modal-selected genes and the plot/filter controls that shape the API queries.
# ────────────────────────────────────────────────────────────────────────────────────────

box::use(
  # shiny[...],
  shiny[moduleServer, NS, tagList, selectInput, updateSelectInput,
        sliderInput, updateSliderInput, checkboxGroupInput, actionButton,
        observeEvent, uiOutput, renderUI, tags, reactive, debounce],
  bslib[accordion, accordion_panel],
  app/view/components/helpers/help_tips[tool_tip, label_and_tooltip, tip_text],
)

# log2(1.5) = 0.585; default rounded to the slider's 0.01 step. Shared by the input and the 'Reset' button.
LFC_DEFAULT <- 0.58

# Text kept in one place so the tooltips stay in step with what the plots actually consume
# (see the plot servers at the bottom of data_explorer.R for the wiring).
TOOLTIP_TEXT <- tip_text(
  tags$div(style = "font-weight:600; margin-bottom:4px;",
    "These two thresholds decide which features count as significant."),
  tags$div(style = "margin-top:4px;", "Drives both the Plot and Compare tabs:"),
  tags$ul(style = "padding-left:16px; margin:2px 0;",
    tags$li(tags$b("Volcano"), " - Up/Down colouring; dashed threshold lines"),
    tags$li(tags$b("Heatmap"), " - which top genes are fetched and shown in addition to your gene list"),
    tags$li(tags$b("Violin"), " - Up / Down / NS grouping"),
    tags$li(tags$b("Feature Scatter"), " - point colouring")
  ),
  tags$div(style = "font-weight:600; margin-top:6px;", "Gene-Drug Explorer :"),
  tags$ul(style = "padding-left:16px; margin:2px 0;", 
    tags$li(style = "margin-top:4px;", "which drug hits count as significant (padj only)")
  ),
  tags$div(style = "font-weight:600; margin-top:6px;", "They do not affect:"),
  tags$div("Expression data table, Histogram, Dots Plot, UMAP, or the drug-panel heatmap.")
)

TOOLTIP_PADJ <- tip_text(
  "Adjusted p-value cut-off. A feature is significant when its padj is ",
  tags$b("below"), " this value.",
  tags$div(style = "margin-top:4px;", "E.g. 0.05 = accept ~5% false discoveries among the calls. Lower is stricter.")
)

TOOLTIP_LFC <- tip_text(
  "Minimum absolute log\u2082 fold change, applied symmetrically",
  tags$div(style = "margin-top:4px;",
    "E.g. 0.58 \u2248 1.5-fold \u00b7 1 = 2-fold \u00b7 2 = 4-fold. Set to 0 to require significance only.")
)

#' @export
sidebar_ui <- function(id) {
  ns <- NS(id)
  tagList(
    
    # ── Selected genes from the modal workflow ─────────────────────────────
    tags$label("Selected search terms", style = "font-weight: 600; color: #333;"),
    uiOutput(ns("selected_terms_display")),

    tags$hr(),
    
    # ── Plot type and query-side filters ─────────────────────────────────────
    tags$label("Plot type", style = "font-weight: 600; color: #333;"),
    selectInput(ns("plot_type"), NULL,
                choices  = c("Volcano", "Heatmap", "Violin", "Feature Scatter",
                             "Histogram", "Dots Plot", "UMAP"
                             #, "Top Features"
                            ),
                selected = "Volcano"
    ),
    
    tags$hr(),
    
    # ── Significance threshold filters (collapsible) ─────────────────────────────
    accordion(
      id = ns("filter_accordion"),
      multiple = TRUE,
      open = FALSE,
      
      accordion_panel(
        title = tags$span("Significance Filters", tool_tip(TOOLTIP_TEXT)),
        value = "significance_filters",   # required: bslib defaults value to title, which must then be a string
        icon  = shiny::icon("filter"),
        
        # Default 0.58 = log2(1.5), the conventional 1.5-fold cut-off. Max 5 ~32-fold.
        label_and_tooltip("Minimum | log₂FC |", TOOLTIP_LFC),
        sliderInput(ns("lfc_thresh"),
                    label = NULL, min = 0, max = 5,
                    value = LFC_DEFAULT,
                    step  = 0.01, ticks = FALSE   # 0.01 so the 0.58 default is representable (0.1 would snap it to 0.6)
        ),
        
        label_and_tooltip("padj threshold", TOOLTIP_PADJ),
        sliderInput(ns("padj_thresh"),
                    label = NULL, min = 0.001, max = 0.2,
                    value = 0.05, step = 0.001, ticks = FALSE
        )
      ),
      
      # ── Additional filters (collapsible) - populated reactively from selected dataset's metadata
      # accordion_panel(
      #   title = "Cell Type Filter",
      #   icon  = shiny::icon("dna"),   # TODO select different icon
      #   uiOutput(ns("cell_type_ui"))
      # ),

      # accordion_panel(
      #   title = "Additional Filters",
      #   icon  = shiny::icon("sliders-h"),
        
      #   tags$label("Organism", style = "font-weight: 600; font-size: 13px;"),
      #   selectInput(ns("organism"), NULL,
      #               choices  = c("All", "human", "mouse"),
      #               selected = "All"
      #   )
      # )
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
sidebar_server <- function(id, selected_dataset) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # ── Display selected search terms as tags ─────────────────────────────
    output$selected_terms_display <- renderUI({
      ds <- selected_dataset()
      terms <- c(ds$genes, ds$proteins)
      if (is.null(ds) || length(terms) == 0) {
        tags$p("No search terms selected", style = "color: #aaa; font-size: 12px; margin-bottom: 4px;")
      } else {
        tags$div(
          style = "margin-bottom: 6px;",
          lapply(terms, function(term) {
            tags$span(term,
                      class = "badge",
                      style = "background-color: #667eea; color: white; margin-right: 4px; padding: 4px 8px; border-radius: 10px;"
            )
          })
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
      updateSliderInput(session, "lfc_thresh",   value = LFC_DEFAULT)
      updateSliderInput(session, "padj_thresh",  value = 0.05)
      updateSelectInput(session, "plot_type",    selected = "Volcano")
      updateSelectInput(session, "organism",     selected = "All")
      # session$reload()
    })

    observeEvent(selected_dataset(), {
      ds <- selected_dataset()
      choices <- c("Volcano", "Heatmap", "Violin", "Feature Scatter",
                   "Histogram", "Dots Plot", "UMAP"
                   #, "Top Features"
                   )
      # if (!is.null(ds) && ds$omic_type %in% c("scrna", "snrna")) {
      #   choices <- c(choices, "UMAP")
      # }

      selected_plot <- input$plot_type %||% "Volcano"
      if (!selected_plot %in% choices) {
        selected_plot <- choices[[1]]
      }

      updateSelectInput(session, "plot_type", choices = choices, selected = selected_plot)
    }, ignoreInit = FALSE)
    
    # ── Return reactive list for use by data_explore.R ────────────────────
    list(
      plot_type         = reactive(input$plot_type),
      # Debounced: the thresholds are now part of the plot cache keys downstream, so without this
      # a slider drag would create one cache entry per tick (step is 0.01). 300 ms lets a drag
      # settle into a single recompute.
      padj_thresh       = debounce(reactive(input$padj_thresh), 300),
      lfc_thresh_min    = debounce(reactive(input$lfc_thresh), 300),
      cell_types        = reactive(input$cell_types),
      organism          = reactive(input$organism)
    )
  })
}

`%||%` <- function(a, b) if (!is.null(a)) a else b




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
