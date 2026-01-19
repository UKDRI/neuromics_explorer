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
    # choices = c("QC", "DimRed", "Violin", "Dot", "Volcano", "Heatmap")),
    selectInput(ns("viz_type"), "Visualisation", choices = NULL),
    # choices = c("UMAP", "t-SNE", "PCA")),
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