# ─────────────────────────────────────────────────────────────────────────────────────────────
# Modal popup: search a gene/ protein to see all datasets containing it (with metadata preview)
# User selects 1+ dataset to explore.
#
# Flow:
#   1. User clicks "Search Gene / Protein" button on data_explore page
#   2. Modal opens with search bar + omic/ lab filters
#   3. Results table shows matching datasets + stats (total features, n_sig, conditions)
#   4. User selects a row, clicks "Explore"
#   5. selected_dataset reactiveVal() is updated, modal closes and plots update
# ─────────────────────────────────────────────────────────────────────────────────────────────

box::use(
  app/logic/query_data/query_builder[fetch_all_datasets, fetch_datasets_for_gene],
  dplyr[mutate, rename, select],
  DT[ datatable, dataTableProxy, DTOutput, renderDT, selectRows],
  glue[glue],
  jsonlite[fromJSON],
  shiny[...],
)

#' @export
ui <- function(id) {
  ns <- NS(id)
  tagList(
    actionButton(
      ns("open_btn"),
      label = "Search Gene or Protein",
      class = "btn btn-primary btn-lg",
      multiple = TRUE,
      options = list(
        placeholder = "Start typing...",
        maxOptions = 30,
        maxItems = NULL,
        create = FALSE
      ),
      style = "margin-bottom: 12px;"
    )
    # The modal is injected by the server on click — no static placeholder needed
  )
}


#' @param registry_con      reactive() returning a valid DBI connection
#' @param selected_dataset  reactiveVal() updated when user confirms a dataset
#'                          Value: list(lab, study_id, dataset_name, omic_type, gene)
#' @export
server <- function(id, registry_con, selected_dataset) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # ── Open modal ────────────────────────────────────────────────────────────
    observeEvent(input$open_btn, {
      showModal(
        modalDialog(
          title    = "Find dataset(s) containing your gene(s) or protein(s)",
          size     = "xl",
          easyClose = TRUE,
          footer   = NULL,

          fluidRow(
            # ── Left: search controls ──────────────────────────────────────
            column(3,
              div(class = "well well-sm",
                tags$label("Gene symbol or protein ID", style = "font-weight:600"),
                textInput(ns("gene_query"), NULL,
                          placeholder = "e.g. GFAP, AQP4, Q9Z223"),
                selectInput(ns("omic_filter"), "Omic type",
                            choices  = c("All", "proteomics", "scrna", "snrna", "bulk"),
                            selected = "All"),
                selectInput(ns("lab_filter"), "Lab",
                            choices  = c("All", "diaz", "hong", "williams"),
                            selected = "All"),
                numericInput(ns("padj_preview"), "Preview padj threshold",
                             value = 0.05, min = 0.001, max = 1, step = 0.001),
                actionButton(ns("search_btn"), "Search",
                             class = "btn btn-primary btn-block",
                             icon  = shiny::icon("search")),
                hr(),
                # Confirm button — only enabled after a row is selected
                uiOutput(ns("confirm_ui"))
              )
            ),

            # ── Right: results ────────────────────────────────────────────
            column(9,
              h5("Datasets containing this gene:"),
              DTOutput(ns("hits_tbl")),
              hr(),
              h5("Selected dataset — metadata preview:"),
              uiOutput(ns("meta_preview"))
            )
          )
        )
      )
    })


    # ── Search ────────────────────────────────────────────────────────────────
    hits_data <- reactiveVal(data.frame())

    observeEvent(input$search_btn, {
      req(nchar(trimws(input$gene_query)) >= 1) # changed from 2

      results <- tryCatch(
        fetch_datasets_for_gene(
          registry_con(),
          query      = input$gene_query,
          omic_type  = if (input$omic_filter  == "All") NULL else input$omic_filter,
          lab_source = if (input$lab_filter   == "All") NULL else input$lab_filter
        ),
        error = function(e) {
          shiny::showNotification(paste("Search error:", e$message), type = "error")
          data.frame()
        }
      )
      hits_data(results)
    })


    # ── Hits table ────────────────────────────────────────────────────────────
    output$hits_tbl <- renderDT({
      df <- hits_data()
      req(nrow(df) > 0)

      # Friendly display columns (hide JSON blobs)
      display <- df |>
        dplyr::select(
          Lab          = lab_source,
          `Dataset`    = dataset_name,
          `Omic`       = omic_type,
          `Gene`       = gene_symbol,
          `Features`   = total_features,
          `Significant (padj<0.05)` = n_sig_features,
          `Samples/Cells` = total_samples,
          `Cell types` = n_cell_types,
          `Conditions` = n_conditions
        )

      datatable(display,
        selection = "single",
        rownames  = FALSE,
        class     = "table-sm table-hover",
        options   = list(
          pageLength   = 8,
          scrollX      = TRUE,
          dom          = "frtip",
          columnDefs   = list(list(className = "dt-right", targets = 4:8))
        )
      )
    })


    # ── Metadata preview card ─────────────────────────────────────────────────
    output$meta_preview <- renderUI({
      row_idx <- input$hits_tbl_rows_selected
      req(row_idx, nrow(hits_data()) > 0)

      row <- hits_data()[row_idx, ]

      # Parse JSON arrays for display
      cell_types  <- tryCatch(paste(jsonlite::fromJSON(row$cell_types_json  %||% "[]"), collapse=", "), error=function(e) "—")
      conditions  <- tryCatch(paste(jsonlite::fromJSON(row$conditions_json  %||% "[]"), collapse=", "), error=function(e) "—")

      div(class = "panel panel-default",
        div(class = "panel-body",
          fluidRow(
            column(4, tags$b("Dataset:"),    p(row$dataset_name)),
            column(4, tags$b("Lab:"),        p(row$lab_source)),
            column(4, tags$b("Omic type:"),  p(row$omic_type))
          ),
          fluidRow(
            column(3, tags$b("Total features:"),  p(format(row$total_features, big.mark=","))),
            column(3, tags$b("Significant:"),     p(format(row$n_sig_features,  big.mark=","))),
            column(3, tags$b("Samples/ Cells:"),  p(format(coalesce_na(row$total_samples, row$total_cells), big.mark=","))),
            column(3, tags$b("Cell types:"),      p(row$n_cell_types %||% "—"))
          ),
          fluidRow(
            column(6, tags$b("Conditions:"), p(conditions)),
            column(6, tags$b("Cell types:"), p(cell_types))
          )
        )
      )
    })


    # ── Confirm button — shown only when a row is selected ───────────────────
    output$confirm_ui <- renderUI({
      req(input$hits_tbl_rows_selected)
      actionButton(ns("confirm_btn"), "✓ Explore selected dataset(s)",
                   class = "btn btn-success btn-block")
    })


    # ── Confirm selection updates reactiveVal and closes modal ─────────────────
    observeEvent(input$confirm_btn, {
      row_idx <- input$hits_tbl_rows_selected
      req(row_idx, nrow(hits_data()) > 0)
      row <- hits_data()[row_idx, ]

      selected_dataset(list(
        lab        = row$lab_source,
        study_id   = row$study_id,
        dataset_name = row$dataset_name,
        omic_type  = row$omic_type,
        gene       = trimws(input$gene_query)
      ))
      removeModal()
    })

  })
}


# ── Utilities ─────────────────────────────────────────────────────────────────
`%||%` <- function(a, b) if (!is.null(a) && !is.na(a) && a != "") a else b
coalesce_na <- function(...) {
  for (x in list(...)) if (!is.null(x) && !is.na(x)) return(x)
  NA
}