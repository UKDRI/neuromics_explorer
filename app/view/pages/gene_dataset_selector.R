# ─────────────────────────────────────────────────────────────────────────────────────────────
# Modal popup: search a gene/ protein to see all datasets containing it (with metadata preview)
# User selects 1+ dataset to explore.
#
# User flow:
#   1. User clicks "Search Gene / Protein" button on data_explore page
#   2. Modal opens with search bar and omic/ lab filters
#   3. Results table shows matching datasets and stats (total features, n_sig, conditions)
#   4. User selects a row, clicks "Explore"
#   5. selected_dataset reactiveVal() is updated, modal closes and plots update
# ─────────────────────────────────────────────────────────────────────────────────────────────

box::use(
  app/logic/query_data/query_builder[fetch_all_datasets, fetch_datasets_for_gene, fetch_metadata_filter_options],
  dplyr[mutate, rename, select],
  DT[ datatable, dataTableProxy, DTOutput, renderDT, selectRows],
  glue[glue],
  jsonlite[fromJSON],
  shiny[...],
)

#' @export
gene_selector_ui <- function(id) {
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
gene_selector_server <- function(id, registry_con, selected_dataset) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # ── Open modal ────────────────────────────────────────────────────────────
    observeEvent(input$open_btn, {
      showModal(
        modalDialog(
          title     = "Find dataset(s) containing your gene(s) or protein(s) of interest",
          size      = "xl",
          easyClose = TRUE,
          footer    = NULL,

          fluidRow(
            # ── Left: search controls ──────────────────────────────────────
            column(3,
              div(class = "well well-sm",
                # tags$label("Gene symbol or protein ID", style = "font-weight:600"),
                selectizeInput(ns("gene_query"), "Gene symbol(s) or protein ID(s)",
                    choices  = NULL,
                    multiple = TRUE,
                    options  = list(
                        placeholder = "Start typing e.g. GAPDH, AQP4...",
                        create      = TRUE,     # allow typing values not in the list
                        maxItems    = 15,   # NULL
                        maxOptions  = 30,
                        plugins     = list("remove_button"), # adds × on each tag
                        loadThrottle = 300
                )),
                # TODO handle dynamically (i.e. choices = NULL)
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
                # Confirm button — only enabled after a row is selected
                uiOutput(ns("confirm_ui"))
              )
            ),

            # ── Right: preview results ───────────────────────────────────────
            column(9,
              h5("Datasets containing selected gene(s):"),
              DTOutput(ns("hits_tbl")),
              hr(),
              h5("Selected datasets — metadata preview:"),
              uiOutput(ns("meta_preview"))
            )
          )
        )
      )
      # Load gene list from index once modal opens - tryCatch to gaurd against main_setup.py not yet running
      genes_sql <- tryCatch(
        DBI::dbGetQuery(
            registry_con(),
            "SELECT DISTINCT gene_symbol 
                FROM gene_study_index 
                ORDER BY gene_symbol"
        )$gene_symbol,
        error = function(e) {
            showNotification(
                "Gene index not available — startup may not have completed.",
                type = "error", duration = 10
            )
            character(0)   # empty — selectize works without suggestions
        }
      )

      updateSelectizeInput(
        session,
        "gene_query",
        choices  = genes_sql,
        server   = TRUE     # streams, doesn't dump all genes into the page
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
        selection = "multiple",
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
      row_idxs <- input$hits_tbl_rows_selected
      req(length(row_idxs) > 0)
      # req(row_idx, nrow(hits_data()) > 0)
      row <- hits_data()[row_idxs, ]

      # List of vectors
      selected_dataset(list(
        genes      = trimws(strsplit(input$modal_gene, ",")[[1]]),
        lab        = row$lab_source,
        study_id   = row$study_id,
        dataset_name = row$dataset_name,
        omic_type  = row$omic_type[1],
        available_datasets = rows,
        cell_types       = unique(unlist(
            lapply(seq_len(nrow(rows)), function(i)
                tryCatch(fetch_metadata_filter_options(registry_con(), rows$lab_source[i], rows$study_id[i])$cell_types[[1]],
                    error = function(e) NULL)
            )
        ))
      ))
      removeModal()
    })

  })
}


# ── Utilities - defensive null-handling helpers ─────────────────────────────────────────────────────────────────
`%||%` <- function(a, b) if (!is.null(a) && !is.na(a) && a != "") a else b
coalesce_na <- function(...) {
  for (x in list(...)) if (!is.null(x) && !is.na(x)) return(x)
  NA
}