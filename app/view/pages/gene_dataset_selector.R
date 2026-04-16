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
  app/logic/api/api_client[fetch_all_datasets, fetch_datasets_for_terms, fetch_gene_index_genes,
                           fetch_metadata_filter_options, fetch_protein_index_ids],
  bslib[tooltip],
  dplyr[arrange, bind_rows, group_by, select, summarise, semi_join],
  DT[ datatable, DTOutput, renderDT],
  jsonlite[fromJSON],
  shiny[...],
)

# Launch button for the modal-first dataset search workflow.
# The alphabetical suggestion list and narrowing behavior belong to the
# selectize inputs inside the modal, not to this button.
#' @export
gene_selector_ui <- function(id) {
  ns <- NS(id)
  tagList(
    actionButton(
      ns("open_btn"),
      label = "Search gene or protein",
      class = "btn btn-primary btn-lg",
      # multiple = TRUE,
      # options = list(
      #   placeholder = "Start typing...",
      #   maxOptions = 30,
      #   maxItems = NULL,
      #   create = FALSE
      # ),
      style = "margin-bottom: 12px; background: linear-gradient(135deg, rgb(225, 35, 92) 100%);"
    )
    # The modal is injected by the server on click — no static placeholder needed
  )
}

#' @param selected_dataset  reactiveVal() updated when user confirms a dataset
#'                          Value: list(lab_source, study_id, dataset_name, omic_type, genes, proteins)
#' @export
gene_selector_server <- function(id, selected_dataset) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Resolve cell-type choices for the active dataset so the sidebar can update immediately.
    get_active_cell_types <- function(rows) {
      if (nrow(rows) == 0) return(character(0))

      active_row <- rows[1, , drop = FALSE]
      opts <- tryCatch(
        fetch_metadata_filter_options(
          active_row$lab_source[1],
          active_row$study_id[1]
        ),
        error = function(e) NULL
      )

      if (is.null(opts) || !"cell_types" %in% names(opts) || length(opts$cell_types) == 0) {
        return(character(0))
      }

      cts <- opts$cell_types[[1]]
      cts[!is.na(cts) & nzchar(cts)]
    }

    # Collapse selected rows down to one row per dataset and keep the first row active.
    build_selected_payload <- function(rows, genes, proteins) {
      selected_rows <- rows |>
        dplyr::group_by(
          lab_source, study_id, dataset_name, omic_type,
          total_features, n_sig_features, total_samples, total_cells,
          n_cell_types, n_conditions, cell_types_json, conditions_json
        ) |>
        dplyr::summarise(
          matched_genes = paste(
            sort(unique(c(
              gene_symbol
            ))),
            collapse = ", "
          ),
          matched_proteins = paste(
            sort(unique(c(
              unlist(lapply(protein_id, parse_json_text), use.names = FALSE)
            ))),
            collapse = ", "
          ),
          .groups = "drop"
        ) |>
        dplyr::arrange(lab_source, study_id)

      list(
        genes             = genes,
        proteins          = proteins,
        lab_source        = selected_rows$lab_source[1],
        study_id          = selected_rows$study_id[1],
        dataset_name      = selected_rows$dataset_name[1],
        omic_type         = selected_rows$omic_type[1],
        selected_datasets = selected_rows,
        cell_types        = get_active_cell_types(selected_rows)
      )
    }

    # ── Open modal ────────────────────────────────────────────────────────────
    # This observeEvent is the UI entry point for dataset search. It opens the
    # modal, fetches starter suggestion lists, and resets both selectize inputs
    # so stale previous selections do not appear when the modal is reopened.
    observeEvent(input$open_btn, {
      showModal(
        modalDialog(
          title     = "Find datasets containing your genes or proteins of interest",
          size      = "xl",
          easyClose = TRUE,
          footer    = NULL,
          # style     = "max-width: 1400px; width: 95vw;",
          style     = "flex: 1 1 auto; min-width: 0;",

          tags$div(
            style = "display: flex; gap: 16px; align-items: flex-start;",

            # ── Left: search controls ───────────────────────────────────────
            tags$div(
              style = "flex: 0 0 290px;",   #fixed-width
              div(class = "well well-sm", style = "margin-bottom: 0;",
                # tags$label("Gene symbol or protein ID", style = "font-weight:600"),
                selectizeInput(ns("gene_query"), "Gene symbol(s)",
                    choices  = NULL,
                    multiple = TRUE,
                    options  = list(
                        placeholder = "Start typing e.g. GAPDH, AQP4...",
                        create      = FALSE,
                        maxItems    = 15,
                        maxOptions  = 30,
                        preload     = "focus",
                        openOnFocus = TRUE,
                        closeAfterSelect = FALSE,
                        plugins     = list("remove_button"),           # adds × on each tag 
                        loadThrottle = 300
                )),
                tags$div(
                  style = "display: flex; align-items: center; gap: 6px; margin-top: -8px; margin-bottom: 10px; color: #555; font-size: 12px;",
                  tags$span("Why do some genes appear more than once?"),
                  bslib::tooltip(
                    tags$span(
                      shiny::icon("circle-info"),
                      style = "display: inline-flex; align-items: left; color: #667eea; cursor: help;"
                    ),
                    "The app preserves gene symbols as they were provided by each dataset. Human genes are often uppercase, while mouse genes only capitalise the first letter, so both forms can appear separately in the search box.",
                    placement = "right"
                  )
                ),
                selectizeInput(ns("protein_query"), "Protein ID(s)",
                    choices  = NULL,
                    multiple = TRUE,
                    options  = list(
                        placeholder = "Start typing e.g. P04406...",
                        create      = FALSE,
                        maxItems    = 15,
                        maxOptions  = 30,
                        preload     = "focus",
                        openOnFocus = TRUE,
                        closeAfterSelect = FALSE,
                        plugins     = list("remove_button"),
                        loadThrottle = 300
                )),
                selectInput(ns("omic_filter"), "Omic type",
                            choices  = c("All", "proteomics", "scrna", "snrna", "bulk"),
                            selected = "All"),
                selectInput(ns("lab_filter"), "Lab",
                            choices  = c("All", "diaz", "hong", "williams"),
                            selected = "All"),
                actionButton(ns("search_btn"), "Search",
                             class = "btn btn-primary btn-block",
                             icon  = shiny::icon("search")),
                # Confirm button — only enabled after a row is selected
                uiOutput(ns("confirm_ui"))
              )
            ),

            # ── Right: preview results ──────────────────────────────────
            tags$div(
              style = "flex: 1 1 auto; min-width: 0;",      #flexible results area grows with modal width
              div(
                class = "alert alert-info",
                role = "alert",
                style = "font-size: 16px; line-height: 1.45; margin-bottom: 12px;",
                tags$strong("How to use this search"),
                tags$ol(
                  style = "font-size: 14px; padding-left: 18px; margin: 8px 0 0;",
                  tags$li("Type or select one or more genes and/or proteins."),
                  tags$li("Click 'Search' button to list datasets containing those terms."),
                  tags$li("Click to select one or more dataset rows from the results table below."),
                  tags$li("Review the metadata preview shown below the table."),
                  tags$li("Click 'Explore selected dataset(s)' to open them in the main explorer page.")
                )
              ),
              h5("Datasets containing searched gene and protein terms:"),
              DTOutput(ns("hits_tbl")),
              hr(),
              h5("Selected datasets — metadata preview:"),
              uiOutput(ns("meta_preview"))
            )
          )
        )
      )
      registered_datasets <- tryCatch(
        fetch_all_datasets(),
        error = function(e) data.frame()
      )
      lab_choices <- c("All", sort(unique(registered_datasets$lab_source)))
      lab_choices <- lab_choices[!is.na(lab_choices) & nzchar(lab_choices)]
      # if (length(lab_choices) == 0) lab_choices <- c("All", "diaz", "hong", "williams")
      updateSelectInput(session, "lab_filter", choices = lab_choices, selected = "All")

      # Load the first alphabetical slice of available genes/proteins so the
      # selectize dropdown shows starter suggestions before typing.
      genes_sql <- tryCatch(
        fetch_gene_index_genes(),
        error = function(e) {
            showNotification(
              paste0(
                "Gene index route unavailable. Restart the FastAPI service. ",
                e$message
              ),
              type = "error", duration = 10
            )
            character(0)   # empty — selectize works without suggestions
        }
      )

      proteins_sql <- tryCatch(
        fetch_protein_index_ids(),
        error = function(e) character(0)
      )

      updateSelectizeInput(
        session,
        "gene_query",
        choices  = genes_sql,
        selected = character(0),
        server   = TRUE
      )

      updateSelectizeInput(
        session,
        "protein_query",
        choices  = proteins_sql,
        selected = character(0),
        server   = TRUE
      )
    })


    # ── Search with genes and/or proteins ──────────────────────────────────
    # This reactiveVal acts as the modal's shared result store. The hits table,
    # metadata preview, and confirm action all read from the same search result.
    hits_data <- reactiveVal(data.frame())

    # This observeEvent runs only when the user clicks Search. It translates the
    # current modal inputs into one API query and updates the shared results.
    observeEvent(input$search_btn, {
      search_genes <- unique(trimws(input$gene_query %||% character(0)))
      search_genes <- search_genes[nzchar(search_genes)]
      search_proteins <- unique(trimws(input$protein_query %||% character(0)))
      search_proteins <- search_proteins[nzchar(search_proteins)]
      req(length(search_genes) > 0 || length(search_proteins) > 0)

      results <- tryCatch({
        fetch_datasets_for_terms(
            genes      = search_genes,
            proteins   = search_proteins,
            omic_type  = if (input$omic_filter  == "All") NULL else input$omic_filter,
            lab_source = if (input$lab_filter   == "All") NULL else input$lab_filter
          ) |>
          dplyr::mutate(
            protein_label = vapply(protein_id, format_json_values, character(1))
          ) |>
          dplyr::arrange(lab_source, study_id, gene_symbol)
      }, error = function(e) {
        shiny::showNotification(paste("Search error:", e$message), type = "error")
        data.frame()
      })

      hits_data(results)
    })


    # ── Hits table ────────────────────────────────────────────────────────────
    # This render block presents the current search result for row selection.
    hits_display <- reactive({
      df <- hits_data()
      if (nrow(df) == 0) return(data.frame()) #req(nrow(df) > 0)

      # Friendly display columns (hides JSON blobs)
      df |>
        dplyr::group_by(lab_source, study_id, dataset_name, omic_type,
                        total_features, n_sig_features, total_samples,
                        total_cells, n_cell_types, n_conditions, cell_types_json, conditions_json) |>
        dplyr::summarise(
          Gene    = paste(sort(unique(gene_symbol[!is.na(gene_symbol) & nzchar(gene_symbol)])), collapse = ", "),
          Protein = {
            vals <- unique(unlist(lapply(protein_id, parse_json_text), use.names = FALSE))
            vals <- vals[nzchar(vals)]
            if (length(vals) == 0) "—" else paste(sort(vals), collapse = ", ")
          },
          .groups = "drop"
        ) |>
        dplyr::arrange(lab_source, dataset_name)
    })

    output$hits_tbl <- renderDT({
      display <- hits_display()
      req(nrow(display) > 0)

      display <- display |>
        dplyr::select(
          Lab          = lab_source,
          `Dataset`    = dataset_name,
          `Modality`   = omic_type,
          `Gene`       = Gene,
          `Protein`    = Protein,
          `Total Features` = total_features,
          `Total Significant (padj<0.05)` = n_sig_features,
          `Total Samples`  = total_samples,
          `Cell Types`     = n_cell_types,
          `Conditions`     = n_conditions
        )

      datatable(display,
        selection = "multiple",
        rownames  = FALSE,
        class     = "table-sm table-hover",
        options   = list(
          pageLength   = 8,
          scrollX      = TRUE,
          dom          = "frtip",
          columnDefs   = list(list(className = "dt-right", targets = 5:9))
        )
      )
    })


    # ── Metadata preview cards for selected datasets ────────────────────────
    # This render block reacts to the selected rows in the hits table and shows
    # compact side-by-side metadata cards for those candidate datasets.
    output$meta_preview <- renderUI({
      row_idx <- input$hits_tbl_rows_selected
      rows <- hits_display()[row_idx, , drop = FALSE]

      # Parse JSON arrays for display
      tags$div(
        style = "display: grid; grid-template-columns: repeat(auto-fit, minmax(240px, 1fr)); gap: 10px;",
        lapply(seq_len(nrow(rows)), function(i) {
          row <- rows[i, , drop = FALSE]
          cell_types <- tryCatch(
            paste(jsonlite::fromJSON(row$cell_types_json %||% "[]"), collapse = ", "),
            error = function(e) "—"
          )
          conditions <- tryCatch(
            paste(jsonlite::fromJSON(row$conditions_json %||% "[]"), collapse = ", "),
            error = function(e) "—"
          )

          tags$div(
            style = "border: 1px solid #d9dee3; border-radius: 8px; padding: 10px 12px; background: #fff;",
            tags$div(style = "font-weight: 700; margin-bottom: 4px;", row$dataset_name),
            tags$div(style = "font-size: 12px; color: #555; margin-bottom: 8px;",
                     paste(row$lab_source, "·", row$omic_type)),
            tags$div(style = "font-size: 12px; line-height: 1.5;",
                     tags$div(tags$b("Total Features: "), format(row$total_features, big.mark = ",")),
                     tags$div(tags$b("Total Significant: "), format(row$n_sig_features, big.mark = ",")),
                     tags$div(tags$b("Samples/Cells: "), format(coalesce_na(row$total_samples, row$total_cells), big.mark = ",")),
                     tags$div(tags$b("Cell types: "), row$n_cell_types %||% "—"),
                     tags$div(tags$b("Conditions: "), conditions %||% "—"),
                     tags$div(tags$b("Cell types: "), cell_types %||% "—"))
          )
        })
      )
    })


    # ── Confirm button — shown only when a row is selected ───────────────────
    # This render block enables the modal confirm action only after at least
    # one dataset row has been selected in the search results table.
    output$confirm_ui <- renderUI({
      req(input$hits_tbl_rows_selected)
      actionButton(ns("confirm_btn"), "✓ Explore selected dataset(s)",
                   class = "btn btn-success btn-block")
    })


    # ── Confirm selection updates reactiveVal and closes modal ─────────────────
    # This observeEvent commits the modal selection into shared explorer state,
    # which then drives the dataset listing, plots, and sidebar terms.
    observeEvent(input$confirm_btn, {
      row_idxs <- input$hits_tbl_rows_selected
      req(length(row_idxs) > 0)
      # selected_rows <- hits_display()[row_idxs, , drop = FALSE]
      selected_keys <- hits_display()[row_idxs, c("lab_source", "study_id", "dataset_name")]
      selected_rows <- hits_data() |>
        dplyr::semi_join(selected_keys,
                        by = c("lab_source", "study_id", "dataset_name"))
      genes <- unique(trimws(input$gene_query %||% character(0)))
      genes <- genes[nzchar(genes)]
      proteins <- unique(trimws(input$protein_query %||% character(0)))
      proteins <- proteins[nzchar(proteins)]
      req(length(genes) > 0 || length(proteins) > 0)

      selected_dataset(build_selected_payload(selected_rows, genes, proteins))
      removeModal()
    })

  })
}


# ── Utilities - defensive null-handling helpers ───────────────────────────
`%||%` <- function(a, b) {
  if (is.null(a) || length(a) == 0) return(b)
  if (length(a) == 1 && is.na(a)) return(b)
  if (length(a) == 1 && identical(a, "")) return(b)
  a
}
coalesce_na <- function(...) {
  for (x in list(...)) if (!is.null(x) && !is.na(x)) return(x)
  NA
}

parse_json_text <- function(value) {
  if (is.null(value) || length(value) == 0 || is.na(value) || identical(value, "")) {
    return(character(0))
  }
  parsed <- tryCatch(jsonlite::fromJSON(value), error = function(e) value)
  parsed <- as.character(parsed)
  parsed[!is.na(parsed) & nzchar(parsed)]
}

format_json_values <- function(value) {
  values <- parse_json_text(value)
  if (length(values) == 0) return("—")
  paste(unique(values), collapse = ", ")
}
