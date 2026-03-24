# Overview table of all registered datasets on explore page
# TODO: checks

box::use(
  shiny[NS, moduleServer, reactive, req, tagList, uiOutput, renderUI],
  DT[DTOutput, renderDT, datatable, formatRound],
  dplyr[mutate, select],
  jsonlite[fromJSON],
  app/logic/query_data/metadata[get_all_datasets],
)

#' @export
dataset_table_ui <- function(id) {
  ns <- NS(id)
  tagList(DTOutput(ns("tbl")))
}

#' @param on_select          function(row) called when a dataset row is clicked
#' @export
dataset_table_server <- function(id, on_select = NULL) {
  moduleServer(id, function(input, output, session) {

    all_data <- reactive({
      get_all_datasets()
    })

    output$tbl <- renderDT({
      df <- all_data()
      req(nrow(df) > 0)

      display <- df |>
        dplyr::select(
          Lab           = lab_source,
          Dataset       = dataset_name,
          Omic          = omic_type,
          Organism      = organism,
          Features      = total_features,
          `Sig (p<0.05)`= n_sig_features,
          `Samples`     = total_samples,
          `Cells`       = total_cells,
          `Conditions`  = n_conditions,
          `Cell types`  = n_cell_types
        )

      datatable(display,
        selection  = "single",
        rownames   = FALSE,
        class      = "table-hover table-sm",
        options    = list(
          pageLength   = 15,
          scrollX      = TRUE,
          dom          = "frtip",
          columnDefs   = list(
            list(className = "dt-right", targets = 4:9)
          )
        )
      )
    })

    # Fire callback when row selected
    if (!is.null(on_select)) {
      shiny::observeEvent(input$tbl_rows_selected, {
        req(input$tbl_rows_selected)
        row <- all_data()[input$tbl_rows_selected, ]
        on_select(list(
          lab          = row$lab_source,
          study_id     = row$study_id,
          dataset_name = row$dataset_name,
          omic_type    = row$omic_type
        ))
      })
    }
  })
}
