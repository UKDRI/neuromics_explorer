# These accessbibility functions derive from datanovia; https://www.datanovia.com/learn/tools/shiny-apps/best-practices/accessibility-performance.html

# Comprehensive accessible UI component library
create_accessible_ui_components <- function() {
  
  # Accessible input components with proper labeling
  accessible_text_input <- function(id, label, description = NULL, required = FALSE, ...) {
    # Generate unique IDs for proper association
    input_id <- id
    desc_id <- paste0(id, "_desc")
    error_id <- paste0(id, "_error")
    
    # Build ARIA attributes
    aria_attrs <- list(
      `aria-describedby` = if (!is.null(description)) desc_id else NULL,
      `aria-required` = if (required) "true" else NULL,
      `aria-invalid` = "false"  # Will be updated by validation
    )
    
    div(class = "form-group",
        # Proper label association
        tags$label(
          `for` = input_id,
          class = if (required) "required" else NULL,
          label,
          if (required) span(class = "required-indicator", "*", `aria-label` = "required")
        ),
        
        # Input with accessibility attributes
        textInput(
          inputId = input_id,
          label = NULL,  # Already provided above
          ...
        ) %>% tagAppendAttributes(!!!aria_attrs),
        
        # Description text
        if (!is.null(description)) {
          div(id = desc_id, class = "form-help-text", description)
        },
        
        # Error message container (hidden by default)
        div(id = error_id, class = "form-error-text", style = "display: none;",
            role = "alert", `aria-live` = "polite")
    )
  }
  
  # Accessible select input with proper grouping
  accessible_select_input <- function(id, label, choices, description = NULL, required = FALSE, ...) {
    input_id <- id
    desc_id <- paste0(id, "_desc")
    
    div(class = "form-group",
        tags$label(
          `for` = input_id,
          class = if (required) "required" else NULL,
          label,
          if (required) span(class = "required-indicator", "*", `aria-label` = "required")
        ),
        
        # Enhanced select with proper ARIA
        selectInput(
          inputId = input_id,
          label = NULL,
          choices = choices,
          ...
        ) %>% tagAppendAttributes(
          `aria-describedby` = if (!is.null(description)) desc_id else NULL,
          `aria-required` = if (required) "true" else NULL
        ),
        
        if (!is.null(description)) {
          div(id = desc_id, class = "form-help-text", description)
        }
    )
  }
  
  # Accessible data table with proper headers and navigation
  accessible_data_table <- function(data, table_id, caption = NULL, summary = NULL) {
    # Generate table with proper semantic structure
    table_html <- tags$table(
      id = table_id,
      class = "table table-striped table-hover",
      role = "table",
      `aria-label` = caption %||% "Data table",
      
      # Caption for screen readers
      if (!is.null(caption)) {
        tags$caption(caption)
      },
      
      # Table header with proper scope
      tags$thead(
        tags$tr(
          lapply(names(data), function(col_name) {
            tags$th(
              scope = "col",
              role = "columnheader",
              `aria-sort` = "none",  # Will be updated by sorting logic
              col_name
            )
          })
        )
      ),
      
      # Table body with row headers where appropriate
      tags$tbody(
        lapply(seq_len(min(nrow(data), 100)), function(row_idx) {  # Limit for performance
          row_data <- data[row_idx, ]
          tags$tr(
            # First column as row header if it's an identifier
            tags$th(scope = "row", role = "rowheader", row_data[[1]]),
            
            # Remaining columns as data cells
            lapply(2:ncol(row_data), function(col_idx) {
              tags$td(
                role = "gridcell",
                `aria-describedby` = paste0(table_id, "_col_", col_idx),
                as.character(row_data[[col_idx]])
              )
            })
          )
        })
      )
    )
    
    # Wrap table with navigation and summary
    div(class = "table-container",
        # Summary for screen readers
        if (!is.null(summary)) {
          div(class = "sr-only", summary)
        },
        
        # Table navigation instructions
        div(class = "table-instructions",
            "Use arrow keys to navigate table cells, Tab to move between interactive elements."
        ),
        
        # Scrollable table wrapper
        div(class = "table-responsive", 
            `aria-label` = "Scrollable data table",
            table_html
        ),
        
        # Table statistics
        div(class = "table-stats", `aria-live` = "polite",
            paste("Showing", min(nrow(data), 100), "of", nrow(data), "rows")
        )
    )
  }
  
  # Accessible plot with alternative text and data table
  accessible_plot_output <- function(id, alt_text, data_table = NULL) {
    plot_id <- id
    table_id <- paste0(id, "_table")
    desc_id <- paste0(id, "_desc")
    
    div(class = "accessible-plot-container",
        # Plot with proper labeling
        div(class = "plot-wrapper",
            plotOutput(
              outputId = plot_id,
              width = "100%",
              height = "400px"
            ) %>% tagAppendAttributes(
              role = "img",
              `aria-labelledby` = desc_id,
              `aria-describedby` = if (!is.null(data_table)) table_id else NULL
            ),
            
            # Alternative text description
            div(id = desc_id, class = "plot-description",
                h4("Chart Description"),
                p(alt_text)
            )
        ),
        
        # Alternative data table
        if (!is.null(data_table)) {
          div(class = "plot-alternative",
              h4("Data Table Alternative"),
              p("The following table contains the data represented in the chart above."),
              div(id = table_id,
                  DT::dataTableOutput(paste0(plot_id, "_datatable"))
              )
          )
        },
        
        # Download options
        div(class = "plot-actions",
            h4("Download Options"),
            downloadButton(paste0(plot_id, "_download"), "Download Chart as PNG",
                           class = "btn btn-secondary"),
            if (!is.null(data_table)) {
              downloadButton(paste0(plot_id, "_data_download"), "Download Data as CSV",
                             class = "btn btn-secondary")
            }
        )
    )
  }
  
  # Accessible navigation menu
  accessible_navigation <- function(menu_items, current_page = NULL) {
    nav_id <- "main_navigation"
    
    tags$nav(
      role = "navigation",
      `aria-label` = "Main navigation",
      id = nav_id,
      
      tags$ul(
        class = "nav nav-tabs",
        role = "tablist",
        
        lapply(names(menu_items), function(item_name) {
          item_config <- menu_items[[item_name]]
          is_current <- !is.null(current_page) && current_page == item_name
          
          tags$li(
            class = "nav-item",
            role = "presentation",
            
            tags$a(
              class = paste("nav-link", if (is_current) "active" else ""),
              href = item_config$href %||% paste0("#", gsub("\\s+", "_", tolower(item_name))),
              role = "tab",
              `aria-selected` = if (is_current) "true" else "false",
              `aria-current` = if (is_current) "page" else NULL,
              tabindex = if (is_current) "0" else "-1",
              
              item_name,
              
              # Add screen reader text for current page
              if (is_current) {
                span(class = "sr-only", " (current page)")
              }
            )
          )
        })
      )
    )
  }
  
  list(
    accessible_text_input = accessible_text_input,
    accessible_select_input = accessible_select_input,
    accessible_data_table = accessible_data_table,
    accessible_plot_output = accessible_plot_output,
    accessible_navigation = accessible_navigation
  )
}



