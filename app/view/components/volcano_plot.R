# view/components/volcano_plot.R
# ─────────────────────────────────────────────────────────────────────────────
# Interactive volcano plot using plotly.
# Data is fetched by the parent (data_explore) — this component only renders.
#
# Design based on prototype: colour by significance direction,
# threshold lines update reactively with sidebar sliders,
# hover shows gene details, click highlights row in results_table.
#
# Original plotly-first renderer reference:
# p <- plotly::plot_ly(
#   df,
#   x = ~log2fc, y = ~neg_log10p, type = "scatter", mode = "markers",
#   color = ~sig, colors = c(Up = COLS$up, Down = COLS$down, NS = COLS$ns),
#   text = ~hover_text, hoverinfo = "text", source = "volcano",
#   marker = list(size = 6, opacity = 0.7, line = list(width = 0))
# )
# ─────────────────────────────────────────────────────────────────────────────

box::use(
  shiny[NS, moduleServer, need, reactive, observe, req, renderUI, uiOutput,
        div, p, tags, tagList, bindCache, validate],
  shinycssloaders[withSpinner],
  plotly[plotlyOutput, renderPlotly, plot_ly, layout, add_annotations,
         add_segments, event_data, event_register],
  dplyr[mutate, case_when, filter, arrange, desc],
)

# UK DRI brand-adjacent palette
COLS <- list(
  up      = "#C0392B",   # red
  down    = "#2980B9",   # blue
  ns      = "#BDC3C7",   # grey
  thresh  = "#7F8C8D",   # dashed line colour
  bg      = "#FAFAFA"
)

# ── UI ────────────────────────────────────────────────────────────────────────
#' @export
volcano_ui <- function(id) {
  ns <- NS(id)
  tagList(
    plotlyOutput(ns("plot"), height = "520px") |> withSpinner(
      type = 1, caption = "Loading plot...", color = "#5b5b5b"),
    uiOutput(ns("click_info")),
    p("Volcano plot showing differentially expressed genes between selected conditions.")
  )
}

# ── Server ────────────────────────────────────────────────────────────────────
#' @param de_data       reactive data.frame from fetch_de_for_gene() — all rows
#'                      (NOT pre-filtered); filtering applied here so threshold
#'                      lines always show even when no genes pass.
#' @param padj_thresh   reactive numeric — from sidebar slider
#' @param lfc_thresh    reactive numeric — from sidebar slider
#' @param gene          reactive character — highlighted gene name
#' @export
volcano_server <- function(id, de_data, padj_thresh, lfc_thresh, gene = reactive(NULL)) {
  moduleServer(id, function(input, output, session) {

    plot_df <- reactive({
      req(de_data(), nrow(de_data()) > 0)
      df <- de_data()

      # Require at minimum log2fc plus one significance column
      req("log2fc" %in% names(df))
      req(("padj" %in% names(df) && any(!is.na(df$padj))) ||
            ("pvalue" %in% names(df) && any(!is.na(df$pvalue))))

      df |>
        dplyr::mutate(
          significance_value = ifelse(!is.na(padj), padj, pvalue),
          neg_log10p = -log10(pmax(significance_value, 1e-300)),
          sig = dplyr::case_when(
            !is.na(padj) & padj < padj_thresh() & log2fc >  lfc_thresh() ~ "Up",
            !is.na(padj) & padj < padj_thresh() & log2fc < -lfc_thresh() ~ "Down",
            TRUE ~ "NS"
          ),
          # # Hover text
          # hover_text = paste0(
          #   "<b>", gene_symbol, "</b><br>",
          #   "log2FC: ",  round(log2fc,  3), "<br>",
          #   "padj: ",    signif(padj,  3), "<br>",
          #   "pvalue: ",  signif(pvalue, 3), "<br>",
          #   if ("cell_type" %in% names(df)) paste0("Cell type: ", cell_type, "<br>") else "",
          #   if ("condition_a" %in% names(df)) paste0(condition_a, " vs ", condition_b) else ""
          # )
        )
    }) 
    # |>
    #   bindCache(
    #     paste(de_data()$lab_source[1] %||% "", collapse = ","),
    #     paste(de_data()$study_id[1] %||% "", collapse = ","),
    #     # padj_thresh(),
    #     # lfc_thresh(),
    #     # gene()  #since annotations added in plot_obj
    #   ) # Causes Error: object '' not found due to computing drug panel cache when datas not ready

    plot_obj <- reactive({
      df   <- plot_df()
      pt   <- padj_thresh()
      lfc  <- lfc_thresh()
      finite_y <- df$neg_log10p[is.finite(df$neg_log10p)]
      finite_x <- df$log2fc[is.finite(df$log2fc)]
      ymax <- if (length(finite_y) > 0) max(finite_y, na.rm = TRUE) * 1.05 else 1
      xpad <- if (length(finite_x) > 0) max(abs(finite_x), na.rm = TRUE) * 1.1 else 1
      # ymax <- max(df$neg_log10p, na.rm=TRUE) * 1.05
      # xpad <- max(abs(df$log2fc), na.rm=TRUE) * 1.1


      p <- plotly::plot_ly(
          df,
          x          = ~log2fc,
          y          = ~neg_log10p,
          type       = "scattergl",
          mode       = "markers",
          color      = ~sig,
          colors     = c(Up = COLS$up, Down = COLS$down, NS = COLS$ns),
          # text       = ~hover_text,
          # hoverinfo  = "text",
          customdata = ~gene_symbol,
          hovertemplate = paste0(
            "<b>%{customdata}</b><br>",
            "log2FC: %{x:.3f}<br>",
            "-log10(p): %{y:.3f}<extra></extra>"
          ),
          source     = "volcano",
          marker     = list(size = 6, opacity = 0.7, line = list(width = 0))
        ) |>
        # Threshold lines as layout shapes, NOT add_segments() scatter traces as this is likely main
        # reason for slow rendering as add_segments() defaults to inherit = TRUE, so each line
        # inherited the full 10,000s-row data, color = ~sig, customdata and marker from plot_ly()
        # (∴ = ~9 SVG scatter traces being converted back to SVG) and ∴ bloating JSON payload,
        # blowing the DOM.
        # Shapes carry no data, traces, or legend entries
        plotly::layout(
          shapes = list(
            # Horizontal padj threshold line
            list(type = "line", x0 = -xpad, x1 = xpad, y0 = -log10(pt), y1 = -log10(pt),
                 line = list(dash = "dot", color = COLS$thresh, width = 1)),
            # Vertical lfc threshold lines (coincide at 0 when lfc_thresh = 0)
            list(type = "line", x0 =  lfc, x1 =  lfc, y0 = 0, y1 = ymax,
                 line = list(dash = "dot", color = COLS$thresh, width = 1)),
            list(type = "line", x0 = -lfc, x1 = -lfc, y0 = 0, y1 = ymax,
                 line = list(dash = "dot", color = COLS$thresh, width = 1))
          ),
          paper_bgcolor = COLS$bg, plot_bgcolor = COLS$bg,
          xaxis = list(
            title       = "log₂ Fold Change",
            zeroline    = TRUE, zerolinecolor = "#ECF0F1", zerolinewidth = 1,
            gridcolor   = "#ECF0F1"
          ),
          yaxis = list(
            title     = "-log₁₀(padj / p-value)",
            gridcolor = "#ECF0F1"
          ),
          legend = list(title = list(text = "Direction"), orientation = "h",
                        y = -0.12),
          margin = list(t = 30)
        )

      # Annotate searched gene if present
      # g <- gene()
      # if (!is.null(g) && "gene_symbol" %in% names(df)) {
      #   match_idx <- which(toupper(df$gene_symbol) == toupper(g))[1]
      # } else {
      #   match_idx <- NA_integer_
      # }
      # if (!is.na(match_idx)) {
      #   gp <- df[match_idx, , drop = FALSE][1, ]
      #   p <- p |> plotly::add_annotations(
      #     x    = gp$log2fc, y = gp$neg_log10p,
      #     text = paste0("<b>", gp$gene_symbol, "</b>"),
      #     showarrow = TRUE, arrowhead = 2, arrowsize = 0.8,
      #     font = list(size = 12, color = "#2C3E50")
      #   )
      # }
      g_terms <- gene()
      if (length(g_terms) > 0 && "gene_symbol" %in% names(df)) {
        for (g in g_terms) {
          match_idxs <- which(toupper(df$gene_symbol) == toupper(g))
          for (idx in match_idxs) {          # one annotation per row (each cell_type etc.)
            gp <- df[idx, , drop = FALSE]
            p  <- p |> plotly::add_annotations(
              x = gp$log2fc, y = gp$neg_log10p,
              text = paste0("<b>", gp$gene_symbol, "</b>"),
              showarrow = TRUE, arrowhead = 2, arrowsize = 0.8,
              font = list(size = 12, color = "#2C3E50")
            )
          }
        }
      }
      plotly::event_register(p, "plotly_click")
      p
    }) |>
      bindCache(
        paste(de_data()$lab_source[1] %||% "", collapse = ","),
        paste(de_data()$study_id[1] %||% "", collapse = ","),
        # padj_thresh(),
        # lfc_thresh(),
        paste(gene() %||% character(0), collapse = ",")
      )

    output$plot <- renderPlotly({
      plot_obj()
    })

    # ── Click-to-inspect ──────────────────────────────────────────────────────
    output$click_info <- renderUI({
      click <- plotly::event_data("plotly_click", source = "volcano")
      req(click)
      df    <- plot_df()
      idx   <- which(
        round(df$log2fc, 5) == round(click$x, 5) &
        round(df$neg_log10p, 5) == round(click$y, 5)
      )
      req(length(idx) > 0)
      row <- df[idx[1], ]

      div(class="alert alert-info", style="font-size:12px; margin-top:6px; padding:6px",
        tags$b(row$gene_symbol), " | ",
        "log2FC: ", round(row$log2fc, 3), " | ",
        "padj: ", signif(row$padj, 3),
        if (!is.na(row$cell_type)) paste0(" | Cell type: ", row$cell_type) else ""
      )
    })

  })
}
