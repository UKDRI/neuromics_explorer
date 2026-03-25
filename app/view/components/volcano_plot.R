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
  shiny[NS, moduleServer, reactive, observe, req, renderUI, uiOutput,
        div, p, tags, tagList],
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
    plotlyOutput(ns("plot"), height = "520px"),
    uiOutput(ns("click_info"))
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

      # Require at minimum log2fc and pvalue columns
      req("log2fc" %in% names(df), "pvalue" %in% names(df))

      df |>
        dplyr::mutate(
          neg_log10p = -log10(pmax(pvalue, 1e-300)),
          sig = dplyr::case_when(
            !is.na(padj) & padj < padj_thresh() & log2fc >  lfc_thresh() ~ "Up",
            !is.na(padj) & padj < padj_thresh() & log2fc < -lfc_thresh() ~ "Down",
            TRUE ~ "NS"
          ),
          # Hover text
          hover_text = paste0(
            "<b>", gene_symbol, "</b><br>",
            "log2FC: ",  round(log2fc,  3), "<br>",
            "padj: ",    signif(padj,  3), "<br>",
            if ("cell_type" %in% names(df)) paste0("Cell type: ", cell_type, "<br>") else "",
            if ("condition_a" %in% names(df)) paste0(condition_a, " vs ", condition_b) else ""
          )
        )
    })

    output$plot <- renderPlotly({
      df   <- plot_df()
      pt   <- padj_thresh()
      lfc  <- lfc_thresh()
      ymax <- max(df$neg_log10p, na.rm=TRUE) * 1.05
      xpad <- max(abs(df$log2fc), na.rm=TRUE) * 1.1

      p <- plotly::plot_ly(
          df,
          x          = ~log2fc,
          y          = ~neg_log10p,
          type       = "scatter",
          mode       = "markers",
          color      = ~sig,
          colors     = c(Up = COLS$up, Down = COLS$down, NS = COLS$ns),
          text       = ~hover_text,
          hoverinfo  = "text",
          source     = "volcano",
          marker     = list(size = 6, opacity = 0.7, line = list(width = 0))
        ) |>
        # Horizontal padj threshold line
        plotly::add_segments(
          x = -xpad, xend = xpad,
          y = -log10(pt), yend = -log10(pt),
          line       = list(dash = "dot", color = COLS$thresh, width = 1),
          showlegend = FALSE, hoverinfo = "skip"
        ) |>
        # Vertical lfc threshold lines (only when lfc_thresh > 0)
        plotly::add_segments(
          x = lfc, xend = lfc, y = 0, yend = ymax,
          line       = list(dash = "dot", color = COLS$thresh, width = 1),
          showlegend = FALSE, hoverinfo = "skip"
        ) |>
        plotly::add_segments(
          x = -lfc, xend = -lfc, y = 0, yend = ymax,
          line       = list(dash = "dot", color = COLS$thresh, width = 1),
          showlegend = FALSE, hoverinfo = "skip"
        ) |>
        plotly::layout(
          paper_bgcolor = COLS$bg, plot_bgcolor = COLS$bg,
          xaxis = list(
            title       = "log₂ Fold Change",
            zeroline    = TRUE, zerolinecolor = "#ECF0F1", zerolinewidth = 1,
            gridcolor   = "#ECF0F1"
          ),
          yaxis = list(
            title     = "-log₁₀(p-value)",
            gridcolor = "#ECF0F1"
          ),
          legend = list(title = list(text = "Direction"), orientation = "h",
                        y = -0.12),
          margin = list(t = 30)
        )

      # Annotate searched gene if present
      g <- gene()
      if (!is.null(g) && g %in% df$gene_symbol) {
        gp <- df[df$gene_symbol == g, ][1, ]
        p <- p |> plotly::add_annotations(
          x    = gp$log2fc, y = gp$neg_log10p,
          text = paste0("<b>", g, "</b>"),
          showarrow = TRUE, arrowhead = 2, arrowsize = 0.8,
          font = list(size = 12, color = "#2C3E50")
        )
      }
      plotly::event_register(p, "plotly_click")
      p
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
