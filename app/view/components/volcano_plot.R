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
  app/view/components/helpers/de_helpers[pick_label_rows, label_cap_notes],
  app/view/components/helpers/help_tips[tool_tip, tip_text],
)

# Volcano payloads guarantees searched genes are present, so a gene with many occurrences
# (dion has 34 rows for MPND, webber 3,400) would draw one label per row and bury the plot.
LABEL_CAP <- 3L

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
    # Kept OUTSIDE the plotly object so it cannot overlap points, survives zoom/pan, and stays out of PNG exports.
    uiOutput(ns("label_note")),
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

    # Shared by plot_obj and output$label_note. A separate reactive rather than a reactiveVal
    # written from inside plot_obj: plot_obj is bindCache'd, so on a cache hit its body never runs
    # and the caption would silently go stale.
    picked_labels <- reactive({
      df <- plot_df()
      g_terms <- gene()
      if (length(g_terms) == 0 || !"gene_symbol" %in% names(df)) {
        return(pick_label_rows(character(0), character(0), numeric(0), cap = LABEL_CAP))
      }
      pick_label_rows(df$gene_symbol, g_terms, df$neg_log10p, cap = LABEL_CAP)
    })

    output$label_note <- renderUI({
      notes <- label_cap_notes(picked_labels()$summary)
      if (is.null(notes)) return(NULL)   # nothing omitted, do nothing
      tags$div(
        style = "font-size:12px; color:#666; margin:2px 0 0 4px; display:flex; align-items:center; gap:2px;",
        tags$span(sprintf("Labels: max %d per gene \u00b7 %s", LABEL_CAP,
                          paste(utils::head(notes, 2), collapse = " \u00b7 "))),
        if (length(notes) > 2) tags$span(sprintf("\u00b7 +%d more", length(notes) - 2)),
        tool_tip(
          tip_text(
            tags$div(style = "font-weight:600; margin-bottom:4px;", "Gene labels"),
            tags$div(sprintf(paste("At most %d labels are drawn per gene, chosen by significance",
                                   "(largest -log10 p). Every matching point is still plotted and",
                                   "hoverable - only the labels are limited."), LABEL_CAP)),
            tags$ul(style = "padding-left:16px; margin:6px 0 0;",
                    lapply(notes, function(n) tags$li(n)))
          ),
          placement = "top"
        )
      )
    })

    # TRACE-REMOVE: why the Plot-tab volcano draws no gene labels while Compare does.
    # Delete every line tagged TRACE-REMOVE once confirmed.
    .vtrace <- function(...) {
      message(sprintf("[VOLC %s] %s", format(Sys.time(), "%H:%M:%OS3"),
                      paste0(as.character(list(...)), collapse = "")))
    }

    plot_obj <- reactive({
      .vtrace("plot_obj BODY RUN (a cache HIT would print nothing at all)")   # TRACE-REMOVE
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
      picked <- picked_labels()
      # TRACE-REMOVE
      .vtrace("  gene() n=", length(g_terms),
              " values=", paste(g_terms, collapse = "/"),
              " | df rows=", nrow(df),
              " has gene_symbol=", "gene_symbol" %in% names(df),
              " | study_id=", paste(unique(df$study_id)[1], collapse = ""),
              " lab_source_in_df=", "lab_source" %in% names(df))
      # TRACE-REMOVE: fingerprint the payload. Expected sum(log2fc) per dataset (20000-row
      # payload): bowles 137.3959 | dion 8587.283 | ruepp 212.7862 (9746 rows) | webber 16706.8684
      .vtrace("  PAYLOAD FINGERPRINT rows=", nrow(df),
              " sum(log2fc)=", round(sum(df$log2fc, na.rm = TRUE), 4),
              " first genes=", paste(utils::head(df$gene_symbol, 3), collapse = ","))
      .vtrace("  picked ", nrow(picked$summary), " term(s) -> ", length(picked$idx),
              " label(s); matched total=", sum(picked$summary$matched))   # TRACE-REMOVE
      # ONE add_annotations call over the capped rows, not one per row otherwise each call rebuilds the
      # whole plotly object.
      if (length(picked$idx) > 0) {
        lab <- df[picked$idx, , drop = FALSE]
        p <- p |> plotly::add_annotations(
          x = lab$log2fc, y = lab$neg_log10p,
          text = paste0("<b>", lab$gene_symbol, "</b>"),
          showarrow = TRUE, arrowhead = 2, arrowsize = 0.8,
          font = list(size = 12, color = "#2C3E50")
        )
      }
      .vtrace("  annotations on returned object=",
              length(plotly::plotly_build(p)$x$layout$annotations %||% list()))   # TRACE-REMOVE
      plotly::event_register(p, "plotly_click")
      p
    }) |>
      bindCache(
        paste(de_data()$lab_source[1] %||% "", collapse = ","),
        paste(de_data()$study_id[1] %||% "", collapse = ","),
        # Key on the data itself rather than lab/study only otherwise the volcano plots are identical under
        # the drug panel for every drug. Allows additional filtering (eg cell_type, timepoint, condition etc)
        nrow(de_data()),
        sum(de_data()$log2fc, na.rm = TRUE),
        padj_thresh(),
        lfc_thresh(),
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
