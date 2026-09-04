# helpers/help_tips.R
#
# Reusable hover instructions/info boxes using the circle-info + tooltip pattern
# this module builds the icon, the hover behaviour, and the body styling.

box::use(
  shiny[icon, tags],
  bslib[tooltip],
)

#' Icon + hover tooltip.
#'
#' @param tooltip_text    Tooltip body. A plain string, or tags for structured/multi-line text
#'                        (may wrap those in tip_text() for consistent styling).
#' @param placement       Side the bubble opens on: "right" (default), "top", "bottom", "left".
#'                        Use "top"/"bottom" inside narrow containers where "right" would clip.
#' @param label           Screen-reader label for the icon.
#' @param icon_name       Font Awesome name. Defaults to the app's circle-info convention.
#' @param colour          Icon appearance.
#' @param size            Icon appearance.
#' @param stop_propagation TRUE (default) swallows the click, so an icon nested inside a
#'                  clickable container — an accordion header, card header, nav item —
#'                  explains itself without also toggling that container.
#' @export
tool_tip <- function(tooltip_text,
                     placement = "right",
                     label = "More information",
                     icon_name = "circle-info",
                     colour = "#667eea",
                     size = "12px",
                     stop_propagation = TRUE) {
  tooltip(
    tags$span(
      icon(icon_name),
      style = sprintf(
        "display:inline-flex; align-items:left; color:%s; cursor:help; margin-left:6px; font-size:%s;",
        colour, size
      ),
      onclick = if (isTRUE(stop_propagation)) "event.stopPropagation();", # ?
      tabindex = "0",        # keyboard reachable - bslib's tooltip also opens on focus
      role = "button",
      `aria-label` = label
    ),
    tooltip_text,
    placement = placement
  )
}

#' Standard tooltip body wrapper.
#' @param ... Tags or strings making up the body.
#' @export
tip_text <- function(...) {
  tags$div(style = "text-align:left; font-size:12px; line-height:1.45;", ...)
}

#' A bold field label with a help tooltip beside it.
#' Convenience for the repeated `tags$label(text, tool_tip(...))` pattern.
#' @param label_text    Label text (e.g. for div).
#' @param tooltip_text  Tooltip body, passed to tool_tip().
#' @param style         Label style. Defaults to the sidebar's field-label styling.
#' @param ...           Passed through to tool_tip() (eg placement, icon_name).
#' @export
label_and_tooltip <- function(label_text, tooltip_text,
                       style = "font-weight: 600; color: #333; font-size: 13px;",
                       ...) {
  tags$label(label_text, tool_tip(tooltip_text, ...), style = style)
}
