# Slim, dismissible notice bar pinned to the bottom of the viewport/page.
#
# Opt-out model: it states what is collected by default, lets a visitor opt out
# in one click, and points at the Privacy & cookies page for granular control.
#
# Currently, styling is inline rather than in custom.css, and visibility uses the `hidden`
# attribute for the reason already noted at the foot of custom.css. Matches dev-release
# banner in main.R.
# TODO: fix css styling to remove inline styling
#
# Behaviour — expands, opting out, 'x' dismissal, and reparenting to <body> for fixed
# positioning in static/nex_consent.js.

box::use(
  bslib[nav_item],
  shiny[div, p, span, tags],
  htmltools[HTML],
)

# Styled similar to the dev-release banner
BAR_STYLE <- paste(
  "position: fixed; left: 0; right: 0; bottom: 0; z-index: 99999;",
  "background: #f5f5f5; border-top: 2px solid #e0e0e0;",
  "box-shadow: 0 -2px 10px rgba(0,0,0,0.06);"
)

INNER_STYLE <- paste(
  "display: flex; align-items: flex-end; gap: 14px;",
  "max-width: 1180px; margin: 0 auto; padding: 8px 18px;"
)

TEXT_STYLE <- paste(
  "margin: 0; flex: 1 1 auto; font-size: 11px; line-height: 1.6;",
  "color: #888; letter-spacing: 0.03em;"
)

LINK_STYLE <- paste(
  "border: 0; background: transparent; padding: 0; font: inherit;",
  "cursor: pointer; color: #0a7aff; font-weight: 600; text-decoration: underline;"
)

# Sits at the far right of the text block, bottom-aligned, so it lands at the
# end of the final line of copy rather than floating beside the first.
MORE_STYLE <- paste(
  "border: 0; background: transparent; padding: 0 0 0 14px; font: inherit;",
  "cursor: pointer; color: #0a7aff; font-weight: 600; font-size: 11px;",
  "letter-spacing: 0.03em; white-space: nowrap; flex: 0 0 auto;"
)

BTN_STYLE <- "font-size: 11px; padding: 3px 10px; white-space: nowrap;"

CLOSE_STYLE <- paste(
  "border: 0; background: transparent; color: #aaa; font-size: 18px;",
  "line-height: 1; padding: 0 2px; cursor: pointer; flex: 0 0 auto;"
)

NAVLINK_STYLE <- paste(
  "border: 0; background: transparent; padding: 6px 4px; font-size: 11px;",
  "color: #8d99a8; letter-spacing: 0.03em; cursor: pointer;",
  "text-decoration: underline;"
)

#' Always-visible route back to the privacy page.
#'
#' Rendered as a navbar item rather than a page footer. An in-flow footer would
#' be unreachable here: page_navbar() is fillable, which pins body height to the
#' viewport, so anything after the panel content sits below the fold with nothing
#' to scroll — the same truncation that hid the foot of the privacy page. The
#' navbar is the one strip guaranteed visible on every route.
#'
#' @export
privacy_nav_link_ui <- function() {
  nav_item(
    tags$button(
      id = "nex-privacy-navlink",
      style = NAVLINK_STYLE,
      type = "button",
      "Privacy & cookies"
    )
  )
}

#' @export
notice_bar_ui <- function() {
  div(
    id = "nex-notice",
    style = BAR_STYLE,
    hidden = NA,
    role = "region",
    `aria-label` = "How we measure use of this platform",

    div(
      style = INNER_STYLE,

      p(
        style = TEXT_STYLE,
        "We measure how this platform is used to help us keep improving.",

        # Inline span so the expanded copy flows into the same paragraph rather
        # than opening a second block.
        span(
          id = "nex-notice-rest",
          hidden = NA,
          " Neuromics Explorer keeps a randomised ID (no name, no email, no tracking ",
          "across other sites) so we can see which datasets matter, where queries fail, ",
          "and whether the tools are actually helping UK DRI scientists. Accepting also ",
          "enables UK DRI's Google Analytics. Choosing Opt out at any time means we keep ",
          "nothing beyond the current browser session."
        ),

        " Please see our ",
        tags$button(
          id = "nex-notice-privacy",
          style = LINK_STYLE,
          type = "button",
          "privacy notice"
        ),
        " for further details."
      ),

      tags$button(
        id = "nex-notice-more",
        style = MORE_STYLE,
        type = "button",
        `aria-expanded` = "false",
        `aria-controls` = "nex-notice-rest",
        "More"
      ),

      tags$button(
        id = "nex-notice-optout",
        class = "btn btn-outline-secondary btn-sm",
        style = BTN_STYLE,
        type = "button",
        "Opt out"
      ),

      tags$button(
        id = "nex-notice-gotit",
        class = "btn btn-primary btn-sm",
        style = BTN_STYLE,
        type = "button",
        "Got it"
      ),

      tags$button(
        id = "nex-notice-close",
        style = CLOSE_STYLE,
        type = "button",
        `aria-label` = "Hide this notice",
        title = "Hide this notice",
        HTML("&times;")
      )
    )
  )
}
