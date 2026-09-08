# Privacy & cookies page (Navbar: Resources > Privacy & cookies).
#
# Opt-out model: both non-essential categories default to 'on', and this page is the
# single place a visitor can turn them off, see exactly what is held, and get the identifier
# they would need to quote for a data erasure request.
#
# Markup only. The controls are driven entirely by static/nex_consent.js rather
# than by Shiny inputs: that keeps them working before the websocket connects,
# avoids a server round-trip for a purely client-side preference, and means the
# switch state is read from the same source of truth the rest of the app uses.

box::use(
  shiny[div, h2, h3, h4, p, tags, span, strong],
  htmltools[HTML],
)

CONTACT_EMAIL <- "ukdri-informatics@ucl.ac.uk"
LAST_UPDATED <- "September 2026"

#' Build one row of the "What we store" table.
#'
#' @noRd
store_row <- function(what, why, where, kept) {
  tags$tr(
    tags$td(HTML(what)),
    tags$td(HTML(why)),
    tags$td(HTML(where)),
    tags$td(HTML(kept))
  )
}

#' Build one toggle row in the "Your choices" panel.
#'
#' @param input_id Namespaced id for the checkbox.
#' @param locked   TRUE for the essential row, which cannot be switched off.
#'
#' @noRd
choice_row <- function(input_id, title, detail, locked = FALSE) {
  toggle <- if (locked) {
    span(class = "nex-choice-locked", "Always on")
  } else {
    div(
      class = "form-check form-switch nex-choice-switch",
      tags$input(
        class = "form-check-input",
        type = "checkbox",
        role = "switch",
        id = input_id,
        checked = NA,
        `aria-describedby` = paste0(input_id, "_detail")
      ),
      tags$label(class = "form-check-label visually-hidden", `for` = input_id, title)
    )
  }

  div(
    class = "nex-choice-row",
    div(
      class = "nex-choice-copy",
      h4(class = "nex-choice-title", title),
      p(id = paste0(input_id, "_detail"), class = "nex-choice-detail", HTML(detail))
    ),
    div(class = "nex-choice-control", toggle)
  )
}

#' @export
privacy_ui <- function() {
  div(
    class = "container nex-privacy",

    # page_navbar() defaults to fillable = TRUE, which clamps every nav panel to
    # the viewport height, override to make scrollable here
    style = "height: 100%; overflow-y: auto; padding-bottom: 72px;",

    div(
      class = "nex-privacy-head",
      h2("Privacy & cookies"),
      p(
        class = "nex-privacy-lede",
        "Neuromics Explorer does not currently require personal details to be used. ",
        "We store a small amount of anonymous usage data by default so we can see ",
        "which datasets and features are useful, and where the platform breaks. ",
        "You can switch that off below, and your choice is remembered in this browser."
      ),
      p(
        class = "nex-privacy-meta",
        "Last updated ", LAST_UPDATED, ". This notice will be updated as the platform and its data protection review progress."
      )
    ),

    # ── Your choices ────────────────────────────────────────────────────
    div(
      class = "nex-privacy-card",
      h3("Your choices"),
      p(
        class = "nex-privacy-note",
        "Changes take effect immediately and apply to this browser only. ",
        "If you clear your browser storage, the defaults will return."
      ),

      choice_row(
        input_id = "nex-privacy-essential",
        title = "Essential",
        detail = paste(
          "Keeps the application working and remembers the choices you make on this",
          "platform. Without these the site cannot function, so they cannot be turned off."
        ),
        locked = TRUE
      ),

      choice_row(
        input_id = "nex-privacy-metrics",
        title = "NEx usage metrics",
        detail = paste(
          "Which pages and data endpoints are used, how long requests take, and",
          "whether they succeed. Held by us, on UK DRI infrastructure, and never",
          "shared with anyone else. Turning this off stops the recording entirely."
        )
      ),

      choice_row(
        input_id = "nex-privacy-analytics",
        title = "Google Analytics",
        detail = paste(
          "Aggregate visitor numbers used for UK DRI communications reporting.",
          "Google acts as our processor to monitor site traffic and may process this data outside the UK.",
          "We do not use it for advertising, or remarketing etc."
        )
      ),

      div(
        class = "nex-privacy-actions",
        tags$button(
          id = "nex-privacy-reset",
          class = "btn btn-outline-secondary btn-sm",
          type = "button",
          "Restore defaults"
        ),
        span(id = "nex-privacy-status", class = "nex-privacy-status", role = "status", "")
      )
    ),

    # ── What we store ───────────────────────────────────────────────────
    div(
      class = "nex-privacy-card",
      h3("What we store, and why"),
      div(
        class = "table-responsive",
        tags$table(
          class = "table nex-privacy-table",
          tags$thead(
            tags$tr(
              tags$th("What"),
              tags$th("Why"),
              tags$th("Where it is held"),
              tags$th("Kept for")
            )
          ),
          tags$tbody(
            store_row(
              "<strong>Session cookie</strong><br><code>nex_session_id</code>",
              "Groups the requests you make into a single visit so one visit is not counted many times.",
              "First-party cookie, set by our own server.",
              "Until you close your browser. Not set at all if you turn usage metrics off."
            ),
            store_row(
              "<strong>Your choice on this page</strong><br><code>nex_consent</code>, <code>nex_metrics</code>",
              "Remembers whether you opted out, so we can honour it on every page and on the server.",
              "Browser local storage, plus a first-party cookie so our server can see it.",
              "6 months, or until you clear your browser storage."
            ),
            store_row(
              "<strong>Random visitor ID</strong><br><code>nex_user_id</code>",
              "A random identifier that lets us tell a returning visit from a new one. It is not linked to your name, or email.",
              "Browser local storage, plus a first-party cookie.",
              "6 months. Removed immediately, and replaced by a value that lasts only for the current tab, if you turn usage metrics off."
            ),
            store_row(
              paste(
                "<strong>Usage metrics</strong><br>",
                "Date and time, the page or data endpoint requested, the response code,",
                "how long it took, your browser's user-agent string, the referring page",
                "address, and a yes/no flag for whether a search took place."
              ),
              paste(
                "Shows which datasets and features are actually used, and surfaces",
                "errors and any slow queries so we can fix them."
              ),
              paste(
                "<code>usage_metrics.sqlite3</code>, a separate database not shared with any third party."
              ),
              paste(
                "No automatic deletion is in place during this release.",
                "We aim to introduce a 13-month limit."
              )
            ),
            store_row(
              "<strong>Google Analytics</strong><br><code>_ga</code>, <code>_ga_&lt;id&gt;</code>",
              "Aggregate visitor and page-view counts for UK DRI communications reporting.",
              paste(
                "Google Analytics 4. Google is our processor and may process the data",
                "outside the UK. Advertising and personalisation signals are switched off."
              ),
              "Up to 2 years (Google's default), or until you clear your cookies."
            )
          )
        )
      ),

      h3(class = "nex-privacy-subhead", "What we do not store"),
      tags$ul(
        class = "nex-privacy-list",
        tags$li(
          strong("The genes and proteins you search for."),
          " We record only that a search happened, never the terms. One exception is worth ",
          "knowing: if you arrive by following a shared Neuromics Explorer link, the referring ",
          "address we log can contain the gene names in that link."
        ),
        tags$li(strong("Names, email addresses, passwords or accounts."), " There are none to store in this release."),
        tags$li(
          strong("Your IP address, in normal operation."),
          " The random visitor ID is used instead, precisely so that we do not need it."
        )
      )
    ),

    # ── User's identifier ─────────────────────────────────────────────────
    div(
      class = "nex-privacy-card",
      h3("Your identifier"),
      p(
        "We have no way of finding your records unless you tell us this value, ",
        "because it is the only thing that links them together. Quote it if you ask ",
        "us to delete your usage data."
      ),
      div(
        class = "nex-privacy-idbox",
        tags$code(id = "nex-privacy-id", class = "nex-privacy-id", "checking…"),
        tags$button(
          id = "nex-privacy-copy",
          class = "btn btn-outline-secondary btn-sm",
          type = "button",
          "Copy"
        )
      ),
      p(
        class = "nex-privacy-note",
        "Email ", tags$a(href = paste0("mailto:", CONTACT_EMAIL), CONTACT_EMAIL),
        " with this value and we will delete the matching rows. ",
        "A self-service delete button is planned for a future release."
      )
    ),

    # ── Rights and contact ──────────────────────────────────────────────
    div(
      class = "nex-privacy-card",
      h3("Your rights and who to contact"),
      tags$dl(
        class = "nex-privacy-dl",
        tags$dt("Who is responsible"),
        tags$dd(
          "Neuromics Explorer is built and run by the Core Informatics team at the ",
          "UK Dementia Research Institute (UK DRI), where UK DRI Ltd is the data controller. ",
        ),

        tags$dt("Our basis for holding usage data"),
        tags$dd(
          "Legitimate interests: understanding how a research platform is used so that ",
          "it can be maintained and improved. We have kept what we collect to the minimum ",
          "needed for that."
        ),

        tags$dt("What you can ask for"),
        tags$dd(
          "A copy of what we hold about you, correction of it, deletion of it, or that we ",
          "stop holding it at all. The switches above give you the last of these immediately; ",
          "for the others, email us with your identifier."
        ),

        tags$dt("How to reach us"),
        tags$dd(tags$a(href = paste0("mailto:", CONTACT_EMAIL), CONTACT_EMAIL)),

        tags$dt("If you are unhappy with our response"),
        tags$dd("You can speak with the UK DRI Data Protection Officer at "),
        tags$a(href = paste0("mailto:", "DPO@ukdri.ac.uk"), "DPO@ukdri.ac.uk"),
        tags$dd(
          "Or you can complain to the Information Commissioner's Office at ",
          tags$a(
            href = "https://ico.org.uk/make-a-complaint/",
            target = "_blank",
            rel = "noopener noreferrer",
            "ico.org.uk/make-a-complaint"
          ), "."
        )
      )
    )
  )
}
