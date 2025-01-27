# ─────────────────────────────────────────────────────────────────────────────
# mod_notes_techniques_page_KD.R
# Page "Notes Techniques"
# ─────────────────────────────────────────────────────────────────────────────

# mod_notes_techniques_page_KD.R
mod_notes_techniques_page_ui <- function(id) {
  ns <- NS(id)
  fluidPage(
    fluidRow(
      column(
        12,
        tags$h2("Notes Techniques"),
        tags$p("Cette page permet d'exposer des précisions techniques (harmonisation, etc.)."),
        tags$hr(),
        # Exemple
        tags$div(style="background:#f0f0f0; padding:10px;",
                 tags$h4("Note Technique 1"),
                 tags$p("Harmonisation des frontières administratives."),
                 tags$a("En savoir plus", href="#")
        )
      )
    )
  )
}
mod_notes_techniques_page_server <- function(id) {
  moduleServer(id, function(input, output, session) { })
}
