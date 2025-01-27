# ─────────────────────────────────────────────────────────────────────────────
# mod_notes_techniques_page_KD.R
# Page "Notes Techniques"
# ─────────────────────────────────────────────────────────────────────────────

mod_notes_techniques_page_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(
        width = 12,
        tags$h2("Notes Techniques"),
        tags$p("Cette page permet d'exposer des précisions techniques : ",
               "harmonisation des frontières administratives, détails sur ",
               "les méthodes statistiques, sur l'agrégation spatiale, etc."),
        tags$hr(),
        tags$div(
          style = "padding:10px; background-color:#f0f0f0; border-radius:5px;",
          tags$h4("Note Technique 1"),
          tags$p("Description de la méthodologie pour la harmonisation des limites administratives..."),
          tags$a("En savoir plus", href="#", style="color:blue;")
        ),
        tags$br(),
        tags$div(
          style = "padding:10px; background-color:#f0f0f0; border-radius:5px;",
          tags$h4("Note Technique 2"),
          tags$p("Méthode d'évaluation de l'évolution temporelle d'un indicateur (probabilités d'exceedance, etc.)."),
          tags$a("En savoir plus", href="#", style="color:blue;")
        ),
        tags$br(),
        tags$div(
          style = "padding:10px; background-color:#f0f0f0; border-radius:5px;",
          tags$h4("Note Technique 3"),
          tags$p("Détails sur l'utilisation des données IHME..."),
          tags$a("En savoir plus", href="#", style="color:blue;")
        ),
        tags$br(),
        tags$div(
          style = "padding:10px; background-color:#f0f0f0; border-radius:5px;",
          tags$h4("Note Technique 4"),
          tags$p("Utilisation des données WHO-ESPEN..."),
          tags$a("En savoir plus", href="#", style="color:blue;")
        )
      )
    )
  )
}

mod_notes_techniques_page_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    # Page statique : pas de logique côté serveur
  })
}
