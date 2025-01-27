# ─────────────────────────────────────────────────────────────────────────────
# mod_guide_page.R
# Page "Guide"
# ─────────────────────────────────────────────────────────────────────────────

# mod_guide_page.R
mod_guide_page_ui <- function(id) {
  ns <- NS(id)
  fluidPage(
    fluidRow(
      column(
        12,
        tags$h2("Guide d'utilisation"),
        tags$ul(
          tags$li("Étape 1 : Sélection du pays"),
          tags$li("Étape 2 : Sélection de l'indicateur"),
          tags$li("Étape 3 : Visualisation cartographique"),
          tags$li("Étape 4 : Analyse par région et téléchargements")
        )
      )
    )
  )
}
mod_guide_page_server <- function(id) {
  moduleServer(id, function(input, output, session) { })
}
