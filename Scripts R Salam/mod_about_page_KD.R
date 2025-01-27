# ─────────────────────────────────────────────────────────────────────────────
# mod_about_page.R
# Page "About"
# ─────────────────────────────────────────────────────────────────────────────

mod_about_page_ui <- function(id) {
  ns <- NS(id)
  tagList(
    # Vous pouvez aussi utiliser fluidPage ou fluidRow si vous préférez
    fluidRow(
      column(
        width = 12,
        tags$h2("À propos"),
        tags$p("Ici, vous pouvez décrire l'objectif général de l'application, ",
               "la méthodologie d'ensemble, vos sources de données, etc."),
        tags$hr(),
        tags$p("Lorem ipsum dolor sit amet, consectetur adipiscing elit. ",
               "Sed nec risus quis libero ornare semper. Praesent vehicula ",
               "justo non faucibus sollicitudin. Pellentesque vehicula augue ",
               "a sollicitudin sodales. Proin blandit sit amet erat non ",
               "feugiat... (texte d'exemple à remplacer).")
      )
    )
  )
}

mod_about_page_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    # A priori, rien de particulier dans le serveur pour une page statique
  })
}
