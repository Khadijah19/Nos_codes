# ─────────────────────────────────────────────────────────────────────────────
# mod_about_page.R
# Page "About"
# ─────────────────────────────────────────────────────────────────────────────

# mod_about_page.R
mod_about_page_ui <- function(id) {
  ns <- NS(id)
  fluidPage(
    fluidRow(
      column(
        12,
        tags$h2("À propos"),
        tags$p("Ici, vous pouvez décrire l'objectif général de l'application, ",
               "la méthodologie globale, vos sources de données, etc."),
        tags$hr(),
        tags$p("Lorem ipsum dolor sit amet, consectetur adipiscing elit. ",
               "Sed nec risus quis libero ornare semper. Praesent vehicula ",
               "justo non faucibus sollicitudin.")
      )
    )
  )
}
mod_about_page_server <- function(id) {
  moduleServer(id, function(input, output, session) { })
}
