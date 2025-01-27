# ─────────────────────────────────────────────────────────────────────────────
# mod_guide_page.R
# Page "Guide"
# ─────────────────────────────────────────────────────────────────────────────

mod_guide_page_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(
        width = 12,
        tags$h2("Guide d'utilisation"),
        tags$p("Dans cette section, vous pouvez expliquer comment naviguer dans l'application, ",
               "comment sélectionner un pays, un indicateur, comment télécharger les données, etc."),
        tags$hr(),
        tags$p("Quelques points clés :"),
        tags$ul(
          tags$li("Étape 1 : Sélection du pays"),
          tags$li("Étape 2 : Sélection de l'indicateur"),
          tags$li("Étape 3 : Visualisation cartographique et analyse"),
          tags$li("Étape 4 : Téléchargement des résultats, etc.")
        ),
        tags$p("N'hésitez pas à insérer des captures d'écran ou des schémas. ",
               "Ici encore, tout est statique, donc vous pouvez simplement mettre vos ",
               "contenus ou tutos en HTML.")
      )
    )
  )
}

mod_guide_page_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    # Rien de spécial côté serveur pour une page statique
  })
}
