# ─────────────────────────────────────────────────────────────────────────────
# mod_guide_page.R
# Page "Guide"
# ─────────────────────────────────────────────────────────────────────────────
mod_guide_page_ui <- function(id) {
  ns <- NS(id)
  fluidPage(
    tags$style(HTML(
      "body { background-color: #f8f9fa; } \n
       h2 { color: #007bff; } \n
       .content-box { background-color: #ffffff; border-radius: 15px; padding: 20px; box-shadow: 1px 4px 6px rgba(0, 0, 0, 0.1); } \n
       .text-section { float: left; width: 50%; padding-right: 20px; } \n
       .screenshot-section { float: right; width: 50%; text-align: center; } \n
       .screenshot { margin: 10px auto; border: 1px solid #ddd; border-radius: 10px; box-shadow: 0px 4px 6px rgba(0, 0, 0, 0.1); }"
    )),
    fluidRow(
      column(
        12,
        div(class = "content-box",
            tags$h2("Guide d'Utilisation"),
            div(
              class = "text-section",
              tags$ol(
                tags$li(tags$span(style = "font-weight: bold;", "Sélectionner un pays et un Indicateur:"),
                        " Choisissez un pays et un indicateur à afficher. La carte par défaut montre les valeurs au niveau national."),
                tags$li(tags$span(style = "font-weight: bold;", "Navigation sur la Carte:"),
                        " Utilisez les boutons zoom avant (+) ou zoom arrière (-) pour ajuster la vue."),
                tags$li(tags$span(style = "font-weight: bold;", "Définition de l'Indicateur:"),
                        " Une brève définition de l’indicateur calculé. Plus de détails sont disponibles dans la section Notes techniques, qui explique la 'Probabilité de Dépassement et la Confiance dans les Changements au Fil du Temps.'"),
                tags$li(tags$span(style = "font-weight: bold;", "Détails sur la carte:"),
                        " Explorez les détails supplémentaires fournis sur la carte."),
                tags$li(tags$span(style = "font-weight: bold;", "Filtrage par Changement:"),
                        " Ajustez les paramètres pour mettre en évidence l'indicateur à un niveau administratif."),
                tags$li(tags$span(style = "font-weight: bold;", "Highlight une région:"),
                        " Mettez en valeur une région spécifique pour des analyses ciblées."),
                tags$li(tags$span(style = "font-weight: bold;", "Ajustez le curseur:"),
                        " Affinez les résultats en fonction d’un seuil particulier."),
                tags$li(tags$span(style = "font-weight: bold;", "Téléchargement des Données:"),
                        " Cliquez sur le bouton Télécharger situé en dessous de la carte ou au-dessus du tableau pour récupérer les données.")
              )
            ),
            div(
              class = "screenshot-section",
              tags$img(src = "LOGO1.JPEG", class = "screenshot", width = "90%", alt = "Screenshot 1"),
              tags$img(src = "C:/Users/hp/Desktop/AwaDIAW_ISE-CL/Semestre1/Informatique_Statistique/Statistique_exploratoire/Projet_Final_Stat_Spatiale/LOGO2.JPEG", class = "screenshot", width = "90%", alt = "Screenshot 2"),
              tags$img(src = "C:/Users/hp/Desktop/AwaDIAW_ISE-CL/Semestre1/Informatique_Statistique/Statistique_exploratoire/Projet_Final_Stat_Spatiale/LOGO3.JPEG", class = "screenshot", width = "90%", alt = "Screenshot 3")
            ),
            tags$div(style = "clear: both;") # Pour nettoyer le flottement
        )
      )
    )
  )
}

mod_guide_page_server <- function(id) {
  moduleServer(id, function(input, output, session) { })
}