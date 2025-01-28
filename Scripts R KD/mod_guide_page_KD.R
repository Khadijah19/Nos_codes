# ─────────────────────────────────────────────────────────────────────────────
# mod_guide_page_KD.R
# Page "Guide"
# ─────────────────────────────────────────────────────────────────────────────

mod_guide_page_ui <- function(id) {
  ns <- NS(id)
  fluidPage(
    tags$head(tags$style(HTML("
      .red-title-box {
        background-color: #cc0000;
        color: white;
        padding: 15px;
        border-radius: 8px;
        font-weight: bold;
        font-size: 24px;
        text-align: center;
        margin-bottom: 20px;
      }
      .guide-container {
        background-color: #fefefe;
        border-radius: 10px;
        box-shadow: 0px 0px 10px rgba(0,0,0,0.1);
        padding: 20px;
        margin-bottom: 30px;
      }
      .guide-text {
        font-size: 16px;
        line-height: 1.5;
        color: #333;
      }
      .guide-images {
        margin-top: 20px;
      }
      .guide-image-box {
        margin-bottom: 15px;
        text-align: center;
      }
      .guide-image {
        max-width: 100%;
        border-radius: 8px;
        box-shadow: 0px 2px 5px rgba(0,0,0,0.15);
      }
    "))),
    
    # Titre rouge centré
    div(class = "red-title-box", "Guide d'Utilisation"),
    
    # Partie principale : 2 colonnes : texte à gauche, images à droite
    fluidRow(
      column(
        width = 7,
        div(
          class = "guide-container",
          div(
            class = "guide-text",
            tags$h3("Comment naviguer dans l'application ?"),
            tags$ol(
              tags$li("Choisissez d'abord un pays et un indicateur dans la page Home."),
              tags$li("La carte s'actualise automatiquement selon le niveau administratif sélectionné (Région, Département, Commune ou Grid)."),
              tags$li("Vous pouvez consulter les statistiques détaillées dans l'onglet 'Analyse par Région' (résumé, tableaux, graphiques)."),
              tags$li("Un filtre par quartiles est disponible pour affiner l'affichage."),
              tags$li("Tous les tableaux et graphiques sont téléchargeables (boutons CSV / PNG).")
            ),
            
            tags$h3("Liens utiles"),
            tags$ul(
              tags$li(tags$a("Vidéo Démonstration (exemple)", href = "#", target = "_blank")),
              tags$li(tags$a("Documentation OMS sur le Paludisme", href = "https://www.who.int/fr/health-topics/malaria", target = "_blank"))
            )
          )
        )
      ),
      
      column(
        width = 5,
        div(
          class = "guide-container guide-images",
          
          # LOGO1
          div(
            class = "guide-image-box",
            tags$img(src = "Images/LOGO1.jpeg",
                     alt = "Logo 1",
                     class = "guide-image")
          ),
          
          # LOGO2
          div(
            class = "guide-image-box",
            tags$img(src = "Images/LOGO2.jpeg",
                     alt = "Logo 2",
                     class = "guide-image")
          ),
          
          # LOGO3
          div(
            class = "guide-image-box",
            tags$img(src = "Images/LOGO3.jpeg",
                     alt = "Logo 3",
                     class = "guide-image")
          )
        )
      )
    )
  )
}

mod_guide_page_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    # Page statique
  })
}
