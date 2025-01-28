# ─────────────────────────────────────────────────────────────────────────────
# mod_guide_page.R
# Page "Guide" 
# ─────────────────────────────────────────────────────────────────────────────

mod_guide_page_ui <- function(id) {
  ns <- NS(id)
  fluidPage(
    # Style pour la mise en page
    tags$head(
      tags$style(HTML("
        .guide-title {
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
          background-color: #ffffff;
          border-radius: 10px;
          box-shadow: 0px 2px 8px rgba(0,0,0,0.1);
          padding: 20px;
          margin-bottom: 30px;
        }
        .guide-text {
          font-size: 16px;
          line-height: 1.6;
          color: #333;
        }
        .guide-text li {
          margin-bottom: 8px;
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
          margin-bottom: 10px;
        }
        .fictive-link {
          text-align: center; 
          margin-top: 30px;
          font-size: 16px;
        }
        .fictive-link a {
          color: #cc0000;
          text-decoration: underline;
          font-weight: bold;
        }
      "))
    ),
    
    # Titre principal de la page
    div(class = "guide-title", "Guide d'Utilisation"),
    
    fluidRow(
      # Colonne de gauche : instructions
      column(
        width = 6,
        div(
          class = "guide-container guide-text",
          tags$ol(
            tags$li("Sélectionner un pays : L'utilisateur peut choisir un pays dans une liste déroulante ou sur une carte interactive."),
            tags$li("Sélectionner un indicateur : L'utilisateur peut choisir un indicateur parmi une liste d'options (par exemple, taux de malaria, indices spectraux, CDI)."),
            tags$li("La définition de l’indicateur sélectionné et ce qui est représenté : Affichage d'une brève description de l'indicateur choisi et ce qu'il représente."),
            tags$li("Sélectionner le niveau administratif (région, département, commune) ou le mode Grid : Permet à l'utilisateur de choisir le niveau administratif ou d'opter pour une vue en grille."),
            tags$li("Agrandir la carte (zoom avant et arrière) : Utilisation des boutons + et – pour zoomer et dézoomer."),
            tags$li("Sélectionner une région : Après la sélection du pays et de l'indicateur, l'utilisateur peut sélectionner une région sur la carte ou via une liste."),
            tags$li("Filtrer par quartile (1er, 2e, 3e, 4e) ou tout sélectionner : L'utilisateur peut filtrer les données en fonction des quartiles."),
            tags$li("Tableau synthétique sur la région sélectionnée téléchargeable sous format .csv : Permet à l'utilisateur de télécharger un tableau de données pour la région."),
            tags$li("Tableaux synthétiques pour chaque département et commune de la région sélectionnée téléchargeables sous format .csv : Un tableau par département et commune."),
            tags$li("Graphiques synthétiques pour chaque département et commune téléchargeables sous format .png : L'utilisateur peut télécharger des graphiques de données.")
          )
        )
      ),
      
      # Colonne de droite : 7 images
      column(
        width = 6,
        div(
          class = "guide-container guide-images",
          
          # Image 1
          div(
            class = "guide-image-box",
            tags$img(src = "Images/1.png", alt = "Image 1", class = "guide-image")
          ),
          # Image 2
          div(
            class = "guide-image-box",
            tags$img(src = "Images/2.png", alt = "Image 2", class = "guide-image")
          ),
          # Image 3
          div(
            class = "guide-image-box",
            tags$img(src = "Images/3.png", alt = "Image 3", class = "guide-image")
          ),
          # Image 4
          div(
            class = "guide-image-box",
            tags$img(src = "Images/4.png", alt = "Image 4", class = "guide-image")
          ),
          # Image 5
          div(
            class = "guide-image-box",
            tags$img(src = "Images/5.png", alt = "Image 5", class = "guide-image")
          ),
          # Image 6
          div(
            class = "guide-image-box",
            tags$img(src = "Images/6.png", alt = "Image 6", class = "guide-image")
          ),
          # Image 7
          div(
            class = "guide-image-box",
            tags$img(src = "Images/7.png", alt = "Image 7", class = "guide-image")
          )
        )
      )
    ),
    
    # Lien fictif en bas de page (ou "Vidéo Démonstration" etc.)
    div(
      class = "fictive-link",
      "Pour plus d'informations sur l'application voir la vidéo : ",
      tags$a("Lien video de présentation", href="https://drive.google.com/drive/folders/1JHSLgr1ml-x9n6kArFRIwGir_g7e3bw-?usp=drive_link", target="_blank")
    )
  )
}

mod_guide_page_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    # Page statique, rien de spécial côté serveur
  })
}
