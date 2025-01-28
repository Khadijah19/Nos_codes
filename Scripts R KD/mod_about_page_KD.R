# ─────────────────────────────────────────────────────────────────────────────
# mod_about_page_KD.R
# Page "About"
# ─────────────────────────────────────────────────────────────────────────────

mod_about_page_ui <- function(id) {
  ns <- NS(id)
  fluidPage(
    # On récupère la même CSS que la Home
    tags$head(tags$style(HTML("
      .dark-box {
        background-color: #666666;
        color: white;
        padding: 15px;
        border-radius: 5px;
        margin-bottom: 20px;
      }
      .red-title-box {
        background-color: #cc0000;
        color: white;
        padding: 10px;
        border-radius: 5px;
        font-weight: bold;
        margin-bottom: 10px;
        font-size: 20px;
      }
      .sub-section-box {
        background-color: #444444;
        color: white;
        padding: 10px;
        border-radius: 5px;
        margin-bottom: 10px;
      }
      .sub-section-title {
        background-color: red;
        color: white;
        padding: 6px;
        border-radius: 5px;
        font-weight: bold;
        margin-bottom: 5px;
      }
    "))),
    
    # Contenu
    # 1) Un grand titre "À propos" en box rouge
    div(
      class = "red-title-box",
      "À propos"
    ),
    
    # 2) Un encadré style "dark-box" pour tout le texte
    div(
      class = "dark-box",
      
      p("Cette application web fournit une analyse détaillée des indicateurs de santé publique ",
        "et de développement à l'échelle infranationale, en mettant l'accent sur :"),
      tags$ul(
        tags$li("Données socio-économiques : taux de paludisme, Indicateur de diffusion de conflits"),
        tags$li("Données environnementales : indices spectraux")
      ),
      
      p("Les indicateurs sont visualisés sous forme de cartes interactives, graphiques et tableaux ",
        "synthétiques couvrant plusieurs périodes et intégrant différentes sources de données."),
      
      # Avertissement
      div(
        class = "sub-section-box",
        div(class="sub-section-title", "Avertissement"),
        p("Cette application a été développée dans un cadre académique. ",
          "La représentation des frontières politiques ne reflète aucune position officielle. ",
          "Les erreurs éventuelles n'engagent ni l'Ecole nationale de la Statistique et de la ",
          "Démographie (ANSD).")
      ),
      
      # Remerciements
      div(
        class = "sub-section-box",
        div(class="sub-section-title", "Remerciements"),
        p("Nous exprimons notre gratitude à M. Aboubacar HEMA pour son encadrement et ses conseils ",
          "précieux tout au long de ce projet."),
        p("Un grand merci également à nos camarades, dont le soutien et les échanges constructifs ",
          "ont contribué à la réussite de ce travail.")
      ),
      
      # Références bibliographiques
      div(
        class = "sub-section-box",
        div(class="sub-section-title", "Références bibliographiques"),
        tags$ul(
          tags$li("UNFPA ONU, La modélisation de la relation entre démographie, paix et sécurité (2020)")
        )
      ),
      
      # Références webographiques
      div(
        class = "sub-section-box",
        div(class="sub-section-title", "Références webographiques"),
        tags$ul(
          tags$li(tags$a("ACLED (Armed Conflict Location and Event Data)", 
                         href = "https://acleddata.com", target = "_blank")),
          tags$li(tags$a("CRAN: Package malariaAtlas", 
                         href = "https://cran.r-project.org/web/packages/malariaAtlas/index.html",
                         target = "_blank")),
          tags$li(tags$a("Malaria Atlas Project | Home", 
                         href = "https://malariaatlas.org/", target = "_blank")),
          tags$li(tags$a("Malaria Journal - Estimated distribution of malaria cases among children", 
                         href = "https://malariajournal.biomedcentral.com/articles/10.1186/s12936-023-04811-z",
                         target = "_blank")),
          tags$li(tags$a("NDBI—ArcGIS Pro | Documentation", 
                         href = "https://pro.arcgis.com/en/pro-app/latest/arcpy/spatial-analyst/ndbi.htm",
                         target = "_blank")),
          tags$li(tags$a("NDVI et PRI - METER Group", 
                         href = "https://metergroup.com/fr/education-guides/ndvi-and-pri-the-researchers-complete-guide/",
                         target = "_blank"))
        )
      ),
      
      # Contributeurs
      div(
        class = "sub-section-box",
        div(class="sub-section-title", "Contributeurs"),
        p("Ce portail a été développé par les étudiants de la 3e génération d’ISEP, en ISE1 cycle long, ",
          "pour l’année académique 2024/2025 à l’Ecole nationale de la Statistique ",
          "et de l’Analyse économique Pierre Ndiaye (ENSAE)."),
        tags$ul(
          tags$li("Alioune Abdou Salam KANE"),
          tags$li("Khadidiatou DIAKHATE"),
          tags$li("Ange Emilson Rayan RAHERINASOLO"),
          tags$li("Awa DIAW")
        )
      )
    )
  )
}

mod_about_page_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    # Page statique
  })
}
