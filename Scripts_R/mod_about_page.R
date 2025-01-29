# ─────────────────────────────────────────────────────────────────────────────
# mod_about_page.R
# Page "About" 
# ─────────────────────────────────────────────────────────────────────────────

mod_about_page_ui <- function(id) {
  ns <- NS(id)
  fluidPage(
    tags$head(tags$style(HTML("
      /* Style du titre principal similaire à la page Guide */
      .about-title {
        background-color: #cc0000;
        color: white;
        padding: 15px;
        border-radius: 8px;
        font-weight: bold;
        font-size: 24px;
        text-align: center;
        margin-bottom: 20px;
      }

      /* Styles pour la mise en forme du contenu */
      .about-container {
        background-color: #f2f2f2; /* Gris clair */
        color: #333;
        padding: 20px;
        border-radius: 8px;
        margin-bottom: 20px;
        box-shadow: 0 2px 5px rgba(0,0,0,0.1);
      }
      .about-section {
        background-color: #ffffff; /* Boîte plus claire */
        color: #333;
        padding: 15px;
        border-radius: 5px;
        margin-bottom: 15px;
        box-shadow: 0 1px 3px rgba(0,0,0,0.05);
      }
      .about-section h4 {
        font-weight: bold;
        margin-top: 0;
        color: #555;
      }
      .about-section p, .about-section li {
        line-height: 1.6;
      }
    "))),
    
    # Titre principal en style "about-title"
    div(class = "about-title", "À propos"),
    
    # Contenu principal dans un conteneur
    div(
      class = "about-container",
      
      # 1) Présentation générale
      div(
        class = "about-section",
        p("Cette application web fournit une analyse détaillée des indicateurs de santé publique ",
          "et de développement à l'échelle infranationale, en mettant l'accent sur :"),
        tags$ul(
          tags$li("Données socio-économiques : taux de paludisme, Indicateur de diffusion de conflits"),
          tags$li("Données environnementales : indices spectraux")
        ),
        p("Les indicateurs sont visualisés sous forme de cartes interactives, graphiques et ",
          "tableaux synthétiques couvrant plusieurs périodes et intégrant différentes sources ",
          "de données.")
      ),
      
      # 2) Avertissement
      div(
        class = "about-section",
        h4("Avertissement"),
        p("Cette application a été développée dans un cadre académique. ",
          "La représentation des frontières politiques ne reflète aucune position officielle. ",
          "Les erreurs éventuelles n'engagent ni l'Ecole nationale de la Statistique et de ",
          "la Démographie (ANSD).")
      ),
      
      # 3) Remerciements
      div(
        class = "about-section",
        h4("Remerciements"),
        p("Nous exprimons notre gratitude à M. Aboubacar HEMA pour son encadrement et ses conseils ",
          "précieux tout au long de ce projet."),
        p("Un grand merci également à nos camarades, dont le soutien et les échanges constructifs ",
          "ont contribué à la réussite de ce travail.")
      ),
      
      # 4) Références bibliographiques
      div(
        class = "about-section",
        h4("Références bibliographiques"),
        tags$ul(
          tags$li("UNFPA ONU, La modélisation de la relation entre démographie, paix et sécurité (2020)")
        )
      ),
      
      # 5) Références webographiques
      div(
        class = "about-section",
        h4("Références webographiques"),
        tags$ul(
          tags$li(
            tags$a("ACLED (Armed Conflict Location and Event Data)",
                   href = "https://acleddata.com", target = "_blank")
          ),
          tags$li(
            tags$a("CRAN: Package malariaAtlas",
                   href = "https://cran.r-project.org/web/packages/malariaAtlas/index.html",
                   target = "_blank")
          ),
          tags$li(
            tags$a("Malaria Atlas Project | Home",
                   href = "https://malariaatlas.org/", target = "_blank")
          ),
          tags$li(
            tags$a("Malaria Journal - Estimated distribution of malaria cases among children",
                   href = "https://malariajournal.biomedcentral.com/articles/10.1186/s12936-023-04811-z",
                   target = "_blank")
          ),
          tags$li(
            tags$a("NDBI—ArcGIS Pro | Documentation",
                   href = "https://pro.arcgis.com/en/pro-app/latest/arcpy/spatial-analyst/ndbi.htm",
                   target = "_blank")
          ),
          tags$li(
            tags$a("NDVI et PRI - METER Group",
                   href = "https://metergroup.com/fr/education-guides/ndvi-and-pri-the-researchers-complete-guide/",
                   target = "_blank")
          )
        )
      ),
      
      # 6) Contributeurs
      div(
        class = "about-section",
        h4("Contributeurs"),
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
