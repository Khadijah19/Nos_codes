# ─────────────────────────────────────────────────────────────────────────────
# mod_about_page_KD.R
# Page "About"
# ─────────────────────────────────────────────────────────────────────────────
mod_about_page_ui <- function(id) {
  ns <- NS(id)
  fluidPage(
    fluidRow(
      column(
        12,
        tags$h2("À propos"),
        tags$p("Cette application web fournit une analyse détaillée des indicateurs de santé publique et de développement à l'échelle infranationale, en mettant l'accent sur :"),
        tags$ul(
          tags$li("Données socio-économiques : taux de paludisme, Indicateur de diffusion de conflits"),
          tags$li("Données environnementales : indices spectraux")
        ),
        tags$p("Les indicateurs sont visualisés sous forme de cartes interactives, graphiques et tableaux synthétiques couvrant plusieurs périodes et intégrant différentes sources de données."),
        tags$hr(),
        tags$h3("Méthodes", class = "text-secondary"),
        tags$p("Les résultats sont issus d'une modélisation standard pour estimer des cartes à haute résolution. Notez que des écarts peuvent exister par rapport aux mesures agrégées publiées ailleurs."),
        tags$p("Pour plus de détails, consultez la section : ",
               tags$a("#Notes techniques#", href = "#", class = "text-info")),
        tags$hr(),
        tags$h3("Avertissement", class = "text-danger"),
        tags$p("Cette application a été développée dans un cadre académique. La représentation des frontières politiques ne reflète aucune position officielle. Les erreurs éventuelles n'engagent ni l'Ecole nationale de la Statistique et de l’Analyse économique Pierre NDIAYE (ENSAE), ni l'Agence nationale de la Statistique et de la Démographie (ANSD)."),
        tags$hr(),
        tags$h3("Remerciements", class = "text-success"),
        tags$p("Nous exprimons notre gratitude à M. Aboubacar HEMA pour son encadrement et ses conseils précieux tout au long de ce projet."),
        tags$p("Un grand merci également à nos camarades, dont le soutien et les échanges constructifs ont contribué à la réussite de ce travail."),
        tags$hr(),
        tags$h3("Références bibliographiques", class = "text-muted"),
        tags$ul(
          tags$li("UNFPA ONU, La modélisation de la relation entre démographie, paix et sécurité (2020)")
        ),
        tags$h3("Références webographiques", class = "text-muted"),
        tags$ul(
          tags$li(tags$a("ACLED (Armed Conflict Location and Event Data)", href = "https://acleddata.com", target = "_blank")),
          tags$li(tags$a("CRAN: Package malariaAtlas", href = "https://cran.r-project.org", target = "_blank")),
          tags$li(tags$a("Malaria Atlas Project | Home", href = "https://malariaatlas.org", target = "_blank")),
          tags$li(tags$a("Malaria Journal - Estimated distribution of malaria cases among children", href = "https://malariajournal.biomedcentral.com", target = "_blank")),
          tags$li(tags$a("NDMI: Vegetation Index Equation And Values Interpretation", href = "https://www.example.com", target = "_blank")),
          tags$li(tags$a("NDVI et PRI - METER Group", href = "https://www.metergroup.com", target = "_blank")),
          tags$li(tags$a("Statistique-Exploratoire-Spatiale/TP10", href = "https://github.com/Abson-dev/Statistique-Exploratoire-Spatiale", target = "_blank"))
        ),
        tags$hr(),
        tags$h3("Contributeurs", class = "text-primary"),
        tags$p("Ce portail a été développé par les étudiants de la 3e génération d’ISEP, en ISE1 cycle long, pour l’année académique 2024/2025 à l’Ecole nationale de la Statistique et de l’Analyse économique Pierre Ndiaye (ENSAE).", class = "text-secondary text-center"),
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
  moduleServer(id, function(input, output, session) { })
}
