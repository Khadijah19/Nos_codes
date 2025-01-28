# ─────────────────────────────────────────────────────────────────────────────
# mod_landing_page_KD.R
# Page d'accueil, liste déroulante multi-pays, puis indicateurs
# ─────────────────────────────────────────────────────────────────────────────

mod_landing_page_ui <- function(id) {
  ns <- NS(id)
  fluidPage(
    tags$head(tags$style(
      HTML("
        body {
          background-color: #f2f2f2;
        }
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
        }
      ")
    )),
    
    # Titre principal
    titlePanel("Cartographie infranationale des indicateurs de santé et de développement"),
    
    # Zone de texte sombre (description)
    div(
      class = "dark-box",
      p("Cette application web fournit une analyse détaillée des indicateurs de santé publique ",
        "et de développement à l'échelle infranationale, en mettant l'accent sur des données ",
        "socio-économiques et environnementales. Plusieurs indicateurs sont visualisés ",
        "sous forme de cartes interactives, de graphiques explicatifs et de tableaux ",
        "synthétiques, couvrant plusieurs périodes et intégrant différentes sources de données."),
      p("Veuillez consulter le Guide et la section About pour plus d'informations ",
        "sur l'utilisation de ce portail.")
    ),
    
    # Sélection du pays
    selectInput(ns("country"), "Sélectionnez un pays :",
                choices = c("", "Senegal", "Burkina"),
                selected = ""),
    
    # Sélection de l'indicateur
    conditionalPanel(
      condition = sprintf("input['%s'] != ''", ns("country")),
      selectInput(ns("indicator_chosen"), "Sélectionnez un indicateur :", choices = NULL)
    )
  )
}

mod_landing_page_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    # Met à jour la liste d'indicateurs en fonction du pays choisi
    observeEvent(input$country, {
      req(input$country)
      country_selected <- input$country
      indics <- fake_indicators[[country_selected]]
      updateSelectInput(session, "indicator_chosen",
                        choices = c("", indics),
                        selected = "")
      
      # Mise à jour des data_global via la fonction (on recharge les shapefiles/rasters)
      if (country_selected != "") {
        update_data_global(country_selected)
      }
    })
    
    # Retourne le pays et l'indicateur sélectionné
    return(
      reactive({
        list(
          country = input$country,
          indicator_chosen = input$indicator_chosen
        )
      })
    )
  })
}
