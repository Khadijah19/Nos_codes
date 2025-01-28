# ─────────────────────────────────────────────────────────────────────────────
# mod_landing_page.R
# Module 1 : Page d'accueil (Home) avec davantage d'espacement pour éviter chevauchement
# ─────────────────────────────────────────────────────────────────────────────

mod_landing_page_ui <- function(id) {
  ns <- NS(id)
  fluidPage(
    # Ajout d’un style CSS plus riche
    tags$head(
      tags$link(rel = "stylesheet", href = "https://fonts.googleapis.com/css?family=Open+Sans:400,600,700"),
      tags$style(HTML("
        body {
          background: linear-gradient(135deg, #e0e0e0, #f7f7f7);
          font-family: 'Open Sans', sans-serif;
          margin: 0; 
          padding: 0;
        }
        .landing-container {
          max-width: 1200px; 
          margin: 40px auto;  /* Augmente la marge haute/basse */
          background-color: #ffffff;
          border-radius: 10px;
          box-shadow: 0px 0px 15px rgba(0,0,0,0.1);
          padding: 30px;  /* Augmente le padding interne */
        }
        .landing-title {
          color: #333333; 
          font-weight: 700;
          margin-bottom: 30px; /* Plus de marge sous le titre */
          text-align: center;
          font-size: 28px;
          position: relative;
        }
        .landing-title:hover {
          color: #cc0000;
          transition: color 0.5s;
        }
        .landing-description {
          background-color: #666666; 
          color: white;
          padding: 20px; 
          border-radius: 5px;
          margin-bottom: 30px; /* Espace plus grand en bas */
          font-size: 16px;
          line-height: 1.6;
        }
        .landing-description p {
          margin-bottom: 15px; /* Espace entre les paragraphes */
        }
        .select-input-label {
          font-weight: 600;
          margin-top: 20px; /* Espace au-dessus du label */
          display: block;   /* Pour que le label occupe toute la ligne */
          margin-bottom: 5px;
        }
        .custom-select {
          display: inline-block;
          width: 300px;
          padding: 8px;
          border-radius: 4px;
          border: 1px solid #ccc;
          font-size: 14px;
          margin-bottom: 20px;
        }
      "))
    ),
    
    # Contenu principal
    div(
      class = "landing-container",
      
      # Titre principal
      div(
        class = "landing-title",
        "Portail interactif pour l'analyse des indicateurs sanitaires, sécuritaires et environnementaux"
      ),
      
      # Zone de texte sombre (description)
      div(
        class = "landing-description",
        p("Bienvenue sur notre portail interactif, dédié à l'exploration et à l'analyse d'indicateurs ",
          "clés en matière de santé, de sécurité et d'environnement. Que vous soyez chercheur, ",
          "décideur ou simplement curieux, ce site vous permettra de visualiser des cartes, ",
          "des graphiques et des tableaux synthétiques pour différents pays et pour plusieurs ",
          "indicateurs pertinents. Profitez également de fonctionnalités avancées : filtres par ",
          "niveau administratif, analyse par région et téléchargement de données."),
        p("Pour commencer, sélectionnez ci-dessous un pays, puis un indicateur. ",
          "Consultez ensuite nos sections 'Guide' et 'Notes Techniques' pour en savoir plus ",
          "sur l'utilisation et la méthodologie.")
      ),
      
      # Sélection du pays
      tags$label("Sélectionnez un pays :", class = "select-input-label"),
      selectInput(ns("country"), NULL,
                  choices = c("", "Senegal", "Burkina"),
                  selected = "", width = "300px"),
      
      # Sélection de l'indicateur (apparait seulement si pays choisi)
      conditionalPanel(
        condition = sprintf("input['%s-country'] != ''", ns("")),
        tags$label("Sélectionnez un indicateur :", class = "select-input-label"),
        selectInput(ns("indicator_chosen"), NULL, choices = NULL, width = "300px")
      )
    )
  )
}

mod_landing_page_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    # Met à jour la liste d'indicateurs si un pays est choisi
    observeEvent(input$country, {
      req(input$country)
      country_selected <- input$country
      indics <- fake_indicators[[country_selected]]
      updateSelectInput(session, "indicator_chosen", 
                        choices = c("", indics), 
                        selected = "")
      
      # Mettre à jour les données globales (shapefiles, rasters, etc.)
      if (country_selected != "") {
        update_data_global(country_selected)
      }
    })
    
    # Retourne le pays et l'indicateur
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
