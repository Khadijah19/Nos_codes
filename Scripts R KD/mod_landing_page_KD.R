# ─────────────────────────────────────────────────────────────────────────────
# mod_landing_page.R
# Module 1 : Page d'accueil, liste déroulante (Sénégal), indicateurs
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
    titlePanel("Cartographie infranationale des indicateurs de santé et de développement infantiles et maternels dans certains pays"),
    
    # Zone de texte sombre (description)
    div(
      class = "dark-box",
      p("Cette application web présente un résumé des indicateurs de santé et de développement ",
        "infantiles et maternels calculés au niveau infranational (zones géographiques situées ",
        "sous le niveau national) pour une sélection de pays."),
      p("Plusieurs indicateurs sont présentés sous forme de cartes, de graphiques et de tableaux, ",
        "et pour plusieurs points dans le temps selon la disponibilité des données. Les changements ",
        "au fil du temps pour chaque indicateur sont également présentés."),
      p("Veuillez consulter le Guide et la section À propos pour plus d'informations sur la manière ",
        "d'utiliser ce portail.")
    ),
    
    # Sélection du pays => ici "Sénégal" uniquement
    selectInput(ns("country"), "Sélectionnez un pays :",
                choices = c("", fake_countries),
                selected = ""),
    
    # Sélection de l'indicateur => maintenant 2 possibilités
    conditionalPanel(
      condition = sprintf("input['%s'] != ''", ns("country")),
      selectInput(ns("indicator"), "Sélectionnez un indicateur :", choices = NULL)
    )
  )
}

mod_landing_page_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    # Met à jour la liste d'indicateurs si le pays est choisi
    observeEvent(input$country, {
      req(input$country)
      country_selected <- input$country
      indics <- fake_indicators[[country_selected]]
      updateSelectInput(session, "indicator", choices = c("", indics), selected = "")
    })
    
    # Retourne le pays + indicateur
    return(
      reactive({
        list(
          country = input$country,
          indicator = input$indicator
        )
      })
    )
  })
}
