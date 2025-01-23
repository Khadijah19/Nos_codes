mod_indicator_page_ui <- function(id) {
  ns <- NS(id)
  fluidPage(
    # Condition pour afficher le contenu uniquement si un pays et un indicateur sont sélectionnés
    conditionalPanel(
      condition = sprintf("input['%s-country'] !== '' && input['%s-indicator'] !== ''", ns(""), ns("")),
      fluidRow(
        column(
          width = 12,
          # Boîte avec le titre de l'indicateur
          div(
            class = "red-title-box",
            textOutput(ns("indicator_title"))
          ),
          # Boîte contenant la description de l'indicateur
          div(
            class = "dark-box",
            uiOutput(ns("indicator_description"))
          )
        )
      )
    )
  )
}

mod_indicator_page_server <- function(id, landing_inputs) {
  moduleServer(id, function(input, output, session) {
    # Récupérer les valeurs du module Landing Page
    data_reac <- reactive({
      landing_inputs()
    })
    
    # Mettre à jour le titre de l'indicateur
    output$indicator_title <- renderText({
      if (data_reac()$indicator == "") {
        return("") # Ne rien afficher si aucun indicateur n'est sélectionné
      } else {
        data_reac()$indicator
      }
    })
    
    # Mettre à jour la description de l'indicateur dynamiquement
    output$indicator_description <- renderUI({
      if (data_reac()$indicator == "Taux moyen de Paludisme") {
        tagList(
          p("Cet indicateur correspond à la moyenne des valeurs d'incidence du paludisme ",
            "calculées à partir de rasters couvrant l'ensemble du territoire Sénégalais. ",
            "Pour chaque pixel, on calcule le taux de paludisme, puis on agrège en moyenne ",
            "pour obtenir un 'Taux moyen de Paludisme'."),
          p("La méthodologie utilise la fonction 'exact_extract()' qui effectue une extraction ",
            "pondérée pour chaque entité (région/département/commune), puis on stocke ",
            "le résultat dans la variable 'mean_index'."),
          p("Les données sources (rasters, shapefiles) proviennent d'analyses spatiales ",
            "réalisées au Sénégal.")
        )
      } else if (data_reac()$indicator == "Taux d'enfants atteints par la malaria") {
        tagList(
          p("Cet indicateur représente le pourcentage d'enfants âgés de 0 à 12 ans ",
            "atteints par la malaria, calculé en appliquant un taux d'infection ",
            "à la population d'enfants estimée à partir des rasters de population."),
          p("La méthodologie utilise les rasters de population et un taux fixe ",
            "d'incidence pour estimer le nombre d'enfants malades. Les résultats ",
            "sont agrégés par région, département et commune."),
          p("Les données sources incluent les rasters de population et d'incidence ",
            "de la malaria, ainsi que les shapefiles des limites administratives.")
        )
      } else {
        p("Aucune description disponible pour cet indicateur.")
      }
    })
    
    # Retourne un booléen pour indiquer si un indicateur est sélectionné
    return(
      reactive({
        data_reac()$indicator != ""
      })
    )
  })
}
