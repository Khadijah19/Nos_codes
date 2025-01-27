# ─────────────────────────────────────────────────────────────────────────────
# mod_indicator_page.R
# Module 2 : Affichage de l'indicateur sélectionné (méthodologie, description)
# ─────────────────────────────────────────────────────────────────────────────

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
          # Boîte contenant la description de l'indicateur (dynamique)
          div(
            class = "dark-box",
            uiOutput(ns("indicator_desc"))  # On met ici un uiOutput
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
    
    # Mettre à jour le titre de l'indicateur (zone rouge)
    output$indicator_title <- renderText({
      if (data_reac()$indicator == "") {
        return("") # Ne rien afficher si aucun indicateur n'est sélectionné
      } else {
        data_reac()$indicator
      }
    })
    
    # Affichage dynamique : méthodologie + description
    output$indicator_desc <- renderUI({
      req(data_reac()$indicator)
      indic <- data_reac()$indicator
      
      if (indic == "Taux moyen de Paludisme") {
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
      } else if (indic == "Taux d'enfant atteint par la malaria") {
        tagList(
          p("Cet indicateur correspond à la proportion (ou pourcentage) d'enfants (0 à 12 ans) ",
            "touchés par la malaria dans une zone géographique donnée (région, département, commune). ",
            "Il s'agit du ratio entre le nombre d'enfants malades et le nombre total d'enfants."),
          p("Méthodologie : à partir d'un raster représentant la population enfantine et d'un ",
            "raster de prévalence de la malaria, nous calculons le nombre d'enfants infectés ",
            "et le rapportons à la population totale. L'agrégation par entité administrative ",
            "se fait via 'exact_extract()', puis nous stockons le résultat dans la colonne ",
            "'taux_malaria'.")
        )
      } else {
        # Au cas où un autre indicateur serait ajouté plus tard
        tagList(
          p("Indicateur non documenté.")
        )
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
