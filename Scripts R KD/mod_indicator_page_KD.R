# ─────────────────────────────────────────────────────────────────────────────
# mod_indicator_page.R
# Module 2 : Affichage de l'indicateur sélectionné (méthodologie, description)
# ─────────────────────────────────────────────────────────────────────────────

mod_indicator_page_ui <- function(id) {
  ns <- NS(id)
  fluidPage(
    # Condition pour afficher le contenu uniquement si un pays et un indicateur sont sélectionnés
    conditionalPanel(
      condition = sprintf("input['%s-country'] !== '' && input['%s-indicator_chosen'] !== ''", ns(""), ns("")),
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
      if (data_reac()$indicator_chosen == "") {
        return("") # Ne rien afficher si aucun indicateur n'est sélectionné
      } else {
        data_reac()$indicator_chosen
      }
    })
    
    
    # Affichage dynamique : méthodologie + description
    output$indicator_desc <- renderUI({
      req(data_reac()$indicator_chosen)
      indic <- data_reac()$indicator_chosen
      
      if (indic == "Taux moyen de Paludisme") {
        tagList(
          p("Cet indicateur correspond à la moyenne des valeurs d'incidence du paludisme calculées..."))
      } else if (indic == "Taux de malaria chez les enfants") {
        tagList(
          p("Cet indicateur mesure le pourcentage d'enfants atteints de malaria..."))
      } else if (indic == "NDVI") {
        tagList(
          p("Le NDVI (Normalized Difference Vegetation Index) est un indicateur qui mesure la végétation. "),
          p("Il varie entre -1 et 1, où des valeurs proches de 1 indiquent une végétation dense, et des valeurs proches de 0 indiquent des zones arides.")
        )
      } else {
        tagList(p("Indicateur non documenté."))
      }
    })
    
    
    # Retourne un booléen pour indiquer si un indicateur est sélectionné
    return(
      reactive({
        data_reac()$indicator_chosen != ""
      })
    )
  })
}
