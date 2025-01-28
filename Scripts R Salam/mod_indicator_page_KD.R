# ─────────────────────────────────────────────────────────────────────────────
# mod_indicator_page_KD.R
# Module 2 : Affichage de l'indicateur (description, méthodo)
# ─────────────────────────────────────────────────────────────────────────────

mod_indicator_page_ui <- function(id) {
  ns <- NS(id)
  fluidPage(
    conditionalPanel(
      condition = sprintf("input['%s-country'] !== '' && input['%s-indicator_chosen'] !== ''", ns(""), ns("")),
      fluidRow(
        column(
          width = 12,
          div(
            class = "red-title-box",
            textOutput(ns("indicator_title"))
          ),
          div(
            class = "dark-box",
            uiOutput(ns("indicator_desc"))
          )
        )
      )
    )
  )
}

mod_indicator_page_server <- function(id, landing_inputs) {
  moduleServer(id, function(input, output, session) {
    
    data_reac <- reactive({
      landing_inputs()
    })
    
    # Titre
    output$indicator_title <- renderText({
      if (data_reac()$indicator_chosen == "") {
        return("")
      } else {
        data_reac()$indicator_chosen
      }
    })
    
    # Description dynamique selon l’indicateur
    output$indicator_desc <- renderUI({
      req(data_reac()$indicator_chosen)
      indic <- data_reac()$indicator_chosen
      
      if (indic == "Taux moyen de Paludisme") {
        tagList(
          p("Cet indicateur correspond à la moyenne des valeurs d'incidence du paludisme ",
            "calculées à partir de rasters couvrant le territoire.")
        )
      } else if (indic == "Taux de malaria chez les enfants") {
        tagList(
          p("Il s'agit du pourcentage d'enfants (0-5 ans) atteints par la malaria, ",
            "calculé sur la base de rasters estimant la population enfantine et ",
            "les cas de paludisme.")
        )
      } else if (indic == "NDVI") {
        tagList(
          p("Le NDVI (Normalized Difference Vegetation Index) reflète la densité de végétation. ",
            "Il est calculé à partir de bandes spectrales satellite (proche infrarouge / rouge).")
        )
      } else {
        tagList(p("Indicateur non documenté."))
      }
    })
    
    # Renvoie un booléen
    return(
      reactive({
        data_reac()$indicator_chosen != ""
      })
    )
  })
}
