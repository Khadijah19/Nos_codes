# ─────────────────────────────────────────────────────────────────────────────
# mod_indicator_page.R
# Module 2 : Affichage de l'indicateur sélectionné (méthodologie, description)
# ─────────────────────────────────────────────────────────────────────────────

mod_indicator_page_ui <- function(id) {
  ns <- NS(id)
  fluidPage(
    tags$head(
      tags$style(HTML("
      .red-title-box {
        display: inline-block;
        background-color: red;
        color: white;
        font-weight: bold;
        padding: 10px;
        border-radius: 5px;
        white-space: nowrap;
      }

      .dark-box {
        display: inline-block;
        background-color: #333;
        color: white;
        padding: 10px;
        border-radius: 5px;
        max-width: 100%;
        word-wrap: break-word;
      }
    "))
    ),
    # Condition pour afficher le contenu uniquement si un pays et un indicateur sont sélectionnés
    conditionalPanel(
      condition = sprintf("input['%s-country'] !== '' && input['%s-indicator_chosen'] !== ''", ns(""), ns("")),
      fluidRow(
        column(
          width = 12,
          # Boîte avec le titre de l'indicateur
          div(
            class = "red-title-box",
            style = "display: inline-block; padding: 10px; border-radius: 5px;",
            textOutput(ns("indicator_title"))
          ),
          # Boîte contenant la description de l'indicateur (dynamique)
          div(
            class = "dark-box",
            style = "display: inline-block; padding: 10px; border-radius: 5px; max-width: 100%;",
            uiOutput(ns("indicator_desc"))
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
          p("Le taux moyen de malaria, calculé ici sur une période de 22 ans (2000-2022) permet de suivre l'évolution de la prévalence de cette maladie infectieuse dans une région donnée sur trois niveaux administratifs (pays, région, département, commune)."))
      } else if (indic == "Taux de malaria chez les enfants") {
        tagList(
          p("Cet indicateur mesure la prévalence de la malaria chez les enfants de moins de 5 ans en 2022."))
      } else if (indic == "CDI") {
        tagList(
          p("Le Conflict Diffusion Indicator (CDI), ou Indicateur de Diffusion des Conflits, évalue l'étendue géographique et l'intensité des conflits en tenant compte de leur impact sur les populations. Pour le calculer, plusieurs étapes méthodologiques ont été suivies. Tout d'abord, le raster des événements a été binarisé : les pixels avec plus de 5 événements ont été affectés d’une valeur de 1, les autres d'une valeur de 0. Ensuite, le raster de population, initialement à une résolution de 100 m, a été agrégé à une résolution de 5 km. Ce raster a également été binarisé, attribuant une valeur de 1 aux pixels ayant plus de 50 habitants, et 0 sinon. Les deux rasters binarisés ont été multipliés, permettant de mettre en évidence les zones de conflits touchant des populations significatives."),
          p("Le CDI a ensuite été calculé en comptant le nombre de pixels égaux à 1 dans le raster binarisé de population (indiquant la population totale concernée) et dans le raster produit obtenu par multiplication. Enfin, le rapport entre ces deux valeurs a été pris : il représente le Conflict Diffusion Indicator, calculé à l’échelle nationale et régionale.")
        )
      }else if (indic == "NDVI") {
        tagList(
          p("Le Normalized Difference Vegetation Index (NDVI), ou Indice de Végétation par Différence Normalisée, est un indicateur spectral clé en télédétection, utilisé pour évaluer la présence et la santé de la végétation verte."),
          a("Calcul de NDVI",href="https://farmonaut.com/remote-sensing/mastering-normalized-difference-indices-from-ndbi-to-ndmi-a-comprehensive-guide-for-remote-sensing-in-agriculture/")
        )
      }else if (indic == "NDBI") {
        tagList(
          p("Le NDBI (Normalized Difference Built-up Index) est un outil efficace pour cartographier les zones bâties, grâce à sa simplicité et à sa capacité à réduire les biais liés à l’éclairage et aux conditions atmosphériques grace à sa formule basée sur le rapport SWIR/NIR."),
          a("Calcul de NDBI",href="https://farmonaut.com/remote-sensing/mastering-normalized-difference-indices-from-ndbi-to-ndmi-a-comprehensive-guide-for-remote-sensing-in-agriculture/")
          
        )  
      }else {
        tagList(
          p("Indicateur non documenté."))
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
