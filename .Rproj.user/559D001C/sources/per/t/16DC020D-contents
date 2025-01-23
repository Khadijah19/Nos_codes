# ────────────────────────────────────────────────────────────────────────────
# mod_map_page.R
# Module 3 : Carte interactive. On propose un radioButton "Région/Département/Commune"
# et on utilise addPolygons() pour colorer en fonction de mean_index (viridis).
# ────────────────────────────────────────────────────────────────────────────

mod_map_page_ui <- function(id){
  ns <- NS(id)
  fluidPage(
    conditionalPanel(
      condition = sprintf("input['%s-country'] !== '' && input['%s-indicator'] !== ''", ns(""), ns("")),
      
      fluidRow(
        column(
          width = 12,
          div(
            class = "dark-box",
            div(
              class = "red-title-box",
              textOutput(ns("map_indicator_title"))
            ),
            # Définition structurée et mise en forme
            div(
              style = "margin: 10px; font-size: 18px; font-weight: bold;",
              uiOutput(ns("indicator_definition"))
            ),
            
            # RadioButtons pour niveau administratif
            radioButtons(ns("admin_level"), "Niveau Administratif :",
                         choices = c("Région", "Département", "Commune"),
                         selected = "Région",
                         inline = TRUE),
            
            # Carte Leaflet
            leafletOutput(ns("map"), width = "90%", height = "500px"),
            br(),
            actionButton(ns("btn_fullscreen"), "Agrandir la carte")
          )
        )
      )
    )
  )
}

mod_map_page_server <- function(id, landing_inputs, indicator_chosen){
  moduleServer(id, function(input, output, session){
    
    # Récupérer les inputs sélectionnés
    data_reac <- reactive({
      landing_inputs()
    })
    
    # Mise à jour du titre de la carte
    output$map_indicator_title <- renderText({
      req(data_reac()$indicator != "")
      paste0("Carte interactive du Sénégal : ", data_reac()$indicator)
    })
    
    # Mise à jour de la définition de l'indicateur
    output$indicator_definition <- renderUI({
      if (data_reac()$indicator == "Taux moyen de Paludisme") {
        "Définition : Taux moyen de Paludisme (moyenne des rasters) au Sénégal."
      } else if (data_reac()$indicator == "Taux d'enfants atteints par la malaria") {
        "Définition : Pourcentage d'enfants affectés par la malaria dans chaque zone administrative."
      } else {
        "Définition : Indicateur non disponible."
      }
    })
    
    # Rendu Leaflet avec coloration selon le niveau administratif choisi
    output$map <- renderLeaflet({
      req(data_reac()$indicator != "")
      
      # Palette viridis continue
      pal <- colorNumeric("viridis", domain = NULL, na.color = "grey50")
      
      if (data_reac()$indicator == "Taux moyen de Paludisme") {
        if (input$admin_level == "Région") {
          leaflet(regions) %>%
            addTiles() %>%
            addPolygons(
              fillColor = ~pal(mean_index),
              fillOpacity = 0.7,
              color = "white",
              weight = 2,
              popup = ~paste0("Région : ", ADM1_FR, "<br>Taux : ", round(mean_index, 3))
            ) %>%
            addLegend("bottomright", pal = pal, values = regions$mean_index,
                      title = "Taux moyen Palu (Région)")
        } else if (input$admin_level == "Département") {
          leaflet(departments) %>%
            addTiles() %>%
            addPolygons(
              fillColor = ~pal(mean_index),
              fillOpacity = 0.7,
              color = "white",
              weight = 2,
              popup = ~paste0("Département : ", ADM2_FR, "<br>Taux : ", round(mean_index, 3))
            ) %>%
            addLegend("bottomright", pal = pal, values = departments$mean_index,
                      title = "Taux moyen Palu (Dépt)")
        } else {
          leaflet(communes) %>%
            addTiles() %>%
            addPolygons(
              fillColor = ~pal(mean_index),
              fillOpacity = 0.7,
              color = "white",
              weight = 2,
              popup = ~paste0("Commune : ", ADM3_FR, "<br>Taux : ", round(mean_index, 3))
            ) %>%
            addLegend("bottomright", pal = pal, values = communes$mean_index,
                      title = "Taux moyen Palu (Commune)")
        }
      } else if (data_reac()$indicator == "Taux d'enfants atteints par la malaria") {
        if (input$admin_level == "Région") {
          leaflet(regions) %>%
            addTiles() %>%
            addPolygons(
              fillColor = ~pal(taux_malaria),
              fillOpacity = 0.7,
              color = "white",
              weight = 2,
              popup = ~paste0("Région : ", ADM1_FR, "<br>Taux de malaria : ", round(taux_malaria, 2), "%")
            ) %>%
            addLegend("bottomright", pal = pal, values = regions$taux_malaria,
                      title = "Taux de Malaria (Région)")
        } else if (input$admin_level == "Département") {
          leaflet(departments) %>%
            addTiles() %>%
            addPolygons(
              fillColor = ~pal(taux_malaria),
              fillOpacity = 0.7,
              color = "white",
              weight = 2,
              popup = ~paste0("Département : ", ADM2_FR, "<br>Taux de malaria : ", round(taux_malaria, 2), "%")
            ) %>%
            addLegend("bottomright", pal = pal, values = departments$taux_malaria,
                      title = "Taux de Malaria (Dépt)")
        } else {
          leaflet(communes) %>%
            addTiles() %>%
            addPolygons(
              fillColor = ~pal(taux_malaria),
              fillOpacity = 0.7,
              color = "white",
              weight = 2,
              popup = ~paste0("Commune : ", ADM3_FR, "<br>Taux de malaria : ", round(taux_malaria, 2), "%")
            ) %>%
            addLegend("bottomright", pal = pal, values = communes$taux_malaria,
                      title = "Taux de Malaria (Commune)")
        }
      }
    })
    
    return(NULL)
  })
}
