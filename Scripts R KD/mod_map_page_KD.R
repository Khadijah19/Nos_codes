# ─────────────────────────────────────────────────────────────────────────────
# mod_map_page.R
# Module 3 : Carte interactive. RadioButtons Région/Département/Commune
# ─────────────────────────────────────────────────────────────────────────────

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
            # Définition structurée (exemple)
            div(
              style = "margin: 10px; font-size: 18px; font-weight: bold;",
              "Définition : Sélectionnez ci-dessous le niveau administratif pour visualiser l'indicateur."
            ),
            div(
              style = "margin: 10px; font-size: 14px;",
              "Selon l'indicateur choisi, la carte affichera soit le Taux moyen de Paludisme, ",
              "soit le Taux d'enfant atteint par la malaria (agrégation par entité administrative)."
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
    
    data_reac <- reactive({
      landing_inputs()
    })
    
    # Mise à jour du titre de la carte
    output$map_indicator_title <- renderText({
      req(data_reac()$indicator != "")
      paste0("Carte interactive du Sénégal : ", data_reac()$indicator)
    })
    
    output$map <- renderLeaflet({
      req(data_reac()$indicator != "")
      
      # 1) Choisir la colonne de données en fonction de l'indicateur
      chosen_col <- if (data_reac()$indicator == "Taux moyen de Paludisme") {
        "mean_index"
      } else {
        "taux_malaria"
      }
      
      # 2) Palette viridis
      # On définit un domaine en fusionnant les valeurs, si besoin
      # (Ici, on applique domain = NULL, ce qui s'adapte aux données chargées)
      pal <- colorNumeric("viridis", domain = NULL, na.color = "grey50")
      
      # 3) En fonction du niveau administratif
      if (input$admin_level == "Région") {
        leaflet(regions) %>%
          addTiles() %>%
          addPolygons(
            fillColor = ~pal(get(chosen_col)),
            fillOpacity = 0.7,
            color = "white",
            weight = 2,
            popup = ~paste0("Région : ", ADM1_FR, 
                            "<br>Taux : ", round(get(chosen_col), 3))
          ) %>%
          addLegend("bottomright", pal = pal, values = regions[[chosen_col]],
                    title = paste(data_reac()$indicator, "(Région)"))
        
      } else if (input$admin_level == "Département") {
        leaflet(departments) %>%
          addTiles() %>%
          addPolygons(
            fillColor = ~pal(get(chosen_col)),
            fillOpacity = 0.7,
            color = "white",
            weight = 2,
            popup = ~paste0("Département : ", ADM2_FR,
                            "<br>Taux : ", round(get(chosen_col), 3))
          ) %>%
          addLegend("bottomright", pal = pal, values = departments[[chosen_col]],
                    title = paste(data_reac()$indicator, "(Dépt)"))
        
      } else {
        leaflet(communes) %>%
          addTiles() %>%
          addPolygons(
            fillColor = ~pal(get(chosen_col)),
            fillOpacity = 0.7,
            color = "white",
            weight = 2,
            popup = ~paste0("Commune : ", ADM3_FR,
                            "<br>Taux : ", round(get(chosen_col), 3))
          ) %>%
          addLegend("bottomright", pal = pal, values = communes[[chosen_col]],
                    title = paste(data_reac()$indicator, "(Commune)"))
      }
    })
    
    # Bouton pour afficher la carte en plein écran (modal)
    observeEvent(input$btn_fullscreen, {
      showModal(modalDialog(
        title = "Carte du Sénégal (plein écran)",
        size = "l",
        easyClose = TRUE,
        leafletOutput(session$ns("map_full"), width = "100%", height = "600px")
      ))
    })
    
    # Rendu de la carte en mode plein écran (même logique)
    output$map_full <- renderLeaflet({
      req(data_reac()$indicator != "")
      chosen_col <- if (data_reac()$indicator == "Taux moyen de Paludisme") {
        "mean_index"
      } else {
        "taux_malaria"
      }
      pal <- colorNumeric("viridis", domain = NULL, na.color = "grey50")
      
      if (input$admin_level == "Région") {
        leaflet(regions) %>%
          addTiles() %>%
          addPolygons(
            fillColor = ~pal(get(chosen_col)),
            fillOpacity = 0.7,
            color = "white",
            weight = 2,
            popup = ~paste0("Région : ", ADM1_FR,
                            "<br>Taux : ", round(get(chosen_col), 3))
          ) %>%
          addLegend("bottomright", pal = pal, values = regions[[chosen_col]],
                    title = paste(data_reac()$indicator, "(Région)"))
        
      } else if (input$admin_level == "Département") {
        leaflet(departments) %>%
          addTiles() %>%
          addPolygons(
            fillColor = ~pal(get(chosen_col)),
            fillOpacity = 0.7,
            color = "white",
            weight = 2,
            popup = ~paste0("Département : ", ADM2_FR,
                            "<br>Taux : ", round(get(chosen_col), 3))
          ) %>%
          addLegend("bottomright", pal = pal, values = departments[[chosen_col]],
                    title = paste(data_reac()$indicator, "(Dépt)"))
        
      } else {
        leaflet(communes) %>%
          addTiles() %>%
          addPolygons(
            fillColor = ~pal(get(chosen_col)),
            fillOpacity = 0.7,
            color = "white",
            weight = 2,
            popup = ~paste0("Commune : ", ADM3_FR,
                            "<br>Taux : ", round(get(chosen_col), 3))
          ) %>%
          addLegend("bottomright", pal = pal, values = communes[[chosen_col]],
                    title = paste(data_reac()$indicator, "(Commune)"))
      }
    })
    
    return(NULL)
  })
}
