# ——————————————————————————————————————————————————————————————————
# mod_map_page.R
# Module 3 : Carte interactive. RadioButtons Région/Département/Commune
# Gestion multi-pays
# ——————————————————————————————————————————————————————————————————

mod_map_page_ui <- function(id) {
  ns <- NS(id)
  fluidPage(
    conditionalPanel(
      condition = sprintf("input['%s-country'] !== '' && input['%s-indicator_chosen'] !== ''", ns(""), ns("")),
      fluidRow(
        column(
          width = 12,
          div(
            class = "dark-box",
            div(
              class = "red-title-box",
              textOutput(ns("map_indicator_title"))
            ),
            # Message descriptif
            div(
              style = "margin: 10px; font-size: 14px;",
              "Selon l'indicateur choisi, la carte affichera soit un agrégat par entité administrative, ",
              "soit le raster des pixels (mode Grid) directement."
            ),
            # Boutons radio avec l'option "Grid"
            radioButtons(ns("admin_level"), "Niveau Administratif :",
                         choices = c("Région", "Département", "Commune", "Grid"),
                         selected = "Grid",
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

mod_map_page_server <- function(id, landing_inputs, indicator_chosen_) {
  moduleServer(id, function(input, output, session) {
    # Récupération des données réactives
    data_reac <- reactive({
      req(landing_inputs())
      landing_inputs()
    })
    
    # Observer le changement de pays et mettre à jour les données globales
    observeEvent(data_reac()$country, {
      req(data_reac()$country)
      update_data_global(data_reac()$country)
    })
    
    # Mise à jour du titre de la carte
    output$map_indicator_title <- renderText({
      req(data_reac()$indicator_chosen)
      paste0("Carte interactive : ", data_reac()$indicator_chosen)
    })
    
    # Carte principale
    output$map <- renderLeaflet({
      req(data_reac()$indicator_chosen)
      
      chosen_col <- switch(
        data_reac()$indicator_chosen,
        "Taux moyen de Paludisme" = "mean_index",
        "Taux de malaria chez les enfants" = "taux_malaria_enfants",
        "NDVI" = "mean_ndvi",
        "NDBI" = "mean_ndbi",
        "CDI" = "CDI",  # Nouvelle entrée pour l'indicateur CDI
        NULL
      )
      req(chosen_col)
      
      if (input$admin_level == "Grid") {
        raster_layer <- switch(
          data_reac()$indicator_chosen,
          "Taux moyen de Paludisme" = data_global$mean_raster,
          "Taux de malaria chez les enfants" = data_global$raster_nombre_malaria_enfants / data_global$raster_pop_enfants,
          "NDVI" = data_global$ndvi_raster,
          "NDBI" = data_global$ndbi_raster,
          "CDI" = data_global$mult_raster/data_global$pop_resampled_binary,
          NULL
        )
        
        req(raster_layer)
        
        leaflet() %>%
          addTiles() %>%
          addRasterImage(
            raster_layer,
            colors = colorNumeric("viridis", domain = range(values(raster_layer), na.rm = TRUE), na.color = "transparent"),
            opacity = 0.8
          ) %>%
          addLegend(
            pal = colorNumeric("viridis", domain = range(values(raster_layer), na.rm = TRUE), na.color = "transparent"),
            values = values(raster_layer),
            title = "Valeur des Pixels"
          )
      } else {
        entity_data <- switch(
          input$admin_level,
          "Région" = data_global$regions,
          "Département" = data_global$departments,
          "Commune" = data_global$communes
        )
        req(entity_data)
        
        # Palette de couleurs avec NA transparents
        pal <- colorNumeric("viridis", domain = range(entity_data[[chosen_col]], na.rm = TRUE), na.color = "transparent")
        
        # Carte Leaflet avec mise en surbrillance et infobulle
        leaflet(entity_data) %>%
          addTiles() %>%
          addPolygons(
            fillColor = ~pal(entity_data[[chosen_col]]),
            fillOpacity = 0.7,
            color = "white",
            weight = 2,
            highlightOptions = highlightOptions(  # Surbrillance
              weight = 3,
              color = "#666",
              fillOpacity = 0.9,
              bringToFront = TRUE
            ),
            label = ~paste0(  # Infobulle
               input$admin_level, " ", 
              if (input$admin_level == "Région") ADM1_FR else 
                if (input$admin_level == "Département") ADM2_FR else ADM3_FR,
              " ; ", data_reac()$indicator_chosen, " : ",
              round(entity_data[[chosen_col]], 3)
            ),
            labelOptions = labelOptions(  # Style de l'infobulle
              style = list(
                "font-weight" = "bold",
                "color" = "#666",
                "background-color" = "rgba(255,255,255,0.8)",
                "border" = "1px solid #ccc",
                "border-radius" = "4px",
                "padding" = "4px"
              ),
              textsize = "13px",
              direction = "auto"
            )
          ) %>%
          addLegend(
            "bottomright",
            pal = pal,
            values = entity_data[[chosen_col]],
            title = paste(data_reac()$indicator_chosen, "(", input$admin_level, ")")
          )
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
      req(data_reac()$indicator_chosen)
      
      chosen_col <- switch(
        data_reac()$indicator_chosen,
        "Taux moyen de Paludisme" = "mean_index",
        "Taux de malaria chez les enfants" = "taux_malaria_enfants",
        "NDVI" = "mean_ndvi",
        "NDBI" = "mean_ndbi",
        "CDI" = "CDI",  # Nouvelle entrée pour l'indicateur CDI
        NULL
      )
      req(chosen_col)
      
      if (input$admin_level == "Grid") {
        raster_layer <- switch(
          data_reac()$indicator_chosen,
          "Taux moyen de Paludisme" = data_global$mean_raster,
          "Taux de malaria chez les enfants" = data_global$raster_nombre_malaria_enfants / data_global$raster_pop_enfants,
          "NDVI" = data_global$ndvi_raster,
          "NDBI" = data_global$ndbi_raster,
          "CDI" = data_global$mult_raster/data_global$pop_resampled_binary,
          NULL
        )
        
        req(raster_layer)
        
        leaflet() %>%
          addTiles() %>%
          addRasterImage(
            raster_layer,
            colors = colorNumeric("viridis", domain = range(values(raster_layer), na.rm = TRUE), na.color = "transparent"),
            opacity = 0.8
          ) %>%
          addLegend(
            pal = colorNumeric("viridis", domain = range(values(raster_layer), na.rm = TRUE), na.color = "transparent"),
            values = values(raster_layer),
            title = "Valeur des Pixels"
          )
      } else {
        entity_data <- switch(
          input$admin_level,
          "Région" = data_global$regions,
          "Département" = data_global$departments,
          "Commune" = data_global$communes
        )
        req(entity_data)
        
        # Palette de couleurs avec NA transparents
        pal <- colorNumeric("viridis", domain = range(entity_data[[chosen_col]], na.rm = TRUE), na.color = "transparent")
        
        # Carte Leaflet avec mise en surbrillance et infobulle
        leaflet(entity_data) %>%
          addTiles() %>%
          addPolygons(
            fillColor = ~pal(entity_data[[chosen_col]]),
            fillOpacity = 0.7,
            color = "white",
            weight = 2,
            highlightOptions = highlightOptions(  # Surbrillance
              weight = 3,
              color = "#666",
              fillOpacity = 0.9,
              bringToFront = TRUE
            ),
            label = ~paste0(  # Infobulle
               input$admin_level, " : ", 
              if (input$admin_level == "Région") ADM1_FR else 
                if (input$admin_level == "Département") ADM2_FR else ADM3_FR,
              " ; ", data_reac()$indicator_chosen, " : ",
              round(entity_data[[chosen_col]], 3)
            ),
            labelOptions = labelOptions(  # Style de l'infobulle
              style = list(
                "font-weight" = "bold",
                "color" = "#666",
                "background-color" = "rgba(255,255,255,0.8)",
                "border" = "1px solid #ccc",
                "border-radius" = "4px",
                "padding" = "4px"
              ),
              textsize = "13px",
              direction = "auto"
            )
          ) %>%
          addLegend(
            "bottomright",
            pal = pal,
            values = entity_data[[chosen_col]],
            title = paste(data_reac()$indicator_chosen, "(", input$admin_level, ")")
          )
      }
    })
    
    
  })
}
