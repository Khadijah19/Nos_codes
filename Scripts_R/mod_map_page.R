mod_map_page_ui <- function(id) {
  ns <- NS(id)
  fluidPage(
    # Cette page ne s'affiche que si un pays ET un indicateur sont sélectionnés
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
            # RadioButtons pour sélectionner le niveau administratif ou 'Grid'
            radioButtons(
              inputId = ns("admin_level"),
              label   = "Niveau Administratif :",
              choices = c("Région", "Département", "Commune", "Grid"),
              selected = "Grid",
              inline   = TRUE
            ),
            
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

mod_map_page_server <- function(id, landing_inputs,indicator_chosen_) {
  moduleServer(id, function(input, output, session) {
    
    # Récupération des infos (pays, indicateur) depuis la landing page
    data_reac <- reactive({
      req(landing_inputs()$country, landing_inputs()$indicator_chosen)
      list(
        country = landing_inputs()$country,
        indicator_chosen = landing_inputs()$indicator_chosen
      )
    })
    
    # Mise à jour des données si le pays et l'indicateur changent
    observeEvent(data_reac(), {
      req(data_reac()$country, data_reac()$indicator_chosen)
      update_data_global(data_reac()$country, data_reac()$indicator_chosen)
    })
    
    # Titre affiché au-dessus de la carte
    output$map_indicator_title <- renderText({
      req(data_reac()$indicator_chosen)
      paste0("Carte interactive : ", data_reac()$indicator_chosen)
    })
    
    # Rendu principal de la carte
    output$map <- renderLeaflet({
      req(data_global$raster_data, data_reac()$indicator_chosen)
      
      # Déterminer la colonne ou le raster en fonction de l'indicateur
      indic <- data_reac()$indicator_chosen
      
      # Sélectionner la colonne de données ou le raster
      chosen_col <- switch(
        indic,
        "Taux moyen de Paludisme"             = "mean_index",
        "Taux de malaria chez les enfants"    = "taux_malaria_enfants",
        "NDVI"                                = "mean_ndvi",
        "NDBI"                                = "mean_ndbi",
        "CDI"                                 = "CDI",
        NULL
      )
      
      # Affichage du raster si mode "Grid"
      if (input$admin_level == "Grid") {
        raster_layer <- data_global$raster_data
        req(raster_layer)
        
        pal_r <- colorNumeric("viridis", domain = range(values(raster_layer), na.rm = TRUE), na.color = "transparent")
        
        leaflet() %>%
          addTiles() %>%
          addRasterImage(
            raster_layer, 
            colors  = pal_r,
            opacity = 0.8
          ) %>%
          addLegend(
            pal    = pal_r,
            values = values(raster_layer),
            title  = paste("Valeur des Pixels -", indic)
          )
        
      } else {
        # Mode agrégé (Région, Département ou Commune)
        req(chosen_col)
        
        entity_data <- switch(
          input$admin_level,
          "Région"      = data_global$regions,
          "Département" = data_global$departments,
          "Commune"     = data_global$communes
        )
        
        req(entity_data)
        
        # Vérification que la colonne existe dans les données
        if (!chosen_col %in% names(entity_data)) {
          showNotification(
            "⚠️ La colonne demandée n’existe pas dans les données de ce niveau administratif.",
            type = "error"
          )
          return(leaflet() %>% addTiles())
        }
        
        pal <- colorNumeric("viridis", domain = range(entity_data[[chosen_col]], na.rm = TRUE), na.color = "transparent")
        
        name_col <- switch(
          input$admin_level,
          "Région"      = "ADM1_FR", 
          "Département" = "ADM2_FR",
          "Commune"     = "ADM3_FR"
        )
        
        leaflet(entity_data) %>%
          addTiles() %>%
          addPolygons(
            fillColor       = ~ pal(get(chosen_col)),
            fillOpacity     = 0.7,
            color           = "white",
            weight          = 2,
            highlightOptions= highlightOptions(
              weight = 3,
              color  = "#666",
              fillOpacity = 0.9,
              bringToFront = TRUE
            ),
            label = ~paste0(
              input$admin_level, " : ", get(name_col),
              " ; ", indic, " = ", round(get(chosen_col), 3)
            ),
            labelOptions = labelOptions(
              style = list(
                "font-weight"       = "bold",
                "color"             = "#666",
                "background-color"  = "rgba(255,255,255,0.8)",
                "border"            = "1px solid #ccc",
                "border-radius"     = "4px",
                "padding"           = "4px"
              ),
              textsize  = "13px",
              direction = "auto"
            )
          ) %>%
          addLegend(
            position = "bottomright",
            pal      = pal,
            values   = entity_data[[chosen_col]],
            title    = paste(indic, "(", input$admin_level, ")")
          )
      }
    })
    
    # Bouton pour afficher la carte en plein écran (modal)
    observeEvent(input$btn_fullscreen, {
      showModal(
        modalDialog(
          title     = "Carte en plein écran",
          size      = "l",
          easyClose = TRUE,
          leafletOutput(session$ns("map_full"), width = "100%", height = "600px")
        )
      )
    })
    
    # Rendu de la carte en mode plein écran
    output$map_full <- renderLeaflet({
      req(data_global$raster_data, data_reac()$indicator_chosen)
      
      # Déterminer la colonne ou le raster en fonction de l'indicateur
      indic <- data_reac()$indicator_chosen
      
      # Sélectionner la colonne de données ou le raster
      chosen_col <- switch(
        indic,
        "Taux moyen de Paludisme"             = "mean_index",
        "Taux de malaria chez les enfants"    = "taux_malaria_enfants",
        "NDVI"                                = "mean_ndvi",
        "NDBI"                                = "mean_ndbi",
        "CDI"                                 = "CDI",
        NULL
      )
      
      # Affichage du raster si mode "Grid"
      if (input$admin_level == "Grid") {
        raster_layer <- data_global$raster_data
        req(raster_layer)
        
        pal_r <- colorNumeric("viridis", domain = range(values(raster_layer), na.rm = TRUE), na.color = "transparent")
        
        leaflet() %>%
          addTiles() %>%
          addRasterImage(
            raster_layer, 
            colors  = pal_r,
            opacity = 0.8
          ) %>%
          addLegend(
            pal    = pal_r,
            values = values(raster_layer),
            title  = paste("Valeur des Pixels -", indic)
          )
        
      } else {
        # Mode agrégé (Région, Département ou Commune)
        req(chosen_col)
        
        entity_data <- switch(
          input$admin_level,
          "Région"      = data_global$regions,
          "Département" = data_global$departments,
          "Commune"     = data_global$communes
        )
        
        req(entity_data)
        
        # Vérification que la colonne existe dans les données
        if (!chosen_col %in% names(entity_data)) {
          showNotification(
            "⚠️ La colonne demandée n’existe pas dans les données de ce niveau administratif.",
            type = "error"
          )
          return(leaflet() %>% addTiles())
        }
        
        pal <- colorNumeric("viridis", domain = range(entity_data[[chosen_col]], na.rm = TRUE), na.color = "transparent")
        
        name_col <- switch(
          input$admin_level,
          "Région"      = "ADM1_FR", 
          "Département" = "ADM2_FR",
          "Commune"     = "ADM3_FR"
        )
        
        leaflet(entity_data) %>%
          addTiles() %>%
          addPolygons(
            fillColor       = ~ pal(get(chosen_col)),
            fillOpacity     = 0.7,
            color           = "white",
            weight          = 2,
            highlightOptions= highlightOptions(
              weight = 3,
              color  = "#666",
              fillOpacity = 0.9,
              bringToFront = TRUE
            ),
            label = ~paste0(
              input$admin_level, " : ", get(name_col),
              " ; ", indic, " = ", round(get(chosen_col), 3)
            ),
            labelOptions = labelOptions(
              style = list(
                "font-weight"       = "bold",
                "color"             = "#666",
                "background-color"  = "rgba(255,255,255,0.8)",
                "border"            = "1px solid #ccc",
                "border-radius"     = "4px",
                "padding"           = "4px"
              ),
              textsize  = "13px",
              direction = "auto"
            )
          ) %>%
          addLegend(
            position = "bottomright",
            pal      = pal,
            values   = entity_data[[chosen_col]],
            title    = paste(indic, "(", input$admin_level, ")")
          )
      }
    })
  })
}
