# ─────────────────────────────────────────────────────────────────────────────
# mod_map_page_KD.R
# Module 3 : Carte interactive
# RadioButtons (Région, Département, Commune, Grid)
# ─────────────────────────────────────────────────────────────────────────────

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
            div(
              style = "margin: 10px; font-size: 14px;",
              "Selon l'indicateur choisi, la carte peut afficher un agrégat par entité administrative ",
              "(Région, Département, Commune) ou le raster brut (Grid)."
            ),
            radioButtons(ns("admin_level"), "Niveau Administratif :",
                         choices = c("Région", "Département", "Commune", "Grid"),
                         selected = "Région",
                         inline = TRUE),
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
    
    data_reac <- reactive({
      req(landing_inputs())
      landing_inputs()
    })
    
    # Titre
    output$map_indicator_title <- renderText({
      req(data_reac()$indicator_chosen)
      paste0("Carte : ", data_reac()$indicator_chosen)
    })
    
    output$map <- renderLeaflet({
      req(data_reac()$indicator_chosen)
      
      # Déterminer la colonne ou le raster en fonction de l'indicateur
      indic <- data_reac()$indicator_chosen
      
      chosen_col <- switch(
        indic,
        "Taux moyen de Paludisme" = "mean_index",
        "Taux de malaria chez les enfants" = "taux_malaria_enfants",
        "NDVI" = "mean_ndvi",
        NULL
      )
      
      # Mode Grid => affichage du raster correspondant
      if (input$admin_level == "Grid") {
        
        raster_layer <- switch(
          indic,
          "Taux moyen de Paludisme" = data_global$mean_raster,
          "Taux de malaria chez les enfants" = {
            # Ex. ratio raster_malaria / raster_pop
            if (is.null(data_global$raster_nombre_malaria_enfants) ||
                is.null(data_global$raster_pop_enfants)) {
              NULL
            } else {
              data_global$raster_nombre_malaria_enfants / data_global$raster_pop_enfants
            }
          },
          "NDVI" = data_global$ndvi_raster,
          NULL
        )
        
        req(raster_layer)
        
        pal_r <- colorNumeric("viridis", domain = range(values(raster_layer), na.rm = TRUE))
        
        leaflet() %>%
          addTiles() %>%
          addRasterImage(raster_layer, colors = pal_r, opacity = 0.8) %>%
          addLegend(
            pal = pal_r,
            values = values(raster_layer),
            title = indic
          )
        
      } else {
        # Mode agrégé (Région/Département/Commune)
        req(chosen_col)
        
        # On récupère le bon sf
        sf_data <- switch(
          input$admin_level,
          "Région" = data_global$regions,
          "Département" = data_global$departments,
          "Commune" = data_global$communes
        )
        
        req(sf_data)
        
        pal <- colorNumeric("viridis", domain = range(sf_data[[chosen_col]], na.rm = TRUE))
        
        # Déterminer le champ de nom pour l’info popup
        name_col <- switch(
          input$admin_level,
          "Région" = "ADM1_FR",
          "Département" = "ADM2_FR",
          "Commune" = "ADM3_FR"
        )
        
        leaflet(sf_data) %>%
          addTiles() %>%
          addPolygons(
            fillColor = ~ pal(get(chosen_col)),
            fillOpacity = 0.7,
            color = "white",
            weight = 2,
            popup = ~ paste0(input$admin_level, " : ", get(name_col),
                             "<br>Valeur : ", round(get(chosen_col), 3))
          ) %>%
          addLegend(
            "bottomright",
            pal = pal,
            values = sf_data[[chosen_col]],
            title = paste(indic, "(", input$admin_level, ")")
          )
      }
      
    })
    
    # Plein écran (modal)
    observeEvent(input$btn_fullscreen, {
      showModal(modalDialog(
        title = "Carte en plein écran",
        size = "l",
        easyClose = TRUE,
        leafletOutput(session$ns("map_full"), width = "100%", height = "600px")
      ))
    })
    
    # Carte plein écran (même logique)
    output$map_full <- renderLeaflet({
      req(data_reac()$indicator_chosen)
      
      # Réutilisez la même logique que ci-dessus,
      # ou factorisez dans une fonction si vous préférez ne pas dupliquer.
      
      # Pour simplifier, on va juste rappeler input$admin_level et faire la même chose.
      
      indic <- data_reac()$indicator_chosen
      chosen_col <- switch(
        indic,
        "Taux moyen de Paludisme" = "mean_index",
        "Taux de malaria chez les enfants" = "taux_malaria_enfants",
        "NDVI" = "mean_ndvi",
        NULL
      )
      
      if (input$admin_level == "Grid") {
        raster_layer <- switch(
          indic,
          "Taux moyen de Paludisme" = data_global$mean_raster,
          "Taux de malaria chez les enfants" = {
            if (is.null(data_global$raster_nombre_malaria_enfants) ||
                is.null(data_global$raster_pop_enfants)) {
              NULL
            } else {
              data_global$raster_nombre_malaria_enfants / data_global$raster_pop_enfants
            }
          },
          "NDVI" = data_global$ndvi_raster,
          NULL
        )
        req(raster_layer)
        pal_r <- colorNumeric("viridis", domain = range(values(raster_layer), na.rm = TRUE))
        
        leaflet() %>%
          addTiles() %>%
          addRasterImage(raster_layer, colors = pal_r, opacity = 0.8) %>%
          addLegend(
            pal = pal_r,
            values = values(raster_layer),
            title = indic
          )
        
      } else {
        req(chosen_col)
        sf_data <- switch(
          input$admin_level,
          "Région" = data_global$regions,
          "Département" = data_global$departments,
          "Commune" = data_global$communes
        )
        req(sf_data)
        
        pal <- colorNumeric("viridis", domain = range(sf_data[[chosen_col]], na.rm = TRUE))
        name_col <- switch(
          input$admin_level,
          "Région" = "ADM1_FR",
          "Département" = "ADM2_FR",
          "Commune" = "ADM3_FR"
        )
        
        leaflet(sf_data) %>%
          addTiles() %>%
          addPolygons(
            fillColor = ~ pal(get(chosen_col)),
            fillOpacity = 0.7,
            color = "white",
            weight = 2,
            popup = ~ paste0(input$admin_level, " : ", get(name_col),
                             "<br>Valeur : ", round(get(chosen_col), 3))
          ) %>%
          addLegend(
            "bottomright",
            pal = pal,
            values = sf_data[[chosen_col]],
            title = paste(indic, "(", input$admin_level, ")")
          )
      }
    })
    
  })
}
