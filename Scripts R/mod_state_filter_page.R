# ─────────────────────────────────────────────────────────────────────────────
# mod_state_filter_page.R
# Module 4 : Sélection d’une Région, puis onglets Résumé, Tableau, Graphique
# + Téléchargements
# ─────────────────────────────────────────────────────────────────────────────

mod_state_filter_page_ui <- function(id){
  ns <- NS(id)
  fluidPage(
    # Le module n’apparaît que si le pays ET l’indicateur sont sélectionnés
    conditionalPanel(
      condition = sprintf("input['landing_page-country'] !== '' && input['landing_page-indicator_chosen'] !== ''"),
      
      fluidRow(
        column(
          width = 3,
          # Liste dynamique des régions (on mettra à jour dans le server)
          selectInput(ns("region"), "Sélectionnez une Région :",
                      choices = c("")),
          
          selectInput(ns("signif"), "Filtrer par Quartile :",
                      choices = c("Afficher tout", "0-25%", "25-50%", "50-75%", "75-100%")),
          
          helpText("Les données affichées peuvent être filtrées selon la fourchette de valeurs.")
        ),
        
        column(
          width = 9,
          # Afficher la suite uniquement si on a choisi une région
          conditionalPanel(
            condition = sprintf("input['%s'] != ''", ns("region")),
            div(
              class = "dark-box",
              div(
                class = "red-title-box",
                textOutput(ns("selected_indicator_title"))
              ),
              div(
                class = "red-title-box",
                textOutput(ns("selected_region_title"))
              ),
              p("Nous présentons ci-dessous les statistiques résumées par région ",
                "pour l'indicateur sélectionné.")
            ),
            
            tabsetPanel(
              # --- Onglet Résumé ---
              tabPanel(
                title = "Résumé",
                br(),
                # Bouton de téléchargement pour le tableau résumé
                downloadButton(ns("download_resume_table"), "Télécharger le résumé (CSV)"),
                tableOutput(ns("resume_table")),
                hr(),
                textOutput(ns("resume_comment"))
              ),
              
              # --- Onglet Tableau ---
              tabPanel(
                title = "Tableau",
                br(),
                h4("Tableau des Départements"),
                downloadButton(ns("download_dep_table"), "Télécharger (CSV)"),
                tableOutput(ns("dep_table")),
                
                h4("Tableau des Communes"),
                downloadButton(ns("download_com_table"), "Télécharger (CSV)"),
                tableOutput(ns("com_table")),
                
                hr(),
                textOutput(ns("table_comment"))
              ),
              
              # --- Onglet Graphique ---
              tabPanel(
                title = "Graphique",
                br(),
                plotOutput(ns("dep_plot"), height = "300px"),
                downloadButton(ns("download_dep_plot"), "Télécharger Graphique Départements (PNG)"),
                
                plotOutput(ns("com_plot"), height = "300px"),
                downloadButton(ns("download_com_plot"), "Télécharger Graphique Communes (PNG)")
              )
            )
          )
        )
      )
    )
  )
}

mod_state_filter_page_server <- function(id, landing_inputs, indicator_chosen_) {
  moduleServer(id, function(input, output, session) {
    
    data_reac <- reactive({ landing_inputs() })
    
    # 1) Mettre à jour la liste de régions dès que data_global$regions est modifié (multi-pays)
    observeEvent(data_global$regions, {
      req(data_global$regions)
      region_choices <- sort(unique(data_global$regions$ADM1_FR))
      updateSelectInput(
        session, "region",
        choices = c("", region_choices),
        selected = ""
      )
    })
    
    # 2) Titres
    output$selected_indicator_title <- renderText({
      req(data_reac()$indicator_chosen)
      paste("Indicateur sélectionné :", data_reac()$indicator_chosen)
    })
    
    output$selected_region_title <- renderText({
      req(input$region)
      paste("Région sélectionnée :", input$region)
    })
    
    # 3) Fonction pour la fourchette
    
    # 4) Colonne selon l’indicateur
    chosen_col <- reactive({
      switch(
        data_reac()$indicator_chosen,
        "Taux moyen de Paludisme"          = "mean_index",
        "Taux de malaria chez les enfants" = "taux_malaria_enfants",
        "NDVI"                             = "mean_ndvi",
        "CDI"= "CDI",
        "NDBI" = "mean_ndbi",
        NULL
      )
    })
    
    validate_chosen_col <- reactive({
      if (is.null(chosen_col())) {
        showNotification("Aucun indicateur valide n'est sélectionné.", type = "error")
        stop("Colonne choisie NULL.")
      }
      chosen_col()
    })
    # Fonction pour calculer les bornes des quantiles
    getQuantileRange <- function(data, signif_str, chosen_col) {
      # Si aucun filtre n'est appliqué (Afficher tout)
      if (signif_str == "Afficher tout") {
        return(c(-Inf, Inf))
      }
      
      # Calcul des quantiles sur la colonne sélectionnée
      quantiles <- quantile(data[[chosen_col]], probs = c(0, 0.25, 0.5, 0.75, 1), na.rm = TRUE)
      
      # Retourner les bornes correspondant au choix
      if (signif_str == "0-25%") return(c(quantiles[1], quantiles[2]))
      if (signif_str == "25-50%") return(c(quantiles[2], quantiles[3]))
      if (signif_str == "50-75%") return(c(quantiles[3], quantiles[4]))
      if (signif_str == "75-100%") return(c(quantiles[4], quantiles[5]))
      
      # Valeur par défaut (en cas de problème)
      return(c(-Inf, Inf))
    }
    
    
    # 5) Filtrage
    deps_filtered <- reactive({
      req(input$region, validate_chosen_col())
      minmax <- getQuantileRange(data_global$departments, input$signif, chosen_col())
      data_global$departments %>%
        filter(
          ADM1_FR == input$region,
          .data[[chosen_col()]] >= minmax[1],
          .data[[chosen_col()]] <= minmax[2]
        )
    })
    
    coms_filtered <- reactive({
      req(input$region, validate_chosen_col())
      minmax <- getQuantileRange(data_global$communes, input$signif, chosen_col())
      data_global$communes %>%
        filter(
          ADM1_FR == input$region,
          .data[[chosen_col()]] >= minmax[1],
          .data[[chosen_col()]] <= minmax[2]
        )
    })
    
    # 6) Tableau Résumé
    summary_table_reactive <- reactive({
      req(input$region, validate_chosen_col())
      
      dep_vals <- deps_filtered()[[chosen_col()]]
      com_vals <- coms_filtered()[[chosen_col()]]
      
      reg_val <- data_global$regions %>%
        filter(ADM1_FR == input$region) %>%
        pull(chosen_col())
      
      data.frame(
        row.names = c("Moyenne", "Maximum", "Minimum"),
        "Région"      = c(
          round(mean(reg_val, na.rm = TRUE), 3),
          round(max(reg_val, na.rm = TRUE), 3),
          round(min(reg_val, na.rm = TRUE), 3)
        ),
        "Département" = c(
          round(mean(dep_vals, na.rm = TRUE), 3),
          round(max(dep_vals, na.rm = TRUE), 3),
          round(min(dep_vals, na.rm = TRUE), 3)
        ),
        "Commune"     = c(
          round(mean(com_vals, na.rm = TRUE), 3),
          round(max(com_vals, na.rm = TRUE), 3),
          round(min(com_vals, na.rm = TRUE), 3)
        )
      )
    })
    
    output$resume_table <- renderTable({
      summary_table_reactive()
    }, rownames = TRUE)
    
    output$resume_comment <- renderText({
      "Interprétation : ce tableau compare la valeur régionale et les valeurs des départements/communes filtrés."
    })
    
    # *** Téléchargement du tableau Résumé ***
    output$download_resume_table <- downloadHandler(
      filename = function(){
        paste0("resume_table_", input$region, "_", Sys.Date(), ".csv")
      },
      content = function(file){
        write.csv(summary_table_reactive(), file, row.names = TRUE)
      }
    )
    
    # 7) Tableaux départements / communes
    output$dep_table <- renderTable({
      req(input$region, validate_chosen_col())
      df <- deps_filtered()
      data.frame(
        Departement = df$ADM2_FR,
        Taux = round(df[[chosen_col()]], 3)
      )
    })
    
    output$com_table <- renderTable({
      req(input$region, validate_chosen_col())
      df <- coms_filtered()
      data.frame(
        Commune = df$ADM3_FR,
        Taux = round(df[[chosen_col()]], 3)
      )
    })
    
    output$table_comment <- renderText({
      "Tableaux listant les départements et communes respectant le filtre choisi."
    })
    
    # Téléchargements CSV pour Départements / Communes
    output$download_dep_table <- downloadHandler(
      filename = function(){
        paste0("departements_", input$region, "_", Sys.Date(), ".csv")
      },
      content = function(file){
        write.csv(deps_filtered(), file, row.names = FALSE)
      }
    )
    output$download_com_table <- downloadHandler(
      filename = function(){
        paste0("communes_", input$region, "_", Sys.Date(), ".csv")
      },
      content = function(file){
        write.csv(coms_filtered(), file, row.names = FALSE)
      }
    )
    
    # 8) Graphiques Départements / Communes
    output$dep_plot <- renderPlot({
      req(input$region, validate_chosen_col())
      df <- deps_filtered()
      ggplot(df, aes(x = ADM2_FR, y = .data[[chosen_col()]], group = 1)) +
        geom_bar(stat = "identity", fill = "skyblue", color = "black") +
        theme_minimal() +
        labs(title = "Taux par Département", x = "Département", y = "Valeur") +
        theme(panel.background = element_rect(fill = "white", color = NA),
              plot.background = element_rect(fill = "white", color = NA),
              axis.text.x = element_text(angle = 45, hjust = 1))
    })
    
    output$com_plot <- renderPlot({
      req(input$region, validate_chosen_col())
      df <- coms_filtered()
      ggplot(df, aes(x = ADM3_FR, y = .data[[chosen_col()]], group = 1)) +
        geom_bar(stat = "identity", fill = "skyblue", color = "black")+
        theme_minimal() +
        labs(title = "Taux par Commune", x = "Commune", y = "Valeur") +
        theme(panel.background = element_rect(fill = "white", color = NA),
              plot.background = element_rect(fill = "white", color = NA),
              axis.text.x = element_text(angle = 45, hjust = 1))
    })
    
    # Téléchargements PNG pour Départements / Communes
    output$download_dep_plot <- downloadHandler(
      filename = function() {
        paste0("graphique_departements_", input$region, "_", Sys.Date(), ".png")
      },
      content = function(file) {
        df <- deps_filtered()
        p <- ggplot(df, aes(x = ADM2_FR, y = .data[[chosen_col()]])) +
          geom_bar(stat = "identity", fill = "skyblue", color = "black")+
          theme_minimal() +
          labs(title = "Graphique par Département", x = "Département", y = "Valeur")
        
        ggsave(file, plot = p, device = "png", width = 10, height = 6, bg = "white")
      }
    )
    
    output$download_com_plot <- downloadHandler(
      filename = function() {
        paste0("graphique_communes_", input$region, "_", Sys.Date(), ".png")
      },
      content = function(file) {
        df <- coms_filtered()
        p <- ggplot(df, aes(x = ADM3_FR, y = .data[[chosen_col()]])) +
          geom_bar(stat = "identity", fill = "skyblue", color = "black") +
          theme_minimal() +
          labs(title = "Graphique par Commune", x = "Commune", y = "Valeur")
        
        ggsave(file, plot = p, device = "png", width = 10, height = 6, bg = "white")
      }
    )
    
  })
}