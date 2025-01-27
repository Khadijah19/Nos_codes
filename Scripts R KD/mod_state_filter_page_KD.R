# ─────────────────────────────────────────────────────────────────────────────
# mod_state_filter_page.R
# Module 4 : Sélection d’une Région, puis onglets Résumé, Tableau, Graphique
# ─────────────────────────────────────────────────────────────────────────────
mod_state_filter_page_ui <- function(id){
  ns <- NS(id)
  fluidPage(
    conditionalPanel(
      condition = sprintf("input['landing_page-country'] !== ''&& input['landing_page-indicator_chosen'] !== ''"),
      
      fluidRow(
        column(
          width = 3,
          # Liste dynamique des régions (en supposant la colonne "ADM1_FR")
          selectInput(ns("region"), "Sélectionnez une Région :",
                      choices = c("", sort(unique(regions$ADM1_FR)))),
          selectInput(ns("signif"), "Filtrer par Significativité (ou seuil) :",
                      choices = c("Afficher tout", "Moins de 0.3", "0.3 à 0.6", "Plus de 0.6")),
          helpText("Les données affichées peuvent être filtrées selon la fourchette de valeurs.")
        ),
        column(
          width = 9,
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
              tabPanel(
                title = "Résumé",
                br(),
                tableOutput(ns("resume_table")),
                hr(),
                textOutput(ns("resume_comment"))
              ),
              tabPanel(
                title = "Tableau",
                br(),
                h4("Tableau des Départements"),
                tableOutput(ns("dep_table")),
                h4("Tableau des Communes"),
                tableOutput(ns("com_table")),
                hr(),
                textOutput(ns("table_comment"))
              ),
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
    
    # Titre indicateur
    output$selected_indicator_title <- renderText({
      req(data_reac()$indicator_chosen)
      paste("Indicateur sélectionné :", data_reac()$indicator_chosen)
    })
    
    # Titre région
    output$selected_region_title <- renderText({
      req(input$region)
      paste("Région sélectionnée :", input$region)
    })
    
    # Fonction pour obtenir la plage de filtrage
    getRange <- function(signif_str) {
      if (signif_str == "Moins de 0.3") return(c(-Inf, 0.3))
      if (signif_str == "0.3 à 0.6")   return(c(0.3, 0.6))
      if (signif_str == "Plus de 0.6") return(c(0.6, Inf))
      return(c(-Inf, Inf))  # "Afficher tout"
    }
    
    # Identifier la colonne en fonction de l'indicateur
    chosen_col <- reactive({
      switch(
        data_reac()$indicator_chosen,
        "Taux moyen de Paludisme" = "mean_index",
        "Taux de malaria chez les enfants" = "taux_malaria_enfants",
        "NDVI" = "mean_ndvi",
        NULL
      )
    })
    
    # Vérification si la colonne est valide
    validate_chosen_col <- reactive({
      if (is.null(chosen_col())) {
        showNotification("Aucun indicateur valide n'est sélectionné.", type = "error")
        stop("Colonne choisie est NULL.")
      }
      chosen_col()
    })
    
    # Filtrage des départements
    deps_filtered <- reactive({
      req(input$region)
      req(validate_chosen_col()) # Vérification si une colonne valide est sélectionnée
      minmax <- getRange(input$signif)
      departments %>% 
        filter(ADM1_FR == input$region,
               .data[[chosen_col()]] >= minmax[1],
               .data[[chosen_col()]] <= minmax[2])
    })
    
    # Filtrage des communes
    coms_filtered <- reactive({
      req(input$region)
      req(validate_chosen_col()) # Vérification si une colonne valide est sélectionnée
      minmax <- getRange(input$signif)
      communes %>% 
        filter(ADM1_FR == input$region,
               .data[[chosen_col()]] >= minmax[1],
               .data[[chosen_col()]] <= minmax[2])
    })
    
    # Résumé des données
    output$resume_table <- renderTable({
      req(input$region)
      req(validate_chosen_col()) # Vérification si une colonne valide est sélectionnée
      
      dep_vals <- deps_filtered()[[chosen_col()]]
      com_vals <- coms_filtered()[[chosen_col()]]
      reg_val <- regions %>% 
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
    }, rownames = TRUE)
    
    # Tableau des départements
    output$dep_table <- renderTable({
      req(input$region)
      req(validate_chosen_col()) # Vérification si une colonne valide est sélectionnée
      df <- deps_filtered()
      data.frame(
        Département = df$ADM2_FR,
        Taux = round(df[[chosen_col()]], 3)
      )
    })
    
    # Tableau des communes
    output$com_table <- renderTable({
      req(input$region)
      req(validate_chosen_col()) # Vérification si une colonne valide est sélectionnée
      df <- coms_filtered()
      data.frame(
        Commune = df$ADM3_FR,
        Taux = round(df[[chosen_col()]], 3)
      )
    })
    
    # Graphiques des départements
    output$dep_plot <- renderPlot({
      req(input$region)
      req(validate_chosen_col()) # Vérification si une colonne valide est sélectionnée
      df <- deps_filtered()
      ggplot(df, aes(x = ADM2_FR, y = .data[[chosen_col()]], group = 1)) +
        geom_line(color = "blue") +
        geom_point(color = "blue", size = 2) +
        theme_minimal() +
        labs(title = "Taux par Département", x = "Département", y = "Valeur de l'indicateur") +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
    })
    
    # Graphiques des communes
    output$com_plot <- renderPlot({
      req(input$region)
      req(validate_chosen_col()) # Vérification si une colonne valide est sélectionnée
      df <- coms_filtered()
      ggplot(df, aes(x = ADM3_FR, y = .data[[chosen_col()]], group = 1)) +
        geom_line(color = "red") +
        geom_point(color = "red", size = 2) +
        theme_minimal() +
        labs(title = "Taux par Commune", x = "Commune", y = "Valeur de l'indicateur") +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
    })
    
    output$download_dep_table <- downloadHandler(
      filename = function() {
        paste("table_departements_", input$region, ".csv", sep = "")
      },
      content = function(file) {
        write.csv(deps_filtered(), file, row.names = FALSE)
      }
    )
    
    output$download_com_table <- downloadHandler(
      filename = function() {
        paste("table_communes_", input$region, ".csv", sep = "")
      },
      content = function(file) {
        write.csv(coms_filtered(), file, row.names = FALSE)
      }
    )
    output$download_dep_plot <- downloadHandler(
      filename = function() {
        paste("graphique_departements_", input$region, ".png", sep = "")
      },
      content = function(file) {
        df <- deps_filtered()
        p <- ggplot(df, aes(x = ADM2_FR, y = .data[[chosen_col()]])) +
          geom_line(color = "blue") +
          geom_point(color = "blue") +
          theme_minimal() +
          labs(title = "Graphique par Département", x = "Département", y = "Valeur")
        
        ggsave(file, plot = p, device = "png", width = 10, height = 6)
      }
    )
    
    output$download_com_plot <- downloadHandler(
      filename = function() {
        paste("graphique_communes_", input$region, ".png", sep = "")
      },
      content = function(file) {
        df <- coms_filtered()
        p <- ggplot(df, aes(x = ADM3_FR, y = .data[[chosen_col()]])) +
          geom_line(color = "red") +
          geom_point(color = "red") +
          theme_minimal() +
          labs(title = "Graphique par Commune", x = "Commune", y = "Valeur")
        
        ggsave(file, plot = p, device = "png", width = 10, height = 6)
      }
    )
    
    
    
  })
}