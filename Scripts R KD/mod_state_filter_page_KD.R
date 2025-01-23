# ─────────────────────────────────────────────────────────────────────────────
# mod_state_filter_page.R
# Module 4 : Sélection d’une Région, puis onglets Résumé, Tableau, Graphique
# ─────────────────────────────────────────────────────────────────────────────

mod_state_filter_page_ui <- function(id){
  ns <- NS(id)
  fluidPage(
    conditionalPanel(
      condition = sprintf("input['landing_page-country'] !== ''&& input['landing_page-indicator'] !== ''"),
      
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
                plotOutput(ns("com_plot"), height = "300px"),
                hr(),
                textOutput(ns("graph_comment"))
              )
            )
          )
        )
      )
    )
  )
}

mod_state_filter_page_server <- function(id, landing_inputs, indicator_chosen){
  moduleServer(id, function(input, output, session){
    
    data_reac <- reactive({ landing_inputs() })
    
    # Titre indicateur
    output$selected_indicator_title <- renderText({
      req(data_reac()$indicator != "")
      paste("Indicateur sélectionné :", data_reac()$indicator)
    })
    
    # Titre région
    output$selected_region_title <- renderText({
      req(input$region != "")
      paste("Région sélectionnée :", input$region)
    })
    
    # Fonction interne pour gérer la fourchette
    getRange <- function(signif_str){
      if (signif_str == "Moins de 0.3") return(c(-Inf, 0.3))
      if (signif_str == "0.3 à 0.6")   return(c(0.3, 0.6))
      if (signif_str == "Plus de 0.6") return(c(0.6, Inf))
      return(c(-Inf, Inf))  # "Afficher tout"
    }
    
    # Reactive pour connaître la colonne de l'indicateur
    chosen_col <- reactive({
      if (data_reac()$indicator == "Taux moyen de Paludisme") {
        "mean_index"
      } else {
        "taux_malaria"
      }
    })
    
    # Reactive pour subsetter les départements de la région choisie
    deps_filtered <- reactive({
      req(input$region != "")
      minmax <- getRange(input$signif)
      # Filtrer les départements en fonction de la région ET de la fourchette
      departments %>% 
        filter(ADM1_FR == input$region,
               .data[[chosen_col()]] >= minmax[1],
               .data[[chosen_col()]] <= minmax[2])
    })
    
    # Reactive pour subsetter les communes
    coms_filtered <- reactive({
      req(input$region != "")
      minmax <- getRange(input$signif)
      communes %>% 
        filter(ADM1_FR == input$region,
               .data[[chosen_col()]] >= minmax[1],
               .data[[chosen_col()]] <= minmax[2])
    })
    
    # Onglet Résumé
    output$resume_table <- renderTable({
      req(input$region != "")
      
      # On récupère les valeurs pour la région, ses départements et communes
      dep_vals <- deps_filtered()[[chosen_col()]]
      com_vals <- coms_filtered()[[chosen_col()]]
      reg_val <- regions %>% 
        filter(ADM1_FR == input$region) %>% 
        pull(chosen_col())
      
      data.frame(
        row.names = c("Moyenne", "Maximum", "Minimum"),
        "Région"      = c(
          round(mean(reg_val), 3),
          round(max(reg_val), 3),
          round(min(reg_val), 3)
        ),
        "Département" = c(
          round(mean(dep_vals), 3),
          round(max(dep_vals), 3),
          round(min(dep_vals), 3)
        ),
        "Commune"     = c(
          round(mean(com_vals), 3),
          round(max(com_vals), 3),
          round(min(com_vals), 3)
        )
      )
    }, rownames = TRUE)
    
    output$resume_comment <- renderText({
      "Interprétation : Le tableau ci-dessus montre la moyenne, le min et le max pour la région sélectionnée, 
       ainsi que pour les départements et les communes qui la composent (selon la fourchette de filtrage)."
    })
    
    # Onglet Tableau
    output$dep_table <- renderTable({
      req(input$region != "")
      df <- deps_filtered()
      data.frame(
        Departement = df$ADM2_FR,
        Taux = round(df[[chosen_col()]], 3)
      )
    })
    
    output$com_table <- renderTable({
      req(input$region != "")
      df <- coms_filtered()
      data.frame(
        Commune = df$ADM3_FR,
        Taux = round(df[[chosen_col()]], 3)
      )
    })
    
    output$table_comment <- renderText({
      "Interprétation : Ce tableau liste les Départements et Communes (filtrés selon la fourchette de significativité) 
       et leurs valeurs pour l'indicateur choisi."
    })
    
    # Onglet Graphique
    output$dep_plot <- renderPlot({
      req(input$region != "")
      df <- deps_filtered()
      ggplot(df, aes(x = ADM2_FR, y = .data[[chosen_col()]], group = 1)) +
        geom_line(color = "blue") +
        geom_point(color = "blue", size = 2) +
        theme_minimal() +
        labs(title = "Taux par Département", x = "Département", y = "Valeur de l'indicateur") +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
    })
    
    output$com_plot <- renderPlot({
      req(input$region != "")
      df <- coms_filtered()
      ggplot(df, aes(x = ADM3_FR, y = .data[[chosen_col()]], group = 1)) +
        geom_line(color = "red") +
        geom_point(color = "red", size = 2) +
        theme_minimal() +
        labs(title = "Taux par Commune", x = "Commune", y = "Valeur de l'indicateur") +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
    })
    
    output$graph_comment <- renderText({
      "Interprétation : Les graphiques ci-dessus illustrent la valeur de l'indicateur 
       dans les départements et communes (filtrés) de la région sélectionnée."
    })
  })
}
