# ─────────────────────────────────────────────────────────────────────────────
# mod_state_filter_page.R
# Module 4 : Sélection d’une Région, puis onglets Résumé, Tableau, Graphique
# + Téléchargements CSV / PNG
# ─────────────────────────────────────────────────────────────────────────────

mod_state_filter_page_ui <- function(id){
  ns <- NS(id)
  fluidPage(
    conditionalPanel(
      condition = sprintf("input['landing_page-country'] !== '' && input['landing_page-indicator'] !== ''"),
      
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
              
              # ─────────────────────────────────────────
              # Onglet Résumé
              # ─────────────────────────────────────────
              tabPanel(
                title = "Résumé",
                br(),
                # Ajout du bouton de téléchargement CSV pour le tableau de résumé
                downloadButton(ns("dl_resume_table"), "Télécharger le résumé (CSV)"),
                br(), br(),
                tableOutput(ns("resume_table")),
                hr(),
                textOutput(ns("resume_comment"))
              ),
              
              # ─────────────────────────────────────────
              # Onglet Tableau
              # ─────────────────────────────────────────
              tabPanel(
                title = "Tableau",
                br(),
                fluidRow(
                  column(
                    width = 6,
                    h4("Tableau des Départements"),
                    downloadButton(ns("dl_dep_table"), "Télécharger (CSV)"),
                    tableOutput(ns("dep_table"))
                  ),
                  column(
                    width = 6,
                    h4("Tableau des Communes"),
                    downloadButton(ns("dl_com_table"), "Télécharger (CSV)"),
                    tableOutput(ns("com_table"))
                  )
                ),
                hr(),
                textOutput(ns("table_comment"))
              ),
              
              # ─────────────────────────────────────────
              # Onglet Graphique
              # ─────────────────────────────────────────
              tabPanel(
                title = "Graphique",
                br(),
                fluidRow(
                  column(
                    width = 6,
                    h4("Graphique par Département"),
                    # Bouton pour télécharger le plot
                    downloadButton(ns("dl_dep_plot"), "Télécharger (PNG)"),
                    plotOutput(ns("dep_plot"), height = "300px")
                  ),
                  column(
                    width = 6,
                    h4("Graphique par Commune"),
                    downloadButton(ns("dl_com_plot"), "Télécharger (PNG)"),
                    plotOutput(ns("com_plot"), height = "300px")
                  )
                ),
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
    
    # Reactive pour subsetter départements
    deps_filtered <- reactive({
      req(input$region != "")
      minmax <- getRange(input$signif)
      departments %>% 
        filter(
          ADM1_FR == input$region,
          .data[[chosen_col()]] >= minmax[1],
          .data[[chosen_col()]] <= minmax[2]
        )
    })
    
    # Reactive pour subsetter communes
    coms_filtered <- reactive({
      req(input$region != "")
      minmax <- getRange(input$signif)
      communes %>% 
        filter(
          ADM1_FR == input$region,
          .data[[chosen_col()]] >= minmax[1],
          .data[[chosen_col()]] <= minmax[2]
        )
    })
    
    # ─────────────────────────────────────────
    # Onglet "Résumé" : Tableau + Download
    # ─────────────────────────────────────────
    # On encapsule le calcul du tableau résumé dans une reactive
    summary_table_reactive <- reactive({
      req(input$region != "")
      
      dep_vals <- deps_filtered()[[chosen_col()]]
      com_vals <- coms_filtered()[[chosen_col()]]
      
      # Valeur unique de la région
      reg_val <- regions %>% 
        filter(ADM1_FR == input$region) %>% 
        pull(chosen_col())
      
      # Construction du tableau (data.frame)
      df <- data.frame(
        row.names = c("Moyenne", "Maximum", "Minimum"),
        "Région" = c(
          round(mean(reg_val), 3),
          round(max(reg_val), 3),
          round(min(reg_val), 3)
        ),
        "Département" = c(
          round(mean(dep_vals), 3),
          round(max(dep_vals), 3),
          round(min(dep_vals), 3)
        ),
        "Commune" = c(
          round(mean(com_vals), 3),
          round(max(com_vals), 3),
          round(min(com_vals), 3)
        )
      )
      df
    })
    
    # Affichage du tableau résumé
    output$resume_table <- renderTable({
      summary_table_reactive()
    }, rownames = TRUE)
    
    output$resume_comment <- renderText({
      "Interprétation : Le tableau ci-dessus montre la moyenne, le min et le max pour la région sélectionnée, 
       ainsi que pour les départements et les communes qui la composent (selon la fourchette de filtrage)."
    })
    
    # Bouton de téléchargement du résumé (CSV)
    output$dl_resume_table <- downloadHandler(
      filename = function(){
        paste0("resume_table_", gsub(" ", "_", input$region), "_", Sys.Date(), ".csv")
      },
      content = function(file){
        write.csv(summary_table_reactive(), file, row.names = TRUE)
      }
    )
    
    # ─────────────────────────────────────────
    # Onglet "Tableau" : Départements / Communes + Download
    # ─────────────────────────────────────────
    dep_table_reactive <- reactive({
      req(input$region != "")
      df <- deps_filtered()
      data.frame(
        Departement = df$ADM2_FR,
        Taux = round(df[[chosen_col()]], 3)
      )
    })
    
    com_table_reactive <- reactive({
      req(input$region != "")
      df <- coms_filtered()
      data.frame(
        Commune = df$ADM3_FR,
        Taux = round(df[[chosen_col()]], 3)
      )
    })
    
    output$dep_table <- renderTable({
      dep_table_reactive()
    })
    
    output$com_table <- renderTable({
      com_table_reactive()
    })
    
    output$table_comment <- renderText({
      "Interprétation : Ce tableau liste les Départements et Communes (filtrés selon la fourchette de significativité) 
       et leurs valeurs pour l'indicateur choisi."
    })
    
    # Téléchargement CSV Département
    output$dl_dep_table <- downloadHandler(
      filename = function(){
        paste0("departements_", gsub(" ", "_", input$region), "_", Sys.Date(), ".csv")
      },
      content = function(file){
        write.csv(dep_table_reactive(), file, row.names = FALSE)
      }
    )
    
    # Téléchargement CSV Commune
    output$dl_com_table <- downloadHandler(
      filename = function(){
        paste0("communes_", gsub(" ", "_", input$region), "_", Sys.Date(), ".csv")
      },
      content = function(file){
        write.csv(com_table_reactive(), file, row.names = FALSE)
      }
    )
    
    # ─────────────────────────────────────────
    # Onglet "Graphique" : Département / Commune + Download
    # ─────────────────────────────────────────
    dep_plot_reactive <- reactive({
      req(input$region != "")
      df <- deps_filtered()
      ggplot(df, aes(x = ADM2_FR, y = .data[[chosen_col()]], group = 1)) +
        geom_line(color = "blue") +
        geom_point(color = "blue", size = 2) +
        theme_minimal() +
        labs(title = "Taux par Département", x = "Département", y = "Valeur de l'indicateur") +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
    })
    
    com_plot_reactive <- reactive({
      req(input$region != "")
      df <- coms_filtered()
      ggplot(df, aes(x = ADM3_FR, y = .data[[chosen_col()]], group = 1)) +
        geom_line(color = "red") +
        geom_point(color = "red", size = 2) +
        theme_minimal() +
        labs(title = "Taux par Commune", x = "Commune", y = "Valeur de l'indicateur") +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
    })
    
    output$dep_plot <- renderPlot({
      dep_plot_reactive()
    })
    
    output$com_plot <- renderPlot({
      com_plot_reactive()
    })
    
    output$graph_comment <- renderText({
      "Interprétation : Les graphiques ci-dessus illustrent la valeur de l'indicateur 
       dans les départements et communes (filtrés) de la région sélectionnée."
    })
    
    # Téléchargement du plot Départements (PNG)
    output$dl_dep_plot <- downloadHandler(
      filename = function(){
        paste0("plot_departements_", gsub(" ", "_", input$region), "_", Sys.Date(), ".png")
      },
      content = function(file){
        ggsave(
          filename = file,
          plot = dep_plot_reactive(), 
          device = "png", width = 8, height = 5
        )
      }
    )
    
    # Téléchargement du plot Communes (PNG)
    output$dl_com_plot <- downloadHandler(
      filename = function(){
        paste0("plot_communes_", gsub(" ", "_", input$region), "_", Sys.Date(), ".png")
      },
      content = function(file){
        ggsave(
          filename = file,
          plot = com_plot_reactive(),
          device = "png", width = 8, height = 5
        )
      }
    )
    
  })
}
