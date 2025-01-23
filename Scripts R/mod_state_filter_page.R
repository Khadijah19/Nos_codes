# ─────────────────────────────────────────────────────────────────────────────
# mod_state_filter_page.R
# Module 4 : Sélection d’une Région (parmi les shapefiles), puis onglets Résumé, Tableau, Graphique
# ─────────────────────────────────────────────────────────────────────────────

mod_state_filter_page_ui <- function(id) {
  ns <- NS(id)
  fluidPage(
    conditionalPanel(
      condition = sprintf("input['landing_page-country'] !== ''&& input['landing_page-indicator'] !== ''"),
      
      fluidRow(
        column(
          width = 3,
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
              div(class = "red-title-box", textOutput(ns("selected_indicator_title"))),
              div(class = "red-title-box", textOutput(ns("selected_region_title"))),
              uiOutput(ns("region_description"))
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

mod_state_filter_page_server <- function(id, landing_inputs, indicator_chosen) {
  moduleServer(id, function(input, output, session) {
    data_reac <- reactive({ landing_inputs() })
    
    output$selected_indicator_title <- renderText({
      req(data_reac()$indicator != "")
      paste("Indicateur sélectionné :", data_reac()$indicator)
    })
    
    output$selected_region_title <- renderText({
      req(input$region != "")
      paste("Région sélectionnée :", input$region)
    })
    
    output$region_description <- renderUI({
      if (data_reac()$indicator == "Taux moyen de Paludisme") {
        "Description : Le Taux moyen de Paludisme est calculé à partir des rasters d'incidence et des données spatiales des régions."
      } else if (data_reac()$indicator == "Taux d'enfants atteints par la malaria") {
        "Description : Cet indicateur estime le pourcentage d'enfants atteints par la malaria en fonction des rasters de population et des taux d'infection."
      } else {
        "Description : Aucune description disponible pour cet indicateur."
      }
    })
    
    getRange <- function(signif_str) {
      if (signif_str == "Moins de 0.3") return(c(-Inf, 0.3))
      if (signif_str == "0.3 à 0.6")   return(c(0.3, 0.6))
      if (signif_str == "Plus de 0.6") return(c(0.6, Inf))
      return(c(-Inf, Inf))
    }
    
    deps_filtered <- reactive({
      req(input$region != "")
      minmax <- getRange(input$signif)
      departments %>%
        filter(ADM1_FR == input$region, mean_index >= minmax[1], mean_index <= minmax[2])
    })
    
    coms_filtered <- reactive({
      req(input$region != "")
      minmax <- getRange(input$signif)
      communes %>%
        filter(ADM1_FR == input$region, mean_index >= minmax[1], mean_index <= minmax[2])
    })
    
    output$resume_table <- renderTable({
      req(input$region != "")
      dep_vals <- deps_filtered()$mean_index
      com_vals <- coms_filtered()$mean_index
      reg_val <- regions %>%
        filter(ADM1_FR == input$region) %>%
        pull(mean_index)
      data.frame(
        row.names = c("Taux moyen", "Maximum", "Minimum"),
        Région = c(round(mean(reg_val), 3), round(max(reg_val), 3), round(min(reg_val), 3)),
        Département = c(round(mean(dep_vals), 3), round(max(dep_vals), 3), round(min(dep_vals), 3)),
        Commune = c(round(mean(com_vals), 3), round(max(com_vals), 3), round(min(com_vals), 3))
      )
    }, rownames = TRUE)
    
    output$dep_table <- renderTable({
      req(input$region != "")
      deps_filtered() %>%
        select(Departement = ADM2_FR, Taux = mean_index) %>%
        mutate(Taux = round(Taux, 3))
    })
    
    output$com_table <- renderTable({
      req(input$region != "")
      coms_filtered() %>%
        select(Commune = ADM3_FR, Taux = mean_index) %>%
        mutate(Taux = round(Taux, 3))
    })
    
    output$dep_plot <- renderPlot({
      req(input$region != "")
      ggplot(deps_filtered(), aes(x = ADM2_FR, y = mean_index, group = 1)) +
        geom_line(color = "blue") +
        geom_point(color = "blue", size = 2) +
        theme_minimal() +
        labs(title = "Taux par Département", x = "Département", y = "Taux")
    })
    
    output$com_plot <- renderPlot({
      req(input$region != "")
      ggplot(coms_filtered(), aes(x = ADM3_FR, y = mean_index, group = 1)) +
        geom_line(color = "red") +
        geom_point(color = "red", size = 2) +
        theme_minimal() +
        labs(title = "Taux par Commune", x = "Commune", y = "Taux")
    })
  })
}
