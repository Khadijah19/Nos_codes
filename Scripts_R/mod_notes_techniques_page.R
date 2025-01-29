# ─────────────────────────────────────────────────────────────────────────────
# mod_notes_techniques_page.R
# Page "Notes Techniques" avec titre principal en style "guide-title"
# (boutons rouges, toggles pour afficher les détails)
# ─────────────────────────────────────────────────────────────────────────────

mod_notes_techniques_page_ui <- function(id) {
  ns <- NS(id)
  fluidPage(
    useShinyjs(),
    
    tags$head(tags$style(HTML("
      /* Style principal du titre (similaire à guide-title) */
      .notes-title {
        background-color: #cc0000;
        color: white;
        padding: 15px;
        border-radius: 8px;
        font-weight: bold;
        font-size: 24px;
        text-align: center;
        margin-bottom: 20px;
      }
      .notes-container {
        background-color: #fafafa;
        border-radius: 8px;
        padding: 20px;
        box-shadow: 0 2px 8px rgba(0,0,0,0.1);
        margin-bottom: 20px;
      }
      .notes-section {
        background-color: #ffffff;
        border: 1px solid #ddd;
        border-radius: 6px;
        margin-bottom: 15px;
        padding: 15px;
      }
      .notes-section h4 {
        font-size: 18px;
        margin-top: 0;
        color: #666;
      }
      .notes-section p, .notes-section li {
        line-height: 1.6;
        color: #333;
      }
      .btn-danger {
        background-color: #cc0000 !important;
        border-color: #b30000 !important;
      }
      .notes-details {
        margin-top: 10px;
      }
    "))),
    
    # Titre principal en style "notes-title"
    div(class = "notes-title", "Notes Techniques"),
    
    div(
      class = "notes-container",
      
      # Section 1 : Taux moyen de malaria ...
      div(
        class = "notes-section",
        actionButton(ns("btn1"), 
                     label = "Taux moyen de malaria entre 2000 et 2022", 
                     class = "btn btn-danger"),
        hidden(
          div(id = ns("details1"), class = "notes-details",
              tags$ul(
                tags$li("Ce qu'il faut savoir"),
                tags$p("Le taux moyen de malaria, calculé sur une période de 22 ans (2000-2022)...")
              ),
              tags$ul(
                tags$li("Derrière les chiffres : comment calculer cet indicateur ?"),
                tags$p("Pour une compréhension de la méthodologie de calcul, vous pouvez consulter le TP4..."),
                tags$a("Lien vers TP4", 
                       href="https://github.com/Abson-dev/Statistique-Exploratoire-Spatiale/tree/main/TP4", 
                       target="_blank")
              ),
              tags$ul(
                tags$li("En savoir plus"),
                tags$p("Le Malaria Atlas Project (MAP) est une initiative qui compile et fournit des données..."),
                tags$a("CRAN: Package malariaAtlas", 
                       href="https://cran.r-project.org/web/packages/malariaAtlas/index.html", 
                       target="_blank"),
                tags$p("Pour plus d'informations sur le Malaria Atlas Project..."),
                tags$a("Malaria Atlas Project | Home", 
                       href="https://malariaatlas.org/", 
                       target="_blank")
              )
          )
        )
      ),
      
      # Section 2 : Taux de malaria chez les enfants ...
      div(
        class = "notes-section",
        actionButton(ns("btn2"), 
                     label = "Taux de malaria chez les enfants en 2022", 
                     class = "btn btn-danger"),
        hidden(
          div(id = ns("details2"), class = "notes-details",
              tags$ul(
                tags$li("Ce qu'il faut savoir"),
                tags$p("Cet indicateur mesure la prévalence de la malaria chez les enfants de moins de 5 ans en 2022...")
              ),
              tags$ul(
                tags$li("Derrière les chiffres : comment calculer cet indicateur ?"),
                tags$p("Pour une compréhension de la méthodologie de calcul, vous pouvez consulter le TP5..."),
                tags$a("Lien vers TP5", 
                       href="https://github.com/Abson-dev/Statistique-Exploratoire-Spatiale/tree/main/TP5", 
                       target="_blank")
              ),
              tags$ul(
                tags$li("En savoir plus : Saviez-vous qu’il existe un journal scientifique dédié au paludisme ?"),
                tags$p("Le Malaria Journal est une source précieuse pour les chercheurs et décideurs..."),
                tags$a("Malaria Journal - Estimated distribution...",
                       href="https://malariajournal.biomedcentral.com/articles/10.1186/s12936-023-04811-z",
                       target="_blank")
              )
          )
        )
      ),
      
      # Section 3 : Conflict Diffusion Indicator
      div(
        class = "notes-section",
        actionButton(ns("btn3"), 
                     label = "Conflict Diffusion Indicator", 
                     class = "btn btn-danger"),
        hidden(
          div(id = ns("details3"), class = "notes-details",
              tags$ul(
                tags$li("Ce qu'il faut savoir"),
                tags$p("Le Conflict Diffusion Indicator (CDI) mesure l’extension géographique et temporelle...")
              ),
              tags$ul(
                tags$li("Derrière les chiffres : comment calculer cet indicateur ?"),
                tags$p("Pour une compréhension de la méthodologie, vous pouvez consulter le TP6..."),
                tags$a("Lien vers TP6",
                       href="https://github.com/Abson-dev/Statistique-Exploratoire-Spatiale/tree/main/TP6",
                       target="_blank")
              ),
              tags$ul(
                tags$li("En savoir plus: « Bringing clarity to crisis »"),
                tags$p("L’Armed Conflict Location & Event Data Project (ACLED) constitue une ressource essentielle..."),
                tags$a("ACLED",
                       href="https://acleddata.com/",
                       target="_blank")
              )
          )
        )
      ),
      
      # Section 4 : NDVI
      div(
        class = "notes-section",
        actionButton(ns("btn4"), 
                     label = "NDVI (Normalized Difference Vegetation Index)", 
                     class = "btn btn-danger"),
        hidden(
          div(id = ns("details4"), class = "notes-details",
              tags$ul(
                tags$li("Ce qu'il faut savoir"),
                tags$p("Le NDVI est un indicateur spectral clé en télédétection...")
              ),
              tags$ul(
                tags$li("Derrière les chiffres : comment calculer cet indicateur ?"),
                tags$p("Pour la méthodologie de calcul, voir le TP10..."),
                tags$a("Lien vers TP10", 
                       href="https://github.com/Abson-dev/Statistique-Exploratoire-Spatiale/tree/main/TP10",
                       target="_blank")
              ),
              tags$ul(
                tags$li("En savoir plus : Exploration du NDVI"),
                tags$p("Le NDVI est calculé en utilisant la réflectance mesurée dans deux bandes..."),
                tags$a("NDVI (indice de végétation...) - METER Group",
                       href="https://metergroup.com/fr/education-guides/ndvi-and-pri-the-researchers-complete-guide/", 
                       target="_blank")
              )
          )
        )
      ),
      
      # Section 5 : NDBI
      div(
        class = "notes-section",
        actionButton(ns("btn5"), 
                     label = "NDBI (Normalized Difference Built-up Index)",
                     class = "btn btn-danger"),
        hidden(
          div(id = ns("details5"), class = "notes-details",
              tags$ul(
                tags$li("Ce qu'il faut savoir"),
                tags$p("L'Indice Différentiel Normalisé des Zones Bâties (NDBI)...")
              ),
              tags$ul(
                tags$li("Derrière les chiffres : comment calculer cet indicateur ?"),
                tags$p("Pour la méthodologie, consultez le TP10..."),
                tags$a("Lien vers TP10",
                       href="https://github.com/Abson-dev/Statistique-Exploratoire-Spatiale/tree/main/TP10",
                       target="_blank")
              ),
              tags$ul(
                tags$li("En savoir plus : les avantages et inconvénients du NDBI"),
                tags$p("Le NDBI est un outil efficace pour cartographier les zones urbaines...")
              )
          )
        )
      )
    )
  )
}

mod_notes_techniques_page_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    # Gestion des toggles : affichage/masquage des blocs
    observeEvent(input$btn1, { toggle("details1") })
    observeEvent(input$btn2, { toggle("details2") })
    observeEvent(input$btn3, { toggle("details3") })
    observeEvent(input$btn4, { toggle("details4") })
    observeEvent(input$btn5, { toggle("details5") })
    
  })
}
