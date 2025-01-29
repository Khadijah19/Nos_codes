# ─────────────────────────────────────────────────────────────────────────────
# app.R
# Fichier principal 
# ─────────────────────────────────────────────────────────────────────────────

# 1) Charger les scripts R
source("Scripts_R/global.R")                 
source("Scripts_R/mod_landing_page.R")       
source("Scripts_R/mod_indicator_page.R")
source("Scripts_R/mod_map_page.R")
source("Scripts_R/mod_state_filter_page.R")

# Nouveaux modules statiques
source("Scripts_R/mod_about_page.R")         
source("Scripts_R/mod_guide_page.R")         
source("Scripts_R/mod_notes_techniques_page.R")  

# 2) Définition de l'UI : on utilise navbarPage
ui <- navbarPage(
  title       = "",
  id          = "main_navbar",
  windowTitle = "Application_SES",  
  
  # Ajout du favicon dans la balise <head>
  header = tagList(
    tags$head(
      # Favicon - notez que "Famicon.png" doit être dans www/
      tags$link(
        rel  = "icon", 
        type = "image/png", 
        href = "Famicon.png"
      )
    )
  ),
  
  # --- Onglet 1 : HOME ---
  tabPanel(
    title = "Home",
    icon  = icon("home"),
    fluidPage(
      useShinyjs(),  
      
      # Module 1 : Landing page (toujours visible)
      mod_landing_page_ui("landing_page"),
      
      # Module 2 : Indicateur (visible après sélection)
      conditionalPanel(
        condition = "input['landing_page-country'] !== '' && input['landing_page-indicator'] !== ''",
        mod_indicator_page_ui("indicator_page")
      ),
      
      # Module 3 : Carte interactive
      conditionalPanel(
        condition = "input['landing_page-country'] !== '' && input['landing_page-indicator'] !== ''",
        mod_map_page_ui("map_page")
      ),
      
      # Module 4 : Analyse par région
      conditionalPanel(
        condition = "input['landing_page-country'] !== '' && input['landing_page-indicator'] !== ''",
        mod_state_filter_page_ui("state_filter_page")
      )
    )
  ),
  
  # --- Onglet 2 : ABOUT ---
  tabPanel(
    title = "About",
    icon  = icon("info-circle"),
    mod_about_page_ui("about_page")
  ),
  
  # --- Onglet 3 : GUIDE ---
  tabPanel(
    title = "Guide",
    icon  = icon("book-open"),
    mod_guide_page_ui("guide_page")
  ),
  
  # --- Onglet 4 : NOTES TECHNIQUES ---
  tabPanel(
    title = "Notes Techniques",
    icon  = icon("file-alt"),
    mod_notes_techniques_page_ui("notes_techniques_page")
  )
)

# 3) Définition du server
server <- function(input, output, session) {
  
  # 3.1) Landing
  landing_vals <- mod_landing_page_server("landing_page")
  
  # 3.2) Indicateur page
  indicator_chosen_ <- mod_indicator_page_server("indicator_page", landing_vals)
  
  # 3.3) Carte
  mod_map_page_server("map_page", landing_inputs = landing_vals, indicator_chosen_)
  
  # 3.4) State filter
  mod_state_filter_page_server("state_filter_page", landing_inputs = landing_vals, indicator_chosen_)
  
  # 3.5) Pages statiques
  mod_about_page_server("about_page")
  mod_guide_page_server("guide_page")
  mod_notes_techniques_page_server("notes_techniques_page")
  
  # 3.6) Synchronisation JS (optionnel)
  observe({
    chosen_bool <- tolower(as.character(indicator_chosen_()))
    runjs(sprintf("Shiny.setInputValue('%s', %s)", "indicator_chosen_", chosen_bool))
  })
}

# 4) Lancement de l'application
shinyApp(ui = ui, server = server)
