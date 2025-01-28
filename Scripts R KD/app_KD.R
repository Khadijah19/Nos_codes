# ─────────────────────────────────────────────────────────────────────────────
# app.R
# Fichier principal
# ─────────────────────────────────────────────────────────────────────────────

# 1) Charger les scripts R
source("global_KD.R")                 # global
source("mod_landing_page_KD.R")       # Home
source("mod_indicator_page_KD.R")
source("mod_map_page_KD.R")
source("mod_state_filter_page_KD.R")

# Nouveaux modules statiques
source("mod_about_page_KD.R")         # About
source("mod_guide_page_KD.R")         # Guide
source("mod_notes_techniques_page_KD.R")   # Notes Techniques

# 2) Définition de l'UI : on utilise navbarPage
ui <- navbarPage(
  title = "",
  id    = "main_navbar",
  windowTitle = "Mon Application SSE",  # Ce titre apparaîtra dans l'onglet du navigateur
  
  # Ajout du favicon dans la balise <head>
  header = tagList(
    tags$head(
      # Favicon
      tags$link(rel = "icon", type = "image/jpeg", href = "Famicon.jpeg")
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
  mod_map_page_server("map_page", landing_inputs =  landing_vals, indicator_chosen_)
  
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
