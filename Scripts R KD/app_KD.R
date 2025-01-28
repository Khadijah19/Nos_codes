# ─────────────────────────────────────────────────────────────────────────────
# app.R
# Fichier principal qui assemble tout
# ─────────────────────────────────────────────────────────────────────────────

source("global_KD.R")
source("mod_landing_page_KD.R")
source("mod_indicator_page_KD.R")
source("mod_map_page_KD.R")
source("mod_state_filter_page_KD.R")

ui <- fluidPage(
  useShinyjs(),
  
  # Module 1 : Landing page (toujours visible)
  mod_landing_page_ui("landing_page"),
  
  # Module 2 : Indicateur (visible après la sélection d'un pays & indicateur)
  conditionalPanel(
    condition = "input['landing_page-country'] !== '' && input['landing_page-indicator_chosen'] !== ''",
    mod_indicator_page_ui("indicator_page")
  ),
  
  # Module 3 : Carte interactive (visible après la sélection d'un indicateur)
  conditionalPanel(
    condition = "input['landing_page-country'] !== '' && input['landing_page-indicator_chosen'] !== ''",
    mod_map_page_ui("map_page")
  ),
  
  # Module 4 : Analyse par région (visible après la sélection d'un indicateur)
  conditionalPanel(
      condition = "input['landing_page-country'] !== '' && input['landing_page-indicator_chosen'] !== ''",
      mod_state_filter_page_ui("state_filter_page")
    
  )
) 

server <- function(input, output, session) {
  # 1) Landing
  landing_vals <- mod_landing_page_server("landing_page")
  
  # 2) Indicateur page
  indicator_chosen_ <- mod_indicator_page_server("indicator_page", landing_vals)
  
  # 3) Carte
  mod_map_page_server("map_page", landing_inputs=landing_vals, indicator_chosen_)
  
  # 4) State filter page
  mod_state_filter_page_server("state_filter_page",landing_inputs= landing_vals, indicator_chosen_)
  
  # Synchronisation JS (optionnel)
  observe({
    chosen_bool <- tolower(as.character(indicator_chosen_()))
    runjs(sprintf("Shiny.setInputValue('%s', %s)", "indicator_chosen_", chosen_bool))
  })
}

shinyApp(ui = ui, server = server)
