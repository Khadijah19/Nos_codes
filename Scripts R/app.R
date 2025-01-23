# ─────────────────────────────────────────────────────────────────────────────
# app.R
# Fichier principal qui assemble tout
# ─────────────────────────────────────────────────────────────────────────────

source("global.R")
source("mod_landing_page.R")
source("mod_indicator_page.R")
source("mod_map_page.R")
source("mod_state_filter_page.R")

ui <- fluidPage(
  useShinyjs(),
  
  # Module 1 : Landing page (toujours visible)
  mod_landing_page_ui("landing_page"),
  
  # Module 2 : Indicateur (visible après la sélection d'un pays & indicateur)
  conditionalPanel(
    condition = "input['landing_page-country'] !== '' && input['landing_page-indicator'] !== ''",
    mod_indicator_page_ui("indicator_page")
  ),
  
  # Module 3 : Carte interactive (visible après la sélection d'un indicateur)
  conditionalPanel(
    condition = "input['landing_page-country'] !== '' && input['landing_page-indicator'] !== ''",
    mod_map_page_ui("map_page")
  ),
  
  # Module 4 : Analyse par région (visible après la sélection d'un indicateur)
  conditionalPanel(
    condition = "input['landing_page-country'] !== '' && input['landing_page-indicator'] !== ''",
    mod_state_filter_page_ui("state_filter_page")
  )
)

server <- function(input, output, session) {
  # 1) Landing
  landing_vals <- mod_landing_page_server("landing_page")
  
  # 2) Indicateur page
  indicator_chosen <- mod_indicator_page_server("indicator_page", landing_vals)
  
  # 3) Carte
  mod_map_page_server("map_page", landing_vals, indicator_chosen)
  
  # 4) State filter page
  mod_state_filter_page_server("state_filter_page", landing_vals, indicator_chosen)
  
  # Synchronisation JS (optionnel)
  observe({
    chosen_bool <- tolower(as.character(indicator_chosen()))
    runjs(sprintf("Shiny.setInputValue('%s', %s)", "indicator_chosen", chosen_bool))
  })
}

shinyApp(ui = ui, server = server)
