# ─────────────────────────────────────────────────────────────────────────────────
# global.R : Gestion des données multi-pays avec calculs et chargement des rasters
# Utilisation de shapefiles (.shp) et non de RDS
# ─────────────────────────────────────────────────────────────────────────────────

# 1) Chargement des bibliothèques nécessaires
library(shiny)
library(shinyjs)
library(leaflet)
library(dplyr)
library(ggplot2)
library(sf)
library(raster)
library(exactextractr)
library(viridis)

# 2) Définition des indicateurs par pays
fake_indicators <- list(
  "Senegal" = c("Taux moyen de Paludisme", "Taux de malaria chez les enfants","CDI", "NDVI", "NDBI"),
  "Burkina" = c("Taux moyen de Paludisme", "Taux de malaria chez les enfants", "CDI", "NDVI", "NDBI")
)

# 3) Définition du chemin commun pour les shapefiles et rasters
chemin <- "../data"

# 4) Définition des chemins de shapefiles pour chaque pays
shapefiles <- list(
  "Senegal" = list(
    regions = file.path(chemin, "Senegal/Shapefiles_Optimises/regions.shp"),
    departments = file.path(chemin, "Senegal/Shapefiles_Optimises/departments.shp"),
    communes = file.path(chemin, "Senegal/Shapefiles_Optimises/communes.shp")
  ),
  "Burkina" = list(
    regions = file.path(chemin, "Burkina/Shapefiles_Optimises/regions.shp"),
    departments = file.path(chemin, "Burkina/Shapefiles_Optimises/departments.shp"),
    communes = file.path(chemin, "Burkina/Shapefiles_Optimises/communes.shp")
  )
)

# 5) Fonction pour charger les données pour un pays donné
charger_donnees_pays <- function(pays) {
  # Chargement des shapefiles
  regions <- st_read(shapefiles[[pays]]$regions, quiet = TRUE)
  departments <- st_read(shapefiles[[pays]]$departments, quiet = TRUE)
  communes <- st_read(shapefiles[[pays]]$communes, quiet = TRUE)
  
  # Chargement des rasters
  raster_path <- file.path(chemin, pays, "Rasters")
  mean_raster <- raster(file.path(raster_path, "malaria/mean_raster.tif"))
  
  raster_nombre_malaria_enfants <- raster(file.path(raster_path, "malaria_enfants/raster_nombre_malaria_enfants.tif"))
  raster_pop_enfants <- raster(file.path(raster_path, "malaria_enfants/raster_pop_enfants.tif"))
  ndvi_raster <- raster(file.path(raster_path, paste0("Indices_spectraux/NDVI_", pays, ".tif")))
  ndbi_raster <- raster(file.path(raster_path, paste0("Indices_spectraux/NDBI_", pays, ".tif")))
  pop_resampled_binary <- raster(file.path(raster_path, "CDI/pop_resampled_binary.tif"))
  mult_raster <- raster(file.path(raster_path, "CDI/mult_raster.tif"))
  
  list(
    regions = regions,
    departments = departments,
    communes = communes,
    mean_raster = mean_raster,
    raster_nombre_malaria_enfants = raster_nombre_malaria_enfants,
    raster_pop_enfants = raster_pop_enfants,
    ndvi_raster = ndvi_raster,
    ndbi_raster = ndbi_raster,
    pop_resampled_binary = pop_resampled_binary,
    mult_raster = mult_raster
  )
}

# 6) Calculs par niveau administratif
#    (Dans ce cas-ci, on suppose que les indicateurs sont déjà inclus ou calculés ailleurs.
#     Vous pouvez adapter si vous souhaitez calculer dynamiquement dans ce script.)
calculer_indicateurs <- function(donnees) {
  # Les calculs sont supposés déjà faits et inclus dans les shapefiles optimisés.
  # On renvoie simplement les données pour compatibilité.
  
  list(
    regions = donnees$regions,
    departments = donnees$departments,
    communes = donnees$communes,
    mean_raster = donnees$mean_raster,
    raster_nombre_malaria_enfants = donnees$raster_nombre_malaria_enfants,
    raster_pop_enfants = donnees$raster_pop_enfants,
    ndvi_raster = donnees$ndvi_raster,
    ndbi_raster = donnees$ndbi_raster,
    pop_resampled_binary = donnees$pop_resampled_binary,
    mult_raster = donnees$mult_raster
  )
}

# 7) Variables réactives pour stocker les données
data_global <- reactiveValues(
  regions = NULL,
  departments = NULL,
  communes = NULL,
  mean_raster = NULL,
  raster_nombre_malaria_enfants = NULL,
  raster_pop_enfants = NULL,
  ndvi_raster = NULL,
  ndbi_raster = NULL,
  pop_resampled_binary = NULL,
  mult_raster = NULL
)

# 8) Fonction pour mettre à jour les données globales
update_data_global <- function(pays) {
  donnees <- charger_donnees_pays(pays)
  resultats <- calculer_indicateurs(donnees)
  
  data_global$regions <- resultats$regions
  data_global$departments <- resultats$departments
  data_global$communes <- resultats$communes
  data_global$mean_raster <- resultats$mean_raster
  data_global$raster_nombre_malaria_enfants <- resultats$raster_nombre_malaria_enfants
  data_global$raster_pop_enfants <- resultats$raster_pop_enfants
  data_global$ndvi_raster <- resultats$ndvi_raster
  data_global$ndbi_raster <- resultats$ndbi_raster
  data_global$pop_resampled_binary <- resultats$pop_resampled_binary
  data_global$mult_raster <- resultats$mult_raster
  
  message("✅ Données mises à jour pour le pays : ", pays)
}
