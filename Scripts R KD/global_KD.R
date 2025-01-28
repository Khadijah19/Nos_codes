
# ─────────────────────────────────────────────────────────────────────────────────
# global.R : Gestion des données multi-pays avec calculs et chargement des rasters
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

# 3) Définition des fichiers shapefile par pays
shapefile_noms <- list(
  "Senegal" = list(
    adm0 = "sen_admbnda_adm0_anat_20240520.shp",
    adm1 = "sen_admbnda_adm1_anat_20240520.shp",
    adm2 = "sen_admbnda_adm2_anat_20240520.shp",
    adm3 = "sen_admbnda_adm3_anat_20240520.shp"
  ),
  "Burkina" = list(
    adm0 = "bfa_admbnda_adm0_igb_20200323.shp",
    adm1 = "bfa_admbnda_adm1_igb_20200323.shp",
    adm2 = "bfa_admbnda_adm2_igb_20200323.shp",
    adm3 = "bfa_admbnda_adm3_igb_20200323.shp"
  )
)

chemin_base <- "C:/Users/HP/OneDrive/Documents/GitHub/Projet_Final_Stat_Spatiale/data"

# 4) Fonction pour charger les données pour un pays donné
charger_donnees_pays <- function(pays) {
  shapefile_path <- file.path(chemin_base, pays, "Shapefiles")
  raster_path <- file.path(chemin_base, pays, "Rasters")
  
  noms <- shapefile_noms[[pays]]
  
  # Chargement des shapefiles
  regions <- st_read(file.path(shapefile_path, noms$adm1), quiet = TRUE)
  departments <- st_read(file.path(shapefile_path, noms$adm2), quiet = TRUE)
  communes <- st_read(file.path(shapefile_path, noms$adm3), quiet = TRUE)
  
  # Chargement des rasters
  #Chargement des rasters en batch
  raster_path <- file.path(chemin_base,pays, "Rasters")
  fichiers_raster <- list.files(raster_path, pattern = "\\.tiff$", full.names = TRUE)
  rasters <- stack(fichiers_raster)
  
  #Calcul du raster moyen (mean_raster)
  mean_raster <- calc(rasters, fun = mean, na.rm = TRUE)
  
  raster_nombre_malaria_enfants <- raster(file.path(raster_path, "malaria_enfants/raster_nombre_malaria_enfants.tif"))
  raster_pop_enfants <- raster(file.path(raster_path, "malaria_enfants/raster_pop_enfants.tif"))
  ndvi_raster <- raster(file.path(raster_path, paste0("Indices_spectraux/NDVI_", pays, ".tif")))
  ndbi_raster<- raster(file.path(raster_path, paste0("Indices_spectraux/NDBI_", pays, ".tif")))
  pop_resampled_binary<- raster(file.path(raster_path,"CDI/pop_resampled_binary.tif"))
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

# 5) Calculs par niveau administratif
calculer_indicateurs <- function(donnees) {
  regions <- donnees$regions
  departments <- donnees$departments
  communes <- donnees$communes
  mean_raster <- donnees$mean_raster
  raster_nombre_malaria_enfants <- donnees$raster_nombre_malaria_enfants
  raster_pop_enfants <- donnees$raster_pop_enfants
  ndvi_raster <- donnees$ndvi_raster
  ndbi_raster <- donnees$ndbi_raster
  pop_resampled_binary <- donnees$pop_resampled_binary
  mult_raster <- donnees$mult_raster
  
  # Calculs pour les niveaux administratifs
  regions$mean_index <- exact_extract(mean_raster, regions, 'mean')
  regions$mean_ndvi <- exact_extract(ndvi_raster, regions, 'mean')
  regions$mean_ndbi <- exact_extract(ndbi_raster, regions, 'mean')
  regions$taux_malaria_enfants <- ifelse(
    exact_extract(raster_pop_enfants, regions, 'sum') > 0,
    exact_extract(raster_nombre_malaria_enfants, regions, 'sum') / exact_extract(raster_pop_enfants, regions, 'sum'),
    NA
  )
  
  departments$mean_index <- exact_extract(mean_raster, departments, 'mean')
  departments$mean_ndvi <- exact_extract(ndvi_raster, departments, 'mean')
  departments$mean_ndbi <- exact_extract(ndbi_raster, departments, 'mean')
  departments$taux_malaria_enfants <- ifelse(
    exact_extract(raster_pop_enfants, departments, 'sum') > 0,
    exact_extract(raster_nombre_malaria_enfants, departments, 'sum') / exact_extract(raster_pop_enfants, departments, 'sum'),
    NA
  )
  
  communes$mean_index <- exact_extract(mean_raster, communes, 'mean')
  communes$mean_ndvi <- exact_extract(ndvi_raster, communes, 'mean')
  communes$mean_ndbi <- exact_extract(ndbi_raster, communes, 'mean')
  communes$taux_malaria_enfants <- ifelse(
    exact_extract(raster_pop_enfants, communes, 'sum') > 0,
    exact_extract(raster_nombre_malaria_enfants, communes, 'sum') / exact_extract(raster_pop_enfants, communes, 'sum'),
    NA
  )
  
  # Calcul CDI pour chaque niveau
  pop_count_regions <- exact_extract(pop_resampled_binary, regions, 'sum')
  prod_count_regions <- exact_extract(mult_raster, regions, 'sum')
  regions$CDI <- ifelse(pop_count_regions > 0, prod_count_regions / pop_count_regions, NA)
  
  pop_count_departments <- exact_extract(pop_resampled_binary, departments, 'sum')
  prod_count_departments <- exact_extract(mult_raster, departments, 'sum')
  departments$CDI <- ifelse(pop_count_departments > 0, prod_count_departments / pop_count_departments, NA)
  
  pop_count_communes <- exact_extract(pop_resampled_binary, communes, 'sum')
  prod_count_communes <- exact_extract(mult_raster, communes, 'sum')
  communes$CDI <- ifelse(pop_count_communes > 0, prod_count_communes / pop_count_communes, NA)
  
  
  list(
    regions = regions,
    departments = departments,
    communes = communes
  )
}

# 6) Variables réactives pour stocker les données
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

# 7) Fonction pour mettre à jour les données globales
update_data_global <- function(pays) {
  donnees <- charger_donnees_pays(pays)
  resultats <- calculer_indicateurs(donnees)
  
  data_global$regions <- resultats$regions
  data_global$departments <- resultats$departments
  data_global$communes <- resultats$communes
  data_global$mean_raster <- donnees$mean_raster
  data_global$raster_nombre_malaria_enfants <- donnees$raster_nombre_malaria_enfants
  data_global$raster_pop_enfants <- donnees$raster_pop_enfants
  data_global$ndvi_raster <- donnees$ndvi_raster
  data_global$ndbi_raster <- donnees$ndbi_raster
  data_global$pop_resampled_binary <- donnees$pop_resampled_binary
  data_global$mult_raster <- donnees$mult_raster
  message("Données mises à jour pour le pays : ", pays)
}


# ─────────────────────────────────────────────────────────────────────────────────
