# ─────────────────────────────────────────────────────────────────────────────
# global_KD.R
# Chargement des librairies, définition des indicateurs,
# et gestion multi-pays (Sénégal/Burkina) avec calculs par niveau admin.
# ─────────────────────────────────────────────────────────────────────────────

# 1) Chargement des bibliothèques
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
  "Senegal" = c("Taux moyen de Paludisme", "Taux de malaria chez les enfants", "NDVI"),
  "Burkina" = c("Taux moyen de Paludisme", "Taux de malaria chez les enfants", "NDVI")
)

# 3) Paramétrage des chemins de shapefiles/rasters par pays
#    Adaptez à votre propre organisation des dossiers !
chemin_base <- "C:/Users/ALIOUNE KANE/Downloads/Projet_Final_Stats_Spatiales/data"
shapefile_noms <- list(
  "Senegal" = list(
    adm1 = "sen_admbnda_adm1_anat_20240520.shp",
    adm2 = "sen_admbnda_adm2_anat_20240520.shp",
    adm3 = "sen_admbnda_adm3_anat_20240520.shp"
  ),
  "Burkina" = list(
    adm1 = "bfa_admbnda_adm1_igb_20200323.shp",
    adm2 = "bfa_admbnda_adm2_igb_20200323.shp",
    adm3 = "bfa_admbnda_adm3_igb_20200323.shp"
  )
)

# 4) Fonction pour charger les shapefiles/rasters d’un pays
charger_donnees_pays <- function(pays) {
  # Shapefiles
  shapefile_path <- file.path(chemin_base, pays, "Shapefiles")
  noms <- shapefile_noms[[pays]]
  
  regions     <- st_read(file.path(shapefile_path, noms$adm1), quiet = TRUE)
  departments <- st_read(file.path(shapefile_path, noms$adm2), quiet = TRUE)
  communes    <- st_read(file.path(shapefile_path, noms$adm3), quiet = TRUE)
  
  # Rasters
  raster_path <- file.path(chemin_base, pays, "Rasters")
  fichiers_raster <- list.files(raster_path, pattern = "\\.tiff$", full.names = TRUE)
  
  # On stack tous les .tiff
  if (length(fichiers_raster) > 0) {
    rasters <- stack(fichiers_raster)
    mean_raster <- calc(rasters, fun = mean, na.rm = TRUE)
  } else {
    mean_raster <- NULL
  }
  
  # EXEMPLE : rasters additionnels
  # À adapter selon vos vrais fichiers
  raster_nombre_malaria_enfants <- raster(file.path(raster_path, "malaria_enfants/raster_nombre_malaria_enfants.tif"))
  raster_pop_enfants <- raster(file.path(raster_path, "malaria_enfants/raster_pop_enfants.tif"))
  ndvi_raster <- raster(file.path(raster_path, paste0("Indices_spectraux/NDVI_", pays, ".tif")))
  
  list(
    regions = regions,
    departments = departments,
    communes = communes,
    mean_raster = mean_raster,
    raster_nombre_malaria_enfants = raster_nombre_malaria_enfants,
    raster_pop_enfants = raster_pop_enfants,
    ndvi_raster = ndvi_raster
  )
}

# 5) Calculs de base pour enrichir les shapefiles
calculer_indicateurs <- function(donnees) {
  # Récupération
  regions     <- donnees$regions
  departments <- donnees$departments
  communes    <- donnees$communes
  
  mean_raster      <- donnees$mean_raster
  raster_malaria   <- donnees$raster_nombre_malaria_enfants
  raster_pop       <- donnees$raster_pop_enfants
  ndvi_raster      <- donnees$ndvi_raster
  
  # Si le raster est NULL (en cas d’absence de fichiers), on gère prudemment
  if (!is.null(mean_raster)) {
    # Taux moyen de paludisme (exemple)
    regions$mean_index     <- exact_extract(mean_raster, regions, 'mean')
    departments$mean_index <- exact_extract(mean_raster, departments, 'mean')
    communes$mean_index    <- exact_extract(mean_raster, communes, 'mean')
  }
  
  if (!is.null(ndvi_raster)) {
    regions$mean_ndvi     <- exact_extract(ndvi_raster, regions, 'mean')
    departments$mean_ndvi <- exact_extract(ndvi_raster, departments, 'mean')
    communes$mean_ndvi    <- exact_extract(ndvi_raster, communes, 'mean')
  }
  
  if (!is.null(raster_malaria) && !is.null(raster_pop)) {
    # Taux malaria enfants = sum(malaria_enf) / sum(pop_enf)
    reg_mal <- exact_extract(raster_malaria, regions, 'sum')
    reg_pop <- exact_extract(raster_pop,     regions, 'sum')
    regions$taux_malaria_enfants <- ifelse(reg_pop > 0, reg_mal / reg_pop, NA)
    
    dep_mal <- exact_extract(raster_malaria, departments, 'sum')
    dep_pop <- exact_extract(raster_pop,     departments, 'sum')
    departments$taux_malaria_enfants <- ifelse(dep_pop > 0, dep_mal / dep_pop, NA)
    
    com_mal <- exact_extract(raster_malaria, communes, 'sum')
    com_pop <- exact_extract(raster_pop,     communes, 'sum')
    communes$taux_malaria_enfants <- ifelse(com_pop > 0, com_mal / com_pop, NA)
  }
  
  list(
    regions = regions,
    departments = departments,
    communes = communes
  )
}

# 6) Stockage "global" réactif (pour le pays en cours)
data_global <- reactiveValues(
  regions = NULL,
  departments = NULL,
  communes = NULL,
  mean_raster = NULL,
  raster_nombre_malaria_enfants = NULL,
  raster_pop_enfants = NULL,
  ndvi_raster = NULL
)

# 7) Fonction qui met à jour data_global selon le pays
update_data_global <- function(pays) {
  # Charger
  donnees_brutes <- charger_donnees_pays(pays)
  # Calculer les indicateurs
  res <- calculer_indicateurs(donnees_brutes)
  
  data_global$regions     <- res$regions
  data_global$departments <- res$departments
  data_global$communes    <- res$communes
  
  data_global$mean_raster                <- donnees_brutes$mean_raster
  data_global$raster_nombre_malaria_enfants <- donnees_brutes$raster_nombre_malaria_enfants
  data_global$raster_pop_enfants         <- donnees_brutes$raster_pop_enfants
  data_global$ndvi_raster                <- donnees_brutes$ndvi_raster
  
  message("Données mises à jour pour le pays : ", pays)
}

# Fin de global_KD.R
# ─────────────────────────────────────────────────────────────────────────────
