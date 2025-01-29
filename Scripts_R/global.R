# ─────────────────────────────────────────────────────────────────────────────────
# global.R : Gestion des données multi-pays avec calculs et chargement des rasters
# ─────────────────────────────────────────────────────────────────────────────────

# Augmente la mémoire maximale autorisée pour Shiny (500MB)
options(shiny.maxRequestSize = 500*1024^2)

# Désactive la censure des erreurs pour voir les messages complets
options(shiny.sanitize.errors = FALSE)

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
  "Senegal" = c("Taux moyen de Paludisme", "Taux de malaria chez les enfants", "CDI", "NDVI", "NDBI"),
  "Burkina" = c("Taux moyen de Paludisme", "Taux de malaria chez les enfants", "CDI", "NDVI", "NDBI")
)

# 3) Définition du chemin commun pour les shapefiles et rasters
chemin <- "data"

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
charger_donnees_pays <- function(pays, indicateur) {
  
  print(paste("📢 Chargement des données pour :", pays, "Indicateur :", indicateur))
  
  # Vérification des shapefiles
  required_shapefiles <- shapefiles[[pays]]
  missing_shapefiles <- required_shapefiles[!file.exists(unlist(required_shapefiles))]
  
  if (length(missing_shapefiles) > 0) {
    stop("❌ Erreur : Les shapefiles suivants sont introuvables pour ", pays, " : ", paste(names(missing_shapefiles), collapse = ", "))
  }
  
  # Chargement des shapefiles
  print("📢 Chargement des shapefiles...")
  regions <- st_read(shapefiles[[pays]]$regions, quiet = TRUE)
  departments <- st_read(shapefiles[[pays]]$departments, quiet = TRUE)
  communes <- st_read(shapefiles[[pays]]$communes, quiet = TRUE)
  print("✅ Shapefiles chargés")
  
  # Définition des fichiers rasters
  raster_path <- file.path(chemin, pays, "Rasters")
  
  raster_files <- list(
    "Taux moyen de Paludisme" = file.path(raster_path, "malaria/mean_raster.tif"),
    "Taux de malaria chez les enfants" = file.path(raster_path, "malaria_enfants/raster_nombre_malaria_enfants.tif"),
    "CDI" = file.path(raster_path, "CDI/mult_raster.tif"),
    "NDVI" = file.path(raster_path, paste0("Indices_spectraux/NDVI_vrai_", pays, ".tif")),
    "NDBI" = file.path(raster_path, paste0("Indices_spectraux/NDBI_vrai_", pays, ".tif"))
  )
  
  if (!(indicateur %in% names(raster_files))) {
    stop("❌ Erreur : Indicateur non reconnu - ", indicateur)
  }
  
  raster_file <- raster_files[[indicateur]]
  
  # Si le fichier raster est manquant, ne pas faire planter l'application
  if (!file.exists(raster_file)) {
    warning("⚠️ Attention : Le fichier raster pour ", indicateur, " est introuvable : ", raster_file)
    raster_data <- NULL
  } else {
    print("✅ Raster trouvé. Chargement...")
    raster_data <- raster(raster_file)
    print("✅ Raster chargé avec succès")
  }
  
  list(
    regions = regions,
    departments = departments,
    communes = communes,
    raster_data = raster_data
  )
}


# 6) Variables réactives pour stocker les données
data_global <- reactiveValues(
  regions = NULL,
  departments = NULL,
  communes = NULL,
  raster_data = NULL
)

# 7) Fonction pour mettre à jour les données globales
update_data_global <- function(pays, indicator_chosen) {
  req(pays, indicator_chosen)  # Vérifie que les valeurs ne sont pas NULL
  message("🔄 Chargement des données pour ", pays, " - ", indicator_chosen)
  
  donnees <- charger_donnees_pays(pays, indicator_chosen)
  
  data_global$regions <- donnees$regions
  data_global$departments <- donnees$departments
  data_global$communes <- donnees$communes
  data_global$raster_data <- donnees$raster_data
  data_global$departments$mean_index <- data_global$departments$men_ndx
  data_global$departments$mean_ndvi <- data_global$departments$men_ndv
  data_global$departments$mean_ndbi <- data_global$departments$men_ndb
  data_global$departments$taux_malaria_enfants <- data_global$departments$tx_mlr_
  data_global$regions$mean_index <- data_global$regions$men_ndx
  data_global$regions$mean_ndvi <- data_global$regions$men_ndv
  data_global$regions$mean_ndbi <- data_global$regions$men_ndb
  data_global$regions$taux_malaria_enfants <- data_global$regions$tx_mlr_
  data_global$communes$mean_index <- data_global$communes$men_ndx
  data_global$communes$mean_ndvi <- data_global$communes$men_ndv
  data_global$communes$mean_ndbi <- data_global$communes$men_ndb
  data_global$communes$taux_malaria_enfants <- data_global$communes$tx_mlr_
  
  message("✅ Données chargées avec succès.")
}
