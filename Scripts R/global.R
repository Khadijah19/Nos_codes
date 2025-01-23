# ─────────────────────────────────────────────────────────────────────────────
# global.R : Lecture des shapefiles et rasters, et calculs nécessaires
# ─────────────────────────────────────────────────────────────────────────────

# 1) Chargement des librairies nécessaires
library(shiny)          # Création d'applications interactives
library(shinyjs)        # Intégration de JavaScript dans Shiny
library(leaflet)        # Création de cartes interactives
library(dplyr)          # Manipulation des données
library(ggplot2)        # Visualisation des données

library(sf)             # Lecture et manipulation des shapefiles
sf_use_s2(FALSE)        # Désactivation temporaire de s2 pour éviter les erreurs
library(raster)         # Manipulation des données raster
library(exactextractr)  # Extraction des valeurs raster par polygone
library(viridis)        # Palette de couleurs pour les visualisations

# 2) Spécification du(s) pays et indicateur(s)
fake_countries <- c("Sénégal")
fake_indicators <- list(
  "Sénégal" = c("Taux moyen de Paludisme", "Taux d'enfants atteints par la malaria")
)

# 3) Définir le chemin général du projet
chemin_base <- "C:/Users/ALIOUNE KANE/Downloads/Projet_Final_Stat_Spatiale/data/Senegal"

# 4) Lecture des shapefiles avec des chemins relatifs
shapefile_path <- file.path(chemin_base, "Shapefiles")
regions <- st_read(file.path(shapefile_path, "sen_admbnda_adm1_anat_20240520.shp"), quiet = TRUE)
departments <- st_read(file.path(shapefile_path, "sen_admbnda_adm2_anat_20240520.shp"), quiet = TRUE)
communes <- st_read(file.path(shapefile_path, "sen_admbnda_adm3_anat_20240520.shp"), quiet = TRUE)

# 4.1) Vérification et correction des géométries invalides
regions <- st_make_valid(regions)
departments <- st_make_valid(departments)
communes <- st_make_valid(communes)

# 4.2) (Optionnel) Simplification des géométries pour améliorer les performances ou réduire les anomalies
# regions <- st_simplify(regions, dTolerance = 100)
# departments <- st_simplify(departments, dTolerance = 50)
# communes <- st_simplify(communes, dTolerance = 25)

# 4.3) Assurer que tous les shapefiles ont le même CRS
# Supposons que les rasters utilisent le CRS WGS84 (EPSG:4326)
crs_target <- st_crs(4326)

regions <- st_transform(regions, crs_target)
departments <- st_transform(departments, crs_target)
communes <- st_transform(communes, crs_target)

# 4.4) Fusion des régions pour représenter l'ensemble du Sénégal (senegal_sf)
senegal_sf <- st_union(regions)
# Corriger si besoin la géométrie fusionnée
senegal_sf <- st_make_valid(senegal_sf)

# 4.5) Conversion de 'senegal_sf' en objet Spatial pour compatibilité avec raster
senegal_sp <- as(senegal_sf, "Spatial")

# 5) Chargement des rasters en batch
raster_path <- file.path(chemin_base, "Rasters")
fichiers_raster <- list.files(raster_path, pattern = "\\.tiff$", full.names = TRUE)
rasters <- stack(fichiers_raster)

# 6) Calcul du raster moyen (mean_raster)
mean_raster <- calc(rasters, fun = mean, na.rm = TRUE)

# 7) Calcul du taux moyen par niveau administratif
region_means <- exact_extract(mean_raster, regions, 'mean')
regions$mean_index <- region_means

department_means <- exact_extract(mean_raster, departments, 'mean')
departments$mean_index <- department_means

commune_means <- exact_extract(mean_raster, communes, 'mean')
communes$mean_index <- commune_means

# 8) Résumé des résultats
summary(regions$mean_index)
summary(departments$mean_index)
summary(communes$mean_index)

# 9) Ajout des noms des zones dans des vecteurs
region_names <- regions$ADM1_EN
department_names <- departments$ADM2_EN
commune_names <- communes$ADM3_EN

########################################################################################################################
###############################################################################
# Script : Exemple d'intégration du calcul "Indicateur du TP5" pour la malaria
###############################################################################

# 1) Définir le répertoire de travail
setwd(dir = "C:/Users/ALIOUNE KANE/Downloads/Projet_Final_Stat_Spatiale")

# 2) Charger les librairies nécessaires
suppressMessages({
  library(sf)
  library(raster)
  library(dplyr)
  library(ggplot2)
  library(viridis)
  library(exactextractr)
  library(leaflet)
  library(ggspatial)  # pour annotation_north_arrow / annotation_scale
})

# 3) Définir les chemins principaux
base_path       <- getwd()
raster_path     <- file.path(base_path, "data/Senegal/Rasters")
shapefile_path  <- file.path(base_path, "data/Senegal/Shapefiles")

# ─────────────────────────────────────────────────────────────────────────────
# 4) Chargement des shapefiles
# ─────────────────────────────────────────────────────────────────────────────
senegal     <- st_read(file.path(shapefile_path, "sen_admbnda_adm0_anat_20240520.shp"), quiet = TRUE)
region      <- st_read(file.path(shapefile_path, "sen_admbnda_adm1_anat_20240520.shp"), quiet = TRUE)
departement <- st_read(file.path(shapefile_path, "sen_admbnda_adm2_anat_20240520.shp"), quiet = TRUE)

# ─────────────────────────────────────────────────────────────────────────────
# 5) Chargement et préparation des rasters
# ─────────────────────────────────────────────────────────────────────────────
malaria_2022 <- raster(file.path(raster_path, "202406_Global_Pf_Parasite_Rate_SEN_2022.tiff")) %>%
  crop(senegal) %>%
  mask(senegal)

pop <- raster(file.path(raster_path, "SEN_population_v1_0_gridded.tif")) %>%
  crop(senegal) %>%
  mask(senegal)

# ─────────────────────────────────────────────────────────────────────────────
# 6) Visualisation de la population (première carte)
# ─────────────────────────────────────────────────────────────────────────────
pop_df <- as.data.frame(rasterToPoints(pop), stringsAsFactors = FALSE)
colnames(pop_df) <- c("x", "y", "value")

ggplot() +
  geom_tile(data = pop_df, aes(x = x, y = y, fill = value)) +
  geom_sf(data = departement, fill = NA, color = "black", size = 0.5) +
  scale_fill_viridis(option = "viridis", na.value = "transparent") +
  labs(title = "Population du Sénégal avec départements",
       fill = "Population") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    legend.position = "right"
  ) +
  annotation_north_arrow(
    location = "tl", which_north = "true",
    pad_x = unit(0.1, "in"), pad_y = unit(0.1, "in"),
    style = north_arrow_fancy_orienteering()
  ) +
  annotation_scale(location = "bl", width_hint = 0.5)

# ─────────────────────────────────────────────────────────────────────────────
# 7) Calcul de la population par niveau administratif
# ─────────────────────────────────────────────────────────────────────────────
pop_admin0 <- exact_extract(pop, senegal, fun = "sum", progress = FALSE)
senegal$population <- pop_admin0

pop_admin1 <- exact_extract(pop, region, fun = "sum", progress = FALSE)
region$population <- pop_admin1

pop_admin2 <- exact_extract(pop, departement, fun = "sum", progress = FALSE)
departement$population <- pop_admin2

# ─────────────────────────────────────────────────────────────────────────────
# 8) Indicateur du TP5 : Taux d'enfants atteints par la malaria
#    (Remplacement de l'ancien code par le nouveau)
# ─────────────────────────────────────────────────────────────────────────────

##
## 8.1) Calcul de la population d'enfants
##
# Ici, on suppose que 0,1% de la population totale a entre 0 et 12 ans.
# (taux à ajuster selon les besoins ou données réelles)
pop_child <- pop * 0.001

# Pour visualiser cette population d'enfants :
pop_child_df <- as.data.frame(rasterToPoints(pop_child), stringsAsFactors = FALSE)
colnames(pop_child_df) <- c("x", "y", "value")

ggplot() +
  geom_tile(data = pop_child_df, aes(x = x, y = y, fill = value)) +
  geom_sf(data = departement, fill = NA, color = "black", size = 0.5) +
  scale_fill_viridis(option = "turbo", na.value = "transparent") +
  labs(title = "Enfants de 0 à 12 ans au Sénégal",
       fill = "Population") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    legend.position = "right"
  ) +
  annotation_north_arrow(
    location = "tl", which_north = "true",
    pad_x = unit(0.1, "in"), pad_y = unit(0.1, "in"),
    style = north_arrow_fancy_orienteering()
  ) +
  annotation_scale(location = "bl", width_hint = 0.5)

##
## 8.2) Calcul des enfants atteints de malaria
##
taux_infection <- 0.03  # Exemple de taux d'infection
pop_malaria    <- pop_child * taux_infection

# Extraction par niveau administratif (somme des valeurs dans chaque polygone)
pop_senegal_malade <- exact_extract(pop_malaria, senegal, fun = "sum", progress = FALSE)
pop_region_malade  <- exact_extract(pop_malaria, region, fun = "sum", progress = FALSE)
pop_dept_malade    <- exact_extract(pop_malaria, departement, fun = "sum", progress = FALSE)

# Ajout dans les tables attributaires
senegal$pop_malade     <- pop_senegal_malade
region$pop_malade      <- pop_region_malade
departement$pop_malade <- pop_dept_malade

##
## 8.3) Calcul du taux de malaria par rapport à la population totale
##      On normalise par "population * 0.001" car pop_child = population * 0.001
##
senegal$taux_malade     <- (senegal$pop_malade     / (senegal$population * 0.001)) * 100
region$taux_malade      <- (region$pop_malade      / (region$population * 0.001)) * 100
departement$taux_malade <- (departement$pop_malade / (departement$population * 0.001)) * 100

# Vérification rapide
summary(senegal$taux_malade)
summary(region$taux_malade)
summary(departement$taux_malade)


# ─────────────────────────────────────────────────────────────────────────────
# Fin du script global.R
# ─────────────────────────────────────────────────────────────────────────────

# Réactivez éventuellement s2 à la fin si vous en avez besoin pour d'autres opérations
# sf_use_s2(TRUE)
