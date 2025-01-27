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
library(raster)         # Manipulation des données raster
library(exactextractr)  # Extraction des valeurs raster par polygone
library(viridis)        # Palette de couleurs pour les visualisations

# 2) Spécification du(s) pays et indicateur(s)
fake_countries <- c("Sénégal", "Burkina")

fake_indicators <- list(
  "Sénégal" = c("Taux moyen de Paludisme", 
                "Taux d'enfant atteint par la malaria"),
  "Burkina" = c("Taux moyen de Paludisme", 
                "Taux d'enfant atteint par la malaria")
)


# 3) Définir le chemin général du projet
chemin_base <- "C:/Users/ALIOUNE KANE/Downloads/Projet_Final_Stats_Spatiales/data/Senegal"

# 4) Lecture des shapefiles avec des chemins relatifs
shapefile_path <- file.path(chemin_base, "Shapefiles")
regions <- st_read(file.path(shapefile_path, "sen_admbnda_adm1_anat_20240520.shp"), quiet = TRUE)
departments <- st_read(file.path(shapefile_path, "sen_admbnda_adm2_anat_20240520.shp"), quiet = TRUE)
communes <- st_read(file.path(shapefile_path, "sen_admbnda_adm3_anat_20240520.shp"), quiet = TRUE)

# 5) Chargement des rasters en batch
raster_path <- file.path(chemin_base, "Rasters")
fichiers_raster <- list.files(raster_path, pattern = "\\.tiff$", full.names = TRUE)
rasters <- stack(fichiers_raster)

# 6) Calcul du raster moyen (mean_raster)
mean_raster <- calc(rasters, fun = mean, na.rm = TRUE)

# 7) Calcul du taux moyen par niveau administratif (Taux moyen de Paludisme)
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

# ─────────────────────────────────────────────────────────────────────────────
# 6bis) Calcul du Taux d'enfant atteint par la malaria (exemple)
# ─────────────────────────────────────────────────────────────────────────────
#  Dans la vraie logique, vous utiliserez le code de votre script sur la malaria,
#  en adaptant la partie extraction/agrégation (exact_extract, etc.) pour aboutir
#  à une colonne "taux_malaria" dans chacune des tables (regions, departments, communes).

# Par exemple (code simplifié / fictif) :
# On suppose que vous avez un raster "raster_malaria_enfants" déjà préparé,
# représentant la proportion d'enfants atteints par la malaria.

# Pour l'exemple, supposons qu'on utilise le mean_raster, juste pour la démonstration :
# (Remplacez ci-dessous par VOTRE raster correspondant)
raster_malaria_enfants <- mean_raster  # A remplacer par votre vrai raster de malaria

# Extraction par zones
region_malaria <- exact_extract(raster_malaria_enfants, regions, 'mean')
departments_malaria <- exact_extract(raster_malaria_enfants, departments, 'mean')
communes_malaria <- exact_extract(raster_malaria_enfants, communes, 'mean')

# Stockage dans les tables sf
regions$taux_malaria <- region_malaria
departments$taux_malaria <- departments_malaria
communes$taux_malaria <- communes_malaria

# ─────────────────────────────────────────────────────────────────────────────
# Fin du script global.R
# ─────────────────────────────────────────────────────────────────────────────
