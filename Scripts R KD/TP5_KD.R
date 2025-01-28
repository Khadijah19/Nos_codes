#====================================================================================#
#     ENSAE Pierre NDIAYE de Dakar ISE1-Cycle long 2024-2025                         #
#     COURS DE Statistique exploratoire spaciale  avec M.Aboubacar HEMA              #
#                           Travaux Pratiques N°5                                    #
#                                                                                    #
#    Groupe : Logiciel R                                                             #
#    Pays : Cameroun                                                                 #
#    Composé de : Jeanne De La Flèche ONANENA AMANA, Khadidiatou COULIBALY,          #
#                 Tamsir NDONG, Samba DIENG                                          #
#                                                                                    #
#====================================================================================#


#                   =====================  CONSIGNE  =====================

# 1. Importer et visualiser le raster population
# 2. Calculer le nombre de personnes par admin et exporter sous format .csv
# 3. Ramener la taille des pixels à 5km
# 4. Visualiser le nouveau raster de la population
# 5. Calculer un nouveau raster d'enfants de 0 à 12ans (O.1%)
# 6. Créer 3 nouveaux rasters binarisés
# 7. Multiplier chacun d'eux par celui de la population
# 8. Calculer nombre d'enfants atteints de la malaria par admin et exporter
# 9. Quel est le taux d'enfants atteints de malaria par admin ?

# Chargement des packages

library(stars)       
library(sf)          
library(ggplot2)     
library(ggspatial)   
library(raster)      
library(cowplot)     
library(leaflet)     
library(viridis)     
library(dplyr)       
library(exactextractr) 
library(kableExtra)
library(knitr)





# 1. Importation et visualisation -------------------------------------------------------------------------------------

# Définir le répertoire de travail
setwd(dir = "C:/Users/HP/OneDrive/Documents/GitHub/Projet_Final_Stat_Spatiale/data/Senegal")

suppressMessages({
  # Définir le chemin principal
  base_path <- getwd()
  
  # Charger les shapefiles
  shapefile_path <- file.path(base_path, "Shapefiles")
  senegal <- st_read(file.path(shapefile_path, "sen_admbnda_adm0_anat_20240520.shp"), quiet = TRUE)
  region <- st_read(file.path(shapefile_path, "sen_admbnda_adm1_anat_20240520.shp"), quiet = TRUE)
  departement <- st_read(file.path(shapefile_path, "sen_admbnda_adm2_anat_20240520.shp"), quiet = TRUE)
  
  # Charger les rasters
  raster_path <- file.path(base_path, "Rasters")
  
  malaria_2022 <- raster(file.path(raster_path, "202406_Global_Pf_Parasite_Rate_SEN_2022.tiff")) %>%
    crop(senegal) %>%
    mask(senegal)
  
  pop <- raster(file.path(raster_path, "SEN_population_v1_0_gridded.tif")) %>%
    crop(senegal) %>%
    mask(senegal)
})



pop_df <- as.data.frame(rasterToPoints(pop), stringsAsFactors = FALSE)
colnames(pop_df) <- c("x", "y", "value")
ggplot() +
  geom_tile(data = pop_df, aes(x = x, y = y, fill = value)) +
  geom_sf(data = departement, fill = NA, color = "black", size = 0.5) +  # Ajouter les contours des régions
  scale_fill_viridis(option = "viridis", na.value = "transparent") +
  labs(title = "Population du Cameroun avec départements",
       fill = "Population") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    legend.position = "right"
  )+ 
  annotation_north_arrow(location = "tl", which_north = "true", pad_x = unit(0.1, "in"), pad_y = unit(0.1, "in"), style = north_arrow_fancy_orienteering()) + 
  annotation_scale(location = "bl", width_hint = 0.5)

# On étend artificiellement notre shapefile du pays
cameroun_1 <- st_buffer(senegal, dist = 5000)

ggplot() +
  geom_sf(data = cameroun_1, fill= NA, color = "red", size = 0.5) +  # Ajout du shapefile "bufferisé"
  geom_sf(data = senegal, fill= NA, color = "blue", size = 0.5) + # Ajout du shp orignal
  scale_fill_viridis(option = "viridis", na.value = "transparent") +
  labs(title = "Cameroun avec et sans buffer") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    legend.position = "right"
  )+ 
  annotation_north_arrow(location = "tl", which_north = "true", pad_x = unit(0.1, "in"), pad_y = unit(0.1, "in"), style = north_arrow_fancy_orienteering()) + 
  annotation_scale(location = "bl", width_hint = 0.5)





# pour vérifier la résolution
library(terra)
senegal_ <- rast(file.path(raster_path, "202406_Global_Pf_Parasite_Rate_SEN_2022.tiff"))
a=res(senegal_)
print(a)





# On va maintenant faire une "intersection"

region_2 <-  suppressWarnings(st_intersection(region, cameroun_1))
departement <- suppressWarnings(st_intersection(departement, cameroun_1))
#arrondissement <- suppressWarnings(st_intersection(arrondissement, cameroun_1))

# Il semble que cela redonne les mêmes shp..

# Visualisons
ggplot() + 
  geom_sf(data = departement, mapping = aes(fill = ADM1_FR)) + 
  geom_sf(data = cameroun_1, fill= NA, color = "black", size = 0.5) + # Ajout du shp orignal
  ggtitle("Départements") +  # Titre de la carte
  theme(plot.title = element_text(hjust = 0.5, size = 25)) + 
  xlab("Longitude") + 
  ylab("Latitude") + 
  annotation_north_arrow(location = "bl", which_north = "true", pad_x = unit(0.25, "in"), pad_y = unit(0.25, "in"), style = north_arrow_fancy_orienteering()) + 
  annotation_scale(location = "bl", width_hint = 0.5)




cameroun = senegal
# 2. Calculer la population pour chaque niveau administratif--------------------------------------------------------------------------------------
pop_admin0 <- exact_extract(pop, cameroun, fun = "sum", progress = FALSE)
cameroun$population <- pop_admin0

library(dplyr)
pop_cameroun <- cameroun %>%
  st_drop_geometry() %>%       
  dplyr::select(ADM0_FR, population) 


pop_cameroun <- data.frame(pop_cameroun)
pop_cameroun

pop_admin1 <- exact_extract(pop, region, fun = "sum", progress = FALSE)
region$population <- pop_admin1

pop_region <- region %>%
  st_drop_geometry() %>%      
  dplyr::select(ADM1_FR, population)  

pop_region <- data.frame(pop_region)
head(pop_region, 5)

pop_admin2 <- exact_extract(pop, departement, fun = "sum", progress = FALSE)
departement$population <- pop_admin2

pop_departement <- departement %>%
  st_drop_geometry() %>%      
  dplyr::select(ADM2_FR, population)   

pop_departement <- data.frame(pop_departement)
head(pop_departement, 5)

#pop_admin3 <- exact_extract(pop, arrondissement, fun = "sum", progress = FALSE)
#arrondissement$population <- pop_admin3

#pop_arrondissement <- arrondissement %>%
#  st_drop_geometry() %>%       
#  select(ADM3_FR, population)  

#pop_arrondissement <- data.frame(pop_arrondissement)
#head(pop_arrondissement, 5)

# Vérifications
t <- data.frame(sum_region = sum(pop_region$population),
                sum_departements=sum(pop_departement$population)
               # sum_arrondissements=sum(pop_arrondissement$population))
)
t

## Les résultats sont égaux A DES VIRGULES PRES !

output_folder <- "Outputs/"
dir.create(output_folder, showWarnings = FALSE) 

# Exporter chaque data frame sous format CSV
write.csv(pop_cameroun, file.path(output_folder, "pop_cameroun.csv"), row.names = FALSE)
write.csv(pop_region, file.path(output_folder, "pop_region.csv"), row.names = FALSE)
write.csv(pop_departement, file.path(output_folder, "pop_departement.csv"), row.names = FALSE)
write.csv(pop_arondissement, file.path(output_folder, "pop_arondissement.csv"), row.names = FALSE)





# 3. Résolution à 5km par aggrégation (somme)------------------------------------------------------------------------------------------------

## Voyons la taille des pixels en degrés
res(pop)

pop_newResw <- aggregate(pop, fact = 50, fun = sum, 
                         filename = "SN_population_aggregated_5km.tif", 
                         overwrite = TRUE)

# facteur d'agrégation : 5000m/100m
# Agrégation des valeurs : somme des valeurs des pixels ('sum')

## Metadonnées du nouveau raster
pop_newResw

# 4. Visualisation du nouveau raster------------------------------------------------------------------------------------ 

# Vérifications
pop_a <- exact_extract(pop_newResw %>%
                         crop(cameroun_1) %>%
                         mask(cameroun_1), cameroun_1, fun = "sum", progress = FALSE)



pop_df <- as.data.frame(rasterToPoints(pop_newResw), stringsAsFactors = FALSE)
colnames(pop_df) <- c("x", "y", "value")
ggplot() +
  geom_tile(data = pop_df, aes(x = x, y = y, fill = value)) +
  geom_sf(data = departement, fill = NA, color = "black", size = 0.5) +  # Ajouter les contours des régions
  scale_fill_viridis(option = "plasma", na.value = "transparent") +
  labs(title = "Population du Cameroun avec départements",
       fill = "Population") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    legend.position = "right"
  )+ 
  annotation_north_arrow(location = "tl", which_north = "true", pad_x = unit(0.1, "in"), pad_y = unit(0.1, "in"), style = north_arrow_fancy_orienteering()) + 
  annotation_scale(location = "bl", width_hint = 0.5)





# 5. Raster des enfants de 0 à 12 ans (0.1% de la population)----------------------------------------------------------------------------------

# Multiplication
pop_child <- pop_newResw*0.001

pop_b <- exact_extract(pop_child %>%
                         crop(cameroun_1) %>%
                         mask(cameroun_1), cameroun_1, fun = "sum", progress = FALSE)


pop_b
(pop_b/pop_a)*100





# 6. Visualisation du nouveau raster --------------------------------------------------------------------------------------------------------------

pop_df <- data.frame(rasterToPoints(pop_child), stringsAsFactors = FALSE)
colnames(pop_df) <- c("x", "y", "value")
ggplot() +
  geom_tile(data = pop_df, aes(x = x, y = y, fill = value)) +
  geom_sf(data = departement, fill = NA, color = "black", size = 0.5) +  # Ajouter les contours des régions
  scale_fill_viridis(option = "turbo", na.value = "transparent") +
  labs(title = "Population du Cameroun avec régions",
       fill = "Population") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    legend.position = "right"
  )+ 
  annotation_north_arrow(location = "tl", which_north = "true", pad_x = unit(0.1, "in"), pad_y = unit(0.1, "in"), style = north_arrow_fancy_orienteering()) + 
  annotation_scale(location = "bl", width_hint = 0.5)





# 7. Rasters binarisés à partir de celui du taux de malaria-------------------------------------------------------------------------------------

# Utilisation du resampling...avec la méthode des plus rpoches voisins (ngb)
## On aligne les rasters suivant celui de la population

# Raster des situations (1, 2, 3)
malaria_2022_CMR_1 <- resample(malaria_2022, pop_child, method = "ngb")
plot(malaria_2022)

malaria_enfants <-malaria_2022_CMR_1*0.001


library(terra)

# Enregistrer le raster au format TIFF
writeRaster(malaria_enfants, "C:/Users/HP/Downloads/malaria_enfants/taux malaria_enfants.tif", format = "GTiff", overwrite = TRUE)

# Vérification
print(r)


plot(malaria_enfants)


pop_cameroun$pop_malade <- exact_extract(pop_malaria, cameroun, fun = "sum", progress = FALSE)

pop_region$pop_malade <- exact_extract(pop_malaria, region, fun = "sum", progress = FALSE) 

pop_departement$pop_malade <- exact_extract(pop_malaria, departement, fun = "sum", progress = FALSE) 

pop_arrondissement$pop_malade  <- exact_extract(pop_malaria, arrondissement, fun = "sum", progress = FALSE) 



# Raster des taux en 2022
taux_2022_1 <- resample(taux_2022, pop_child, method = "ngb")

# Nombre d'enfants "par situation" (atteints ou non)
aucun <- calc(malaria_2022_CMR_1 == 1, fun = function(x) { ifelse(x, 1, 0) })*pop_child
moyen <- calc(malaria_2022_CMR_1 == 2, fun = function(x) { ifelse(x, 1, 0) })*pop_child
grave <- calc(malaria_2022_CMR_1 == 3, fun = function(x) { ifelse(x, 1, 0) })*pop_child

plot()
# Nombre d'enfants atteints par situation
pop_aucun  <- aucun*taux_2022_1
pop_moyen  <- moyen*taux_2022_1
pop_grave  <- grave*taux_2022_1

plot(pop_aucun)

# Calculons le nombre d'enfants atteints par situation

A <-  sum(values(pop_aucun), na.rm = TRUE)
B <- sum(values(pop_moyen), na.rm = TRUE)
C <-  sum(values(pop_grave), na.rm = TRUE) 


t1 <- data.frame(
  situation_aucun = c(A, A*100/(A+B+C)),
  situation_moyen = c(B, B*100/(A+B+C)),
  situation_grave = c(C, C*100/(A+B+C)),
  somme =  c( A+B+C, A*100/(A+B+C)+ B*100/(A+B+C)+ C*100/(A+B+C))
)



## Vérifications...

# Raster ayant toute la population malade
pop_malaria <- pop_child* resample(taux_2022, pop_child, method = "ngb")
enf_malades <- sum(values(pop_malaria), na.rm = TRUE) 

t_1 <-data.frame(total_enfants_malades = enf_malades ) %>% t()




# 8. Nombre d'enfants atteints par admin-------------------------------------------------------------------------------------------------------

pop_cameroun$pop_malade <- exact_extract(pop_malaria, cameroun, fun = "sum", progress = FALSE)

pop_region$pop_malade <- exact_extract(pop_malaria, region, fun = "sum", progress = FALSE) 

pop_departement$pop_malade <- exact_extract(pop_malaria, departement, fun = "sum", progress = FALSE) 

pop_arrondissement$pop_malade  <- exact_extract(pop_malaria, arrondissement, fun = "sum", progress = FALSE) 

# Exporter chaque data frame sous format CSV
write.csv(pop_cameroun, file.path(output_folder, "cameroun.csv"), row.names = FALSE)
write.csv(pop_region, file.path(output_folder, "region_cmr.csv"), row.names = FALSE)
write.csv(pop_departement, file.path(output_folder, "departement_cmr.csv"), row.names = FALSE)
write.csv(pop_arondissement, file.path(output_folder, "arondissement_cmr.csv"), row.names = FALSE)

head(pop_arrondissement,10)


# Vérifications
t2 <- data.frame(
  enfts_malades_cameroun = sum(pop_cameroun$pop_malade),
  enfts_malades_region = sum(pop_region$pop_malade),
  enfts_malades_departements=sum(pop_departement$pop_malade),
  enfts_malades_arrondissements=sum(pop_arrondissement$pop_malade)) %>% t()




# 9. Taux d'enfants atteints par admin--------------------------------------------------------------------------------------------------------

## On prend la population malade sur celle totale d'enfants et le tout * 100
# taux = nb atteints dans l'admin / nb total

pop_cameroun$taux_malade <- pop_cameroun$pop_malade/(pop_cameroun$population*0.001)

pop_region$taux_malade <- pop_region$pop_malade/(pop_region$population*0.001)
plot(pop_region)

pop_departement$taux_malade <- pop_departement$pop_malade/(pop_departement$population*0.001)

pop_arrondissement$taux_malade  <- pop_arrondissement$pop_malade/(pop_arrondissement$population*0.001)

head(pop_arrondissement,10)

# ////////////////////////////////////////////////////// Fin du script /////////////////////////////////////////////////////////////////











######### taux enfants malaria


pop_newResw <- aggregate(pop, fact = 50, fun = sum, 
                         filename = "SN_population_aggregated_5km.tif", 
                         overwrite = TRUE)

raster_pop_enfants <-  pop_newResw*0.001

origin(raster_pop_enfants)
origin(malaria_2022)
origin(raster_pop_enfants) <- origin(malaria_2022)

raster_nombre_malaria_enfants <-raster_pop_enfants*malaria_2022

plot(raster_nombre_malaria_enfants)

raster_taux_enfants_malaria  <-raster_nombre_malaria_enfants/raster_pop_enfants
plot(raster_taux_enfants_malaria)

# Enregistrer le raster au format TIFF
writeRaster(raster_taux_enfants_malaria, "C:/Users/HP/Downloads/malaria_enfants/raster_taux_enfants_malaria.tif", format = "GTiff", overwrite = TRUE)
writeRaster(raster_nombre_malaria_enfants, "C:/Users/HP/Downloads/malaria_enfants/raster_nombre_malaria_enfants.tif", format = "GTiff", overwrite = TRUE)
writeRaster(raster_pop_enfants, "C:/Users/HP/Downloads/malaria_enfants/raster_pop_enfants.tif", format = "GTiff", overwrite = TRUE)


plot(raster_pop_enfants)
res(raster_pop_enfants)
res(raster_pop_enfants)
raster(malaria_2022)
raster_taux_enfants



# 7) Calcul du taux moyen par niveau administratif (Taux moyen de Paludisme pour les enfants)
region_nombre_enfants_malaria <- exact_extract(raster_nombre_malaria_enfants, regions, 'sum')
region_nombre_enfants <- exact_extract(raster_pop_enfants, regions, 'sum')

regions$nb_enfants_malaria<-region_nombre_enfants_malaria
regions$nb_enfants<-region_nombre_enfants

regions$taux_malaria_enf_region <- (regions$nb_enfants_malaria/regions$nb_enfants)



BI_SN <- raster("C:/Users/HP/OneDrive/Documents/GitHub/Projet_Final_Stat_Spatiale/data/Senegal/TP10_Indicators_SEN/NDVI_Senegal.tif")
region_means <- exact_extract(BI_SN, regions, 'mean')


department_means <- exact_extract(mean_raster, departments, 'mean')

commune_means <- exact_extract(mean_raster, communes, 'mean')





library(dplyr)
library(terra)
BI_SN <- raster("C:/Users/HP/OneDrive/Documents/GitHub/Projet_Final_Stat_Spatiale/data/Senegal/TP10_Indicators_SEN/BI_SN.tif") %>%
  crop(senegal) %>%
  mask(senegal)
View(BI_SN)
plot(BI_SN)
print(res(BI_SN))

summary(BI_SN)  # Vérifiez les valeurs minimales, maximales et NA
plot(BI_SN)  # Visualisez le raster
unique(values(BI_SN))

# Agréger le raster avec un facteur de 5 (chaque pixel regroupe 5x5 pixels)
BI_SN_aggregated <- aggregate(BI_SN, fact = 167, fun = mean, na.rm = TRUE)

# Visualiser les dimensions et la résolution du raster agrégé
print(BI_SN_aggregated)
plot(BI_SN_aggregated)

summary(BI_SN_aggregated)

5000/30



# 3) Définir le chemin général du projet
chemin_base <- "C:/Users/HP/OneDrive/Documents/GitHub/Projet_Final_Stat_Spatiale/data/Senegal"

# 4) Lecture des shapefiles avec des chemins relatifs
shapefile_path <- file.path(chemin_base, "Shapefiles")
pays <- st_read(file.path(shapefile_path, "sen_admbnda_adm0_anat_20240520.shp"), quiet = TRUE)
regions <- st_read(file.path(shapefile_path, "sen_admbnda_adm1_anat_20240520.shp"), quiet = TRUE)
departments <- st_read(file.path(shapefile_path, "sen_admbnda_adm2_anat_20240520.shp"), quiet = TRUE)
communes <- st_read(file.path(shapefile_path, "sen_admbnda_adm3_anat_20240520.shp"), quiet = TRUE)

# 5) Chargement des rasters en batch
raster_path <- file.path(chemin_base, "Rasters")
fichiers_raster <- list.files(raster_path, pattern = "\\.tiff$", full.names = TRUE)
rasters <- stack(fichiers_raster)

# 6) Calcul du raster moyen (mean_raster)
mean_raster <- calc(rasters, fun = mean, na.rm = TRUE)
plot(mean_raster)


# Pour Burkina taux malaria chez les enfants
# Définir le répertoire de travail
setwd(dir = "C:/Users/HP/OneDrive/Documents/GitHub/Projet_Final_Stat_Spatiale/data/Burkina")

suppressMessages({
  # Définir le chemin principal
  base_path <- getwd()
  
  # Charger les shapefiles
  shapefile_path <- file.path(base_path, "Shapefiles")
  bfa <- st_read(file.path(shapefile_path, "bfa_admbnda_adm0_igb_20200323.shp"), quiet = TRUE)
  region <- st_read(file.path(shapefile_path, "bfa_admbnda_adm1_igb_20200323.shp"), quiet = TRUE)
  departement <- st_read(file.path(shapefile_path, "bfa_admbnda_adm2_igb_20200323.shp"), quiet = TRUE)
  
  # Charger les rasters
  raster_path <- file.path(base_path, "Rasters")
  
  malaria_2022 <- raster(file.path(raster_path, "202406_Global_Pf_Parasite_Rate_BFA_2022.tiff")) %>%
    crop(bfa) %>%
    mask(bfa)
  
  pop <- raster(file.path(raster_path, "BFA_population_v1_1_gridded.tif")) %>%
    crop(bfa) %>%
    mask(bfa)
})


pop_newResw <- aggregate(pop, fact = 50, fun = sum, 
                         filename = "SN_population_aggregated_5km.tif", 
                         overwrite = TRUE)

raster_pop_enfants <-  pop_newResw*0.001

origin(raster_pop_enfants)
origin(malaria_2022)
origin(raster_pop_enfants) <- origin(malaria_2022)

raster_nombre_malaria_enfants <-raster_pop_enfants*malaria_2022

plot(malaria_2022)

raster_taux_enfants_malaria  <-raster_nombre_malaria_enfants/raster_pop_enfants
plot(raster_taux_enfants_malaria)

# Enregistrer le raster au format TIFF
writeRaster(raster_taux_enfants_malaria, "C:/Users/HP/OneDrive/Documents/GitHub/Projet_Final_Stat_Spatiale/data/Burkina/Rasters/malaria_enfants/raster_taux_enfants_malaria.tif", format = "GTiff", overwrite = TRUE)
writeRaster(raster_nombre_malaria_enfants, "C:/Users/HP/OneDrive/Documents/GitHub/Projet_Final_Stat_Spatiale/data/Burkina/Rasters/malaria_enfants/raster_nombre_malaria_enfants.tif", format = "GTiff", overwrite = TRUE)
writeRaster(raster_pop_enfants, "C:/Users/HP/OneDrive/Documents/GitHub/Projet_Final_Stat_Spatiale/data/Burkina/Rasters/malaria_enfants/raster_pop_enfants.tif", format = "GTiff", overwrite = TRUE)



##CDI 
CDI <- raster("C:/Users/HP/OneDrive/Documents/GitHub/Projet_Final_Stat_Spatiale/data/Burkina/Rasters/CDI/burkina_tous_points_raster.tif")
summary(CDI)
plot(CDI)




data <- read.csv("C:/Users/HP/OneDrive/1231116193333-Desktop/Desktop/ISEP3/Stat_spatiales/TP11/Points_data.csv")
View(data)

## Chargement du fichier de données spatiales (shapefile) pour les limites administratives du Mali

shp <- st_read("C:/Users/HP/OneDrive/Documents/GitHub/Projet_Final_Stat_Spatiale/data/Senegal/Shapefiles/sen_admbnda_adm0_anat_20240520.shp")

## Conversion des données en un objet spatial sf

data_spatial <- sf::st_as_sf(data, coords = c("longitude", "latitude"), crs = st_crs(shp))


# On selectionne donc le pays
BFA = "Senegal"

# Créer un sous-ensemble de données ne contenant que les événements relatifs au Mali
AOI_event <- data_spatial %>%
  filter(country == BFA)

plot(AOI_event)

# Visualisation des événements au Mali
ggplot(AOI_event) +
  aes(fill = event_type, colour = event_type) + # Associer les couleurs aux types d'événements
  geom_sf(size = 1.2) + # Représentation des géométries spatiales
  scale_fill_hue(direction = 1) + # Échelle de couleur pour les types d'événements
  theme_minimal()

# Fonction pour créer un raster basé sur les données géolocalisées
Create_raster <- function(datafile, filename = "C:/Users/HP/OneDrive/1231116193333-Desktop/Desktop/ISEP3/Stat_spatiales/TP11/Rasterisation.tif") {
  # Reprojection des données spatiales pour un système de coordonnées en mètres
  # EPSG 32629 : UTM Zone 29N, adapté au Mali
  AOI_prj <- st_transform(datafile, crs = 32629)  
  
  # Définir l'étendue géographique (extent) à partir des limites de l'objet spatial reprojeté
  ext <- raster::extent(sf::st_bbox(AOI_prj))  # Conversion des limites en format `extent`
  
  # Spécification de la résolution en mètres (5 km ici)
  res <- 5000  
  
  # Définir un système de coordonnées pour le raster
  rast_crs <- CRS("+proj=utm +zone=29 +datum=WGS84 +units=m +no_defs")
  raster_template <- raster::raster(ext = ext, resolution = res, crs = rast_crs)
  
  # Rasteriser les données : calculer la somme des événements dans chaque cellule de la grille
  Raster <- raster::rasterize(AOI_prj, raster_template, field = 1, fun = sum, na.rm = TRUE)
  
  # Sauvegarder le raster en format GeoTIFF
  raster::writeRaster(Raster, filename = filename, format = "GTiff", overwrite = TRUE)
  
  return(Raster) # Pour retourner l'objet raster
}

# Application de la fonction sur les données des événements
AOI_Raster <- Create_raster(
  AOI_event, 
  "C:/Users/HP/OneDrive/1231116193333-Desktop/Desktop/ISEP3/Stat_spatiales/TP11/Rasterisation_general.tif"
)
plot(AOI_Raster)


### Création de raster pour chaque année et visualistion du nombre d'attaques par année

for( i in unique(data$year)){
  assign(paste0("data_", i), data[data$year == i,])
  assign(paste0("AOI_Raster_", i), Create_raster(AOI_event,  paste0("C:/Users/HP/OneDrive/1231116193333-Desktop/Desktop/ISEP3/Stat_spatiales/TP11/Rasterisation_", i, ".tif")))# Création du raster pour chaque année et sauvegarde en fichier GeoTIFF
}


AOI_Raster_binaire <- AOI_Raster_2020
values(AOI_Raster_binaire) <- ifelse(values(AOI_Raster_2020) >= 5, 1, 0) # Les valeurs 1 sont prises si le nombre d'evenements est supérieur à 5


writeRaster(AOI_Raster_binaire, "C:/Users/HP/OneDrive/1231116193333-Desktop/Desktop/ISEP3/Stat_spatiales/TP11/Rasterisation_2020_Binaire_EVENEMENTS.tif", format = "GTiff", overwrite = TRUE)

# Créer et binariser directement en excluant les NA
AOI_Raster_binaire[!is.na(values(AOI_Raster)) & values(AOI_Raster) >= 5] <- 1
AOI_Raster_binaire[!is.na(values(AOI_Raster)) & values(AOI_Raster) < 5] <- 0

pop <- raster("C:/Users/HP/OneDrive/1231116193333-Desktop/Desktop/Stat_explo_spatiale/Statistique-Exploratoire-Spatiale/TP4/data/WorldPop/SEN/SEN_population_v1_0_gridded.tif")

### Agréger le raster à 5 km en utilisant la somme
fact <- round(5000/100)

pop_5km <- aggregate(pop, fact = fact, fun = sum, na.rm = TRUE)


### Sauvegarder le raster agrégé


writeRaster(pop_5km, "C:/Users/HP/OneDrive/1231116193333-Desktop/Desktop/Stat_explo_spatiale/Statistique-Exploratoire-Spatiale/TP4/data/WorldPop/SEN/SEN_population_v1_0_gridded_5km.tif", format = "GTiff", overwrite = TRUE)


crs(pop_5km) # En degres (CRS=WGS84)
crs(AOI_Raster_binaire) # En metre (UTM zone 29N)


#Les systèmes de coordonnées ne sont pas les mêmes, on va donc reprojeter

### Reprojection avec méthode nearest neigbours


pop_5km_utm <- projectRaster(pop_5km, crs = crs(AOI_Raster_binaire), method = "ngb")
crs(pop_5km_utm) # C'est maintenant en en metre (UTM zone 29N)


### Verification de la correspondance de resolution


res(pop_5km_utm)
res(AOI_Raster_binaire)



### Resample pour qu'on ait également la meme dimension


pop_resampled <- resample(pop_5km_utm, AOI_Raster_binaire, method = "ngb")


### Rebinarisation stricte


pop_resampled_binary <- calc(pop_resampled, fun = function(x) {
  ifelse(x < 50, 0, 1)
})

### Vérification finale


print(unique(values(pop_resampled_binary)))



## 3- RASTER PRODUIT

### Vérifier les dimensions des deux rasters avant de pouvoir faire la multiplication


dim_AOI <- dim(AOI_Raster_binaire)
dim_pop <- dim(pop_resampled_binary)
print(paste("Dimensions du raster AOI_Raster_binaire : ", paste(dim_AOI, collapse = " x ")))
print(paste("Dimensions du raster pop_5km_binary : ", paste(dim_pop, collapse = " x ")))
print(unique(values(pop_resampled_binary))) 
print(unique(values(AOI_Raster_binaire)))


### FAISONS ACTUELLEMENT LA MULTIPLICATION


mult_raster <- AOI_Raster_binaire * pop_resampled_binary


### Vérifier un résumé des valeurs du raster résultant
writeRaster(pop_resampled_binary, "C:/Users/HP/OneDrive/Documents/GitHub/Projet_Final_Stat_Spatiale/data/Senegal/Rasters/CDI/pop_resampled_binary.tif", format = "GTiff", overwrite = TRUE)
writeRaster(mult_raster, "C:/Users/HP/OneDrive/Documents/GitHub/Projet_Final_Stat_Spatiale/data/Senegal/Rasters/CDI/mult_raster.tif", format = "GTiff", overwrite = TRUE)
plot(mult_raster)
plot(pop_resampled_binary)
plot(mult_raster)

  
a<- raster("C:/Users/HP/OneDrive/Documents/GitHub/Projet_Final_Stat_Spatiale/data/Burkina/Rasters/CDI/pop_resampled_binary.tif")
plot(a)

summary(values(mult_raster))


## 4- Calculer par admin, du CDI

### AU NIVEAU DES REGIONS


shp_region <- st_read("C:/Users/HP/OneDrive/Documents/GitHub/Projet_Final_Stat_Spatiale/data/Senegal/Shapefiles/sen_admbnda_adm1_anat_20240520.shp", quiet= TRUE)
shp_departement <- st_read("C:/Users/HP/OneDrive/Documents/GitHub/Projet_Final_Stat_Spatiale/data/Senegal/Shapefiles/sen_admbnda_adm2_anat_20240520.shp", quiet= TRUE)
shp_commune <- st_read("C:/Users/HP/OneDrive/Documents/GitHub/Projet_Final_Stat_Spatiale/data/Senegal/Shapefiles/sen_admbnda_adm3_anat_20240520.shp", quiet= TRUE)


#### Calculer le nombre de 1 dans le raster binarisé pour chaque zone administrative


pop_count_region <- extract(pop_resampled_binary, shp_region, fun = sum, na.rm = TRUE)
print(pop_count_region)


#### Calculer le nombre de 1 dans le raster produit pour chaque zone administrative


prod_count_region <- extract(mult_raster, shp_region, fun = sum, na.rm = TRUE)
print(prod_count_region)


#### Calculer le CDI pour chaque zone administrative


CDI_region <- prod_count_region / pop_count_region
plot(CDI)

a_region <-data.frame(Admin = shp_region$ADM1_FR, CDI = CDI_region)
View(a_region)

# Joindre les valeurs du CDI à la carte des régions
shp_region$CDI <- CDI_region
shp_region_utm <- st_transform(shp_region, crs = 32629)
# Charger le package ggspatial
library(ggspatial)

# Visualisation avec ggplot
ggplot(data = shp_region_utm) +
  geom_sf(aes(fill = CDI), color = "white", size = 0.2) +  # Remplir selon CDI
  scale_fill_viridis_c(option = "C", na.value = "gray") +  # Couleur pour les CDI, gérer les NA en mettant le gris
  labs(title = "Carte du CDI par Région", fill = "CDI") +
  geom_sf_text(aes(label = ADM1_FR), size = 3, color = "black", fontface = "bold") +
  annotation_north_arrow(location = "tl", which_north = "true", 
                         height = unit(1, "cm"), width = unit(1, "cm"),
                         style = north_arrow_fancy_orienteering) +  # Style de la flèche
  theme_minimal() +
  theme(legend.position = "right",  
        legend.title = element_text(size = 10),
        legend.text = element_text(size = 8)) +
  guides(fill = guide_colorbar(title = "CDI", title.position = "top", 
                               title.hjust = 0.5, barheight = 10))



# Charger les bibliothèques nécessaires
library(leaflet)
library(sf)
library(viridis) # Pour les palettes de couleurs

# Génération d'une palette de couleurs en fonction de la variable CDI
palette_CDI_region <- colorNumeric(palette = "viridis", domain = shp_region_utm$CDI, na.color = "gray")

# Carte interactive avec Leaflet
leaflet(data = shp_region_utm) %>%
  addTiles() %>%  # Ajouter une couche de fond
  addPolygons(
    fillColor = ~palette_CDI_region(CDI_region),  # Couleur de remplissage en fonction de CDI
    weight = 1,  # Bordure des régions
    color = "white",  # Couleur de la bordure
    fillOpacity = 0.7,  # Opacité du remplissage
    label = ~paste("<strong>Région:</strong>", ADM1_FR, "<br>",
                   "<strong>CDI:</strong>", round(CDI_region, 2)),  # Infobulle
    highlightOptions = highlightOptions(
      weight = 3,
      color = "blue",
      fillOpacity = 0.9,
      bringToFront = TRUE
    )
  ) %>%
  addLegend(
    pal = palette_CDI,
    values = ~CDI,
    position = "bottomright",
    title = "CDI",
    labFormat = labelFormat(suffix = ""),
    opacity = 1
  )


# Charger les bibliothèques nécessaires
library(sf)
library(leaflet)
library(viridis)

# ────────────────────────────────────────────────
# 1. Chargement des shapefiles et calculs du CDI
# ────────────────────────────────────────────────

# Charger les shapefiles
shp_region <- st_read("C:/Users/HP/OneDrive/Documents/GitHub/Projet_Final_Stat_Spatiale/data/Senegal/Shapefiles/sen_admbnda_adm1_anat_20240520.shp", quiet = TRUE)
shp_departement <- st_read("C:/Users/HP/OneDrive/Documents/GitHub/Projet_Final_Stat_Spatiale/data/Senegal/Shapefiles/sen_admbnda_adm2_anat_20240520.shp", quiet = TRUE)
shp_commune <- st_read("C:/Users/HP/OneDrive/Documents/GitHub/Projet_Final_Stat_Spatiale/data/Senegal/Shapefiles/sen_admbnda_adm3_anat_20240520.shp", quiet = TRUE)

# Calculer les valeurs de population et de production pour chaque niveau
pop_count_region <- extract(pop_resampled_binary, shp_region, fun = sum, na.rm = TRUE)
prod_count_region <- extract(mult_raster, shp_region, fun = sum, na.rm = TRUE)
CDI_region <- prod_count_region / pop_count_region

pop_count_departement <- extract(pop_resampled_binary, shp_departement, fun = sum, na.rm = TRUE)
prod_count_departement <- extract(mult_raster, shp_departement, fun = sum, na.rm = TRUE)
CDI_departement <- prod_count_departement / pop_count_departement

pop_count_commune <- extract(pop_resampled_binary, shp_commune, fun = sum, na.rm = TRUE)
prod_count_commune <- extract(mult_raster, shp_commune, fun = sum, na.rm = TRUE)
CDI_commune <- prod_count_commune / pop_count_commune

# Ajouter les valeurs CDI aux shapefiles
shp_region$CDI <- CDI_region
shp_departement$CDI <- CDI_departement
shp_commune$CDI <- CDI_commune

# ────────────────────────────────────────────────
# 2. Création des cartes interactives avec Leaflet
# ────────────────────────────────────────────────

# a. Carte des Régions
palette_CDI_region <- colorNumeric(palette = "viridis", domain = shp_region$CDI, na.color = "gray")
leaflet(data = shp_region) %>%
  addTiles() %>%
  addPolygons(
    fillColor = ~palette_CDI_region(CDI),
    weight = 1,
    color = "white",
    fillOpacity = 0.7,
    label = ~paste("<strong>Région:</strong>", ADM1_FR, "<br>",
                   "<strong>CDI:</strong>", round(CDI, 2)),
    highlightOptions = highlightOptions(
      weight = 3,
      color = "blue",
      fillOpacity = 0.9,
      bringToFront = TRUE
    )
  ) %>%
  addLegend(
    pal = palette_CDI_region,
    values = ~CDI,
    position = "bottomright",
    title = "CDI Régions",
    labFormat = labelFormat(suffix = ""),
    opacity = 1
  )

# b. Carte des Départements
palette_CDI_depart <- colorNumeric(palette = "viridis", domain = shp_departement$CDI, na.color = "gray")
leaflet(data = shp_departement) %>%
  addTiles() %>%
  addPolygons(
    fillColor = ~palette_CDI_depart(CDI),
    weight = 1,
    color = "white",
    fillOpacity = 0.7,
    label = ~paste("<strong>Département:</strong>", ADM2_FR, "<br>",
                   "<strong>CDI:</strong>", round(CDI, 2)),
    highlightOptions = highlightOptions(
      weight = 3,
      color = "blue",
      fillOpacity = 0.9,
      bringToFront = TRUE
    )
  ) %>%
  addLegend(
    pal = palette_CDI_depart,
    values = ~CDI,
    position = "bottomright",
    title = "CDI Départements",
    labFormat = labelFormat(suffix = ""),
    opacity = 1
  )

# c. Carte des Communes
palette_CDI_commune <- colorNumeric(palette = "viridis", domain = shp_commune$CDI, na.color = "gray")
leaflet(data = shp_commune) %>%
  addTiles() %>%
  addPolygons(
    fillColor = ~palette_CDI_commune(CDI),
    weight = 1,
    color = "white",
    fillOpacity = 0.7,
    label = ~paste("Commune:", ADM3_FR, " ; ",
                   "CDI:", round(CDI, 2)),
    highlightOptions = highlightOptions(
      weight = 3,
      color = "blue",
      fillOpacity = 0.9,
      bringToFront = TRUE
    )
  ) %>%
  addLegend(
    pal = palette_CDI_commune,
    values = ~CDI,
    position = "bottomright",
    title = "CDI Communes",
    labFormat = labelFormat(suffix = ""),
    opacity = 1
  )


NDBI <- raster("C:/Users/HP/Downloads/NDBI_Senegal.tif")
View(NDBI)
plot(NDBI)


NDVI <- raster("C:/Users/HP/OneDrive/Documents/GitHub/Projet_Final_Stat_Spatiale/data/Senegal/Rasters/Indices_spectraux/NDVI_Senegal.tif")
plot(NDVI)


NNDVI<- raster("C:/Users/HP/Downloads/NDVI_Burkina (1).tif")
plot(NNDVI)


