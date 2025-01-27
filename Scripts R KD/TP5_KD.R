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





