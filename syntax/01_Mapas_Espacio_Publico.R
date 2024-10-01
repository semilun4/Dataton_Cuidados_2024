#-------------------------------------------------------------------------------
# Proyecto:   Dataton Sistema de Cuidados 
# Fecha:      30/09/2024
# Autora:     Semiramis G. de la Cruz
# Descripion: Mapas del espacio publico en CDMX 
#
#-------------------------------------------------------------------------------

cat("\014")
rm(list = ls())

library("dplyr")
library(sf)
library(ggplot2)

#---- DATOS --------------------------------------------------------------------

PATH <- "C:/Users/semir/Documents/ME/Dataton_Cuidados_2024/"
DATADIR <- paste0(PATH, 'data/')

shp_estac <- st_read(paste0(DATADIR, "estacionamiento/estacionamientos.shp"), 
                     options = "ENCODING=UTF-8")
shp_manz <- st_read(paste0(DATADIR, "manzanas/poligono_manzanas_cdmx.shp"))

area_Verde <- st_read(paste0(DATADIR, "areas_verdes/ca_1.shp"))

#---- MAPA ESTACIONAMIENTO -----------------------------------------------------

ggplot() +
  geom_sf(data = shp_manz, fill = NA, color = "gray", lwd = 0.3) +  # Límites de manzanas
  geom_sf(data = shp_estac, aes(geometry = geometry), color = "#3A37A6", size = 2) +  # Puntos de estacionamientos
  labs(title = "Estacionamientos Públicos en la Ciudad de México",
       x = "Longitud", y = "Latitud") +
  theme_minimal()

shp_estac %>% 
  group_by(MUNICIPIO) %>% 
  summarise(N = n()) %>% 
  View()

#---- MAPA AREAS VERDES --------------------------------------------------------

area_Verde %>% 
  group_by(alcaldia) %>% 
  summarise(N = n()) %>% 
  View()

# Calcular los centroides de las áreas verdes
centroides_area_verde <- st_centroid(area_Verde)
# Mapa de los centroides de áreas verdes
ggplot() +
  geom_sf(data = shp_manz, fill = NA, color = "gray", lwd = 0.3) +  # Límites de manzanas
  geom_sf(data = centroides_area_verde, aes(geometry = geometry), color = "#3A37A6", size = 2) +  # Centroides de áreas verdes
  labs(title = "Áreas Verdes en la Ciudad de México",
       x = "Longitud", y = "Latitud") +
  theme_minimal()


# Definir los rangos de T_AV_M2 en 4 categorías
area_Verde$T_AV_M2_cat <- cut(area_Verde$T_AV_M2, 
                        breaks = 4, 
                        labels = c("Muy bajo", "Bajo", "Medio", "Alto"))

# Crear el mapa con 4 tonos
ggplot(data = area_Verde) +
  geom_sf(aes(fill = T_AV_M2_cat), color = "black") +  # Usar la columna categorizada
  labs(title = "Mapa de Áreas Verdes en la Ciudad de México", 
       fill = "Área Verde (m2)") + 
  scale_fill_manual(values = c("#fdae61", "#fee08b", "#66bd63", "#1a9850")) +  # Especificar 4 tonos de color
  theme_minimal()



