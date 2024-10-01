#-------------------------------------------------------------------------------
# Fecha:      30/09/2024
# Autora:     Semiramis G. de la Cruz
#
#-------------------------------------------------------------------------------

cat("\014")
rm(list = ls())

library(ggplot2)
library(tidyr)
library(dplyr)
library(corrplot)
library(tibble)

#---- DATOS --------------------------------------------------------------------

PATH <- "C:/Users/semir/Documents/ME/Dataton_Cuidados_2024/"
DATADIR <- paste0(PATH, 'data/')

# DATOS DEL MAPA DE CUIDADOS CDMX
datos_delegaciones_cuidados <- read.csv(paste0(DATADIR, 
                                              "datos_delegaciones_cuidados.csv"), 
                                       header = TRUE, sep = ",", 
                                       fileEncoding = "UTF-8")


#---- LIMPIEZA DE DATOS --------------------------------------------------------------------

names(datos_delegaciones_cuidados)
str(datos_delegaciones_cuidados)

# Limpiar las columnas 
datos_delegaciones_cuidados <- datos_delegaciones_cuidados %>%
  mutate(
    Poblacion_Total = gsub(",", "", Poblacion_Total) %>% as.numeric(),
    Poblacion_Mayores_65 = gsub(",", "", Poblacion_Mayores_65) %>% as.numeric(),
    Mujeres_Ocupadas = gsub(",", "", Mujeres_Ocupadas) %>% as.numeric(),
    Mujeres_Inactivas_Economicamente = gsub(",", "", Mujeres_Inactivas_Economicamente) %>% as.numeric(),
    Poblacion_Objetivo = gsub(",", "", Poblacion_Objetivo) %>% as.numeric(),
    Establecimientos_Cuidados_Indirectos = gsub(",", "", Establecimientos_Cuidados_Indirectos) %>% as.numeric(),
    Total_Establecimientos = gsub(",", "", Total_Establecimientos) %>% as.numeric(),
    Tasa_Demanda_Potencial_Mayores_65 = gsub(",", "", Tasa_Demanda_Potencial_Mayores_65) %>% as.numeric(),
    
    Total_Poblacion_Requiere_Cuidados = gsub("%", "", Total_Poblacion_Requiere_Cuidados) %>% as.numeric() / 100,
    Total_Poblacion_Requiere_Cuidados_65 = gsub("%", "", Total_Poblacion_Requiere_Cuidados_65) %>% as.numeric() / 100,
    Poblacion_en_Probreza = gsub("%", "", Poblacion_en_Probreza) %>% as.numeric()
  )

# Verifica el resultado
str(datos_delegaciones_cuidados)
summary(datos_delegaciones_cuidados)





# Gráfico de barras por delegación
ggplot(datos_delegaciones_cuidados, aes(x = reorder(Alcaldia, Total_Poblacion_Requiere_Cuidados_65), 
                                        y = Total_Poblacion_Requiere_Cuidados_65)) +
  geom_bar(stat = "identity", fill = "#3A37A6") +
  coord_flip() +
  theme_minimal() +
  labs(title = "Total de Población que Requiere Cuidados (65+) por Delegación",
       x = "Delegación", y = "Total Población que Requiere Cuidados (65+)")





# Selección de variables numéricas
numericas <- datos_delegaciones_cuidados %>%
  select(-Areas_verdes, -Estacionamientos, 
         -Tasa_Demanda_Potencial_Mayores_65) %>% 
  select_if(is.numeric)
correlaciones <- cor(numericas, use = "complete.obs")
correlacion_requiere_cuidados_65 <- correlaciones[, "Total_Poblacion_Requiere_Cuidados_65"]

# Convertir las correlaciones en un dataframe para graficar
correlacion_df <- as.data.frame(correlacion_requiere_cuidados_65) %>%
  rownames_to_column(var = "Variable") %>%
  rename(Correlacion = correlacion_requiere_cuidados_65)

# Graficar correlaciones
ggplot(correlacion_df, aes(x = reorder(Variable, Correlacion), y = Correlacion)) +
  geom_bar(stat = "identity", fill = "#3A37A6") +
  coord_flip() +
  theme_minimal() +
  labs(title = "Correlación de Variables con Total_Poblacion_Requiere_Cuidados_65",
       x = "Variable", y = "Correlación")


# Diagrama de dispersión entre dos variables
ggplot(datos_delegaciones_cuidados, aes(x = Mujeres_Ocupadas, 
                                        y = Total_Poblacion_Requiere_Cuidados_65)) +
  geom_point(color = "blue", size = 3) +
  theme_minimal() +
  labs(title = "Relación entre Mujeres Ocupadas y Población Requiere Cuidados (65+)",
       x = "Mujeres Ocupadas", y = "Total Población que Requiere Cuidados (65+)")


data <- data.frame(
  Category = c(
    "Personas con discapacidad o dependencia",
    "Personas sin discapacidad o dependencia",
    "Personas de 60 años y más (Total)"
  ),
  `No recibe cuidados (%)` = c(34.8, 77.6, 71.2),
  `No recibe cuidados (millones)` = c(1.0, 13.2, 14.2),
  `Recibe cuidados (%)` = c(65.2, 22.4, 28.8),
  `Recibe cuidados (millones)` = c(1.9, 3.8, 5.7)
)

#---- RESULTADOS ---------------------------------------------------------------
datos_ordenados <- datos_delegaciones_cuidados %>%
  arrange(desc(Poblacion_en_Probreza), 
          desc(Tasa_Demanda_Potencial_Mujeres_Ocupadas), 
          desc(Estacionamientos)) %>% 
  select(Alcaldia, Poblacion_en_Probreza, Tasa_Demanda_Potencial_Mujeres_Ocupadas,
         Estacionamientos)
