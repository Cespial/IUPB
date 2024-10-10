
# SISTEMAS MECATRONICOS

library(readxl)
library(dplyr)
library(tidyr)
library(tidyverse)
library(readxl)
library(janitor)
library(dplyr)
library(purrr)
library(tidyr)
library(openxlsx)
library(stringi)
library(lubridate)

# LEER DATOS --------------

# 1. CARGAR

# Definir la ruta del web-scraping totales separados
prueba_ingles <- '/Users/cristianespinal/Desktop/prueba_ingles.xlsx'

# Leer la hoja de cálculo Excel
prueba_ingles <- read_excel(prueba_ingles)

ingles_tyt <- '/Users/cristianespinal/Downloads/sis_mec_tyt.xlsx'
ingles_tyt <- read_excel(ingles_tyt)

# Cargar la base de datos
prueba_ingles <- read.csv("ruta_a_tu_archivo.csv")

# Filtrar los datos para incluir solo estrato 1 y 2 y eliminar NA
prueba_ingles <- prueba_ingles %>% 
  filter(Estrato %in% c(1, 2)) %>% 
  drop_na(Estrato)

# Verificar las primeras filas después de filtrar
head(prueba_ingles)

# Histograma de las puntuaciones solo para estrato 1 y 2
ggplot(prueba_ingles, aes(x = Puntuación)) +
  geom_histogram(binwidth = 5, fill = "#4E79A7", color = "black") +  # Usar un azul sobrio
  labs(title = "Distribución de las Puntuaciones (Estrato 1 y 2)", x = "Puntuación", y = "Frecuencia") +
  theme_minimal() +
  theme(
    plot.title = element_text(size=18, face="bold", hjust = 0.5),
    axis.title.x = element_text(size=14),
    axis.title.y = element_text(size=14)
  ) +
  geom_vline(aes(xintercept = mean(prueba_ingles$Puntuación)), color = "#E15759", linetype = "dashed", size = 1) +  # Línea de media
  annotate("text", x = mean(prueba_ingles$Puntuación) + 1, y = 5, label = paste("Media =", round(mean(prueba_ingles$Puntuación), 2)), color = "#E15759", size = 5)

# Boxplot de puntuaciones para estrato 1 y 2
ggplot(prueba_ingles, aes(x = as.factor(Estrato), y = Puntuación)) +
  geom_boxplot(fill = "#76B7B2", color = "black") +  # Usar un verde suave
  labs(title = "Distribución de Puntuaciones por Estrato (1 y 2)", x = "Estrato", y = "Puntuación") +
  theme_minimal() +
  theme(
    plot.title = element_text(size=18, face="bold", hjust = 0.5),
    axis.title.x = element_text(size=14),
    axis.title.y = element_text(size=14)
  ) +
  scale_x_discrete(labels = c("Estrato 1", "Estrato 2"))


# Calcular la media de puntuaciones por estrato
media_por_estrato <- aggregate(Puntuación ~ Estrato, data = prueba_ingles, FUN = mean)

# Gráfico de barras para estrato 1 y 2
ggplot(media_por_estrato, aes(x = as.factor(Estrato), y = Puntuación)) +
  geom_bar(stat = "identity", fill = "#59A14F") +  # Usar un verde sobrio
  labs(title = "Media de Puntuaciones por Estrato (1 y 2)", x = "Estrato", y = "Puntuación Promedio") +
  theme_minimal() +
  theme(
    plot.title = element_text(size=18, face="bold", hjust = 0.5),
    axis.title.x = element_text(size=14),
    axis.title.y = element_text(size=14)
  ) +
  geom_text(aes(label = round(Puntuación, 2)), vjust = -0.5, size = 5) +  # Etiquetas con la media en cada barra
  scale_x_discrete(labels = c("Estrato 1", "Estrato 2"))

# Scatter plot de Puntuaciones por Estrato 1 y 2
ggplot(prueba_ingles, aes(x = as.factor(Estrato), y = Puntuación)) +
  geom_jitter(color = "#F28E2B", width = 0.2) +  # Usar un naranja suave
  labs(title = "Dispersión de Puntuaciones por Estrato (1 y 2)", x = "Estrato", y = "Puntuación") +
  theme_minimal() +
  theme(
    plot.title = element_text(size=18, face="bold", hjust = 0.5),
    axis.title.x = element_text(size=14),
    axis.title.y = element_text(size=14)
  ) +
  scale_x_discrete(labels = c("Estrato 1", "Estrato 2"))

# Gráfico de violín para la distribución de Puntuaciones por Estrato (1 y 2)
ggplot(prueba_ingles, aes(x = as.factor(Estrato), y = Puntuación)) +
  geom_violin(fill = "#EDC949", color = "black") +  # Usar un amarillo suave
  labs(title = "Distribución de Puntuaciones por Estrato (1 y 2)", x = "Estrato", y = "Puntuación") +
  theme_minimal() +
  theme(
    plot.title = element_text(size=18, face="bold", hjust = 0.5),
    axis.title.x = element_text(size=14),
    axis.title.y = element_text(size=14)
  ) +
  scale_x_discrete(labels = c("Estrato 1", "Estrato 2"))

#######

# Filtrar los datos para solo estratos 1 y 2
prueba_ingles_filtrada <- prueba_ingles %>% 
  filter(Estrato %in% c(1, 2)) %>% 
  drop_na(Estrato)

# Estadísticas descriptivas generales para la puntuación
estadisticas_generales <- summary(prueba_ingles_filtrada$Puntuación)
print(estadisticas_generales)

# Media, mediana, y desviación estándar por estrato
media_por_estrato <- aggregate(Puntuación ~ Estrato, data = prueba_ingles_filtrada, FUN = mean)
mediana_por_estrato <- aggregate(Puntuación ~ Estrato, data = prueba_ingles_filtrada, FUN = median)
desviacion_por_estrato <- aggregate(Puntuación ~ Estrato, data = prueba_ingles_filtrada, FUN = sd)

# Mínimo, máximo, y cuartiles por estrato
cuartiles_por_estrato <- aggregate(Puntuación ~ Estrato, data = prueba_ingles_filtrada, FUN = quantile)

# Conteo de observaciones por estrato
conteo_por_estrato <- prueba_ingles_filtrada %>% 
  group_by(Estrato) %>% 
  summarise(n = n())

# Mostrar resultados
cat("Estadísticas Generales de la Puntuación:\n")
print(estadisticas_generales)

cat("\nMedia de Puntuaciones por Estrato:\n")
print(media_por_estrato)

cat("\nMediana de Puntuaciones por Estrato:\n")
print(mediana_por_estrato)

cat("\nDesviación Estándar de Puntuaciones por Estrato:\n")
print(desviacion_por_estrato)

cat("\nCuartiles de Puntuaciones por Estrato:\n")
print(cuartiles_por_estrato)

cat("\nConteo de Observaciones por Estrato:\n")
print(conteo_por_estrato)

#############

# Cargar paquetes necesarios
library(ggplot2)
library(dplyr)

# Filtrar datos para Estrato 1 y Estrato 2
ingles_tyt_filtrado <- ingles_tyt %>%
  filter(FAMI_ESTRATOVIVIENDA %in% c("Estrato 1", "Estrato 2"))

# Revisar las primeras filas después del filtrado
head(ingles_tyt_filtrado)

# Histograma de puntuaciones por estrato
ggplot(ingles_tyt_filtrado, aes(x = MOD_INGLES_PUNT, fill = FAMI_ESTRATOVIVIENDA)) +
  geom_histogram(binwidth = 5, color = "black", alpha = 0.7, position = "identity") +
  labs(title = "Distribución de Puntuaciones de Inglés (SaberTyT)", x = "Puntuación de Inglés", y = "Frecuencia") +
  scale_fill_manual(values = c("#4E79A7", "#F28E2B")) +  # Colores sobrios para estratos 1 y 2
  theme_minimal() +
  theme(plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14)) +
  guides(fill = guide_legend(title = "Estrato"))


# Boxplot de puntuaciones por estrato
ggplot(ingles_tyt_filtrado, aes(x = FAMI_ESTRATOVIVIENDA, y = MOD_INGLES_PUNT, fill = FAMI_ESTRATOVIVIENDA)) +
  geom_boxplot() +
  labs(title = "Distribución de Puntuaciones por Estrato (SaberTyT)", x = "Estrato", y = "Puntuación de Inglés") +
  scale_fill_manual(values = c("#76B7B2", "#FF9DA7")) +
  theme_minimal() +
  theme(plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        legend.position = "none") +
  scale_x_discrete(labels = c("Estrato 1", "Estrato 2"))

# Calcular la media de puntuaciones por estrato
media_por_estrato_tyt <- ingles_tyt_filtrado %>%
  group_by(FAMI_ESTRATOVIVIENDA) %>%
  summarise(Puntuacion_Media = mean(MOD_INGLES_PUNT))

# Gráfico de barras de la media de puntuaciones
ggplot(media_por_estrato_tyt, aes(x = FAMI_ESTRATOVIVIENDA, y = Puntuacion_Media, fill = FAMI_ESTRATOVIVIENDA)) +
  geom_bar(stat = "identity", color = "black", alpha = 0.7) +
  labs(title = "Media de Puntuaciones de Inglés por Estrato (SaberTyT)", x = "Estrato", y = "Puntuación Media") +
  scale_fill_manual(values = c("#4E79A7", "#F28E2B")) +
  theme_minimal() +
  theme(plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        legend.position = "none")

# Scatter plot de puntuaciones por estrato
ggplot(ingles_tyt_filtrado, aes(x = FAMI_ESTRATOVIVIENDA, y = MOD_INGLES_PUNT, color = FAMI_ESTRATOVIVIENDA)) +
  geom_jitter(width = 0.2, size = 2, alpha = 0.7) +
  labs(title = "Dispersión de Puntuaciones por Estrato (SaberTyT)", x = "Estrato", y = "Puntuación de Inglés") +
  scale_color_manual(values = c("#4E79A7", "#F28E2B")) +
  theme_minimal() +
  theme(plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        legend.position = "none")

# Gráfico de violín de puntuaciones por estrato
ggplot(ingles_tyt_filtrado, aes(x = FAMI_ESTRATOVIVIENDA, y = MOD_INGLES_PUNT, fill = FAMI_ESTRATOVIVIENDA)) +
  geom_violin(trim = FALSE, alpha = 0.7) +
  labs(title = "Distribución de Puntuaciones por Estrato (SaberTyT)", x = "Estrato", y = "Puntuación de Inglés") +
  scale_fill_manual(values = c("#EDC949", "#76B7B2")) +
  theme_minimal() +
  theme(plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        legend.position = "none") +
  scale_x

# Cargar los paquetes necesarios
library(dplyr)
library(ggplot2)

# Filtrar los datos para estratos 1 y 2
ingles_tyt_filtrado <- ingles_tyt %>% 
  filter(FAMI_ESTRATOVIVIENDA %in% c("Estrato 1", "Estrato 2")) %>% 
  mutate(Estrato = as.factor(FAMI_ESTRATOVIVIENDA))

# Verificar los primeros datos después de filtrar
head(ingles_tyt_filtrado)

# Estadísticas descriptivas generales
estadisticas_generales <- summary(ingles_tyt_filtrado$MOD_INGLES_PUNT)
print(estadisticas_generales)

# Media, mediana y desviación estándar por estrato
media_por_estrato <- ingles_tyt_filtrado %>%
  group_by(Estrato) %>%
  summarise(Media = mean(MOD_INGLES_PUNT, na.rm = TRUE),
            Mediana = median(MOD_INGLES_PUNT, na.rm = TRUE),
            Desviacion = sd(MOD_INGLES_PUNT, na.rm = TRUE))

print(media_por_estrato)

# Cuartiles por estrato
cuartiles_por_estrato <- ingles_tyt_filtrado %>%
  group_by(Estrato) %>%
  summarise(Min = min(MOD_INGLES_PUNT, na.rm = TRUE),
            Q1 = quantile(MOD_INGLES_PUNT, 0.25, na.rm = TRUE),
            Mediana = median(MOD_INGLES_PUNT, na.rm = TRUE),
            Q3 = quantile(MOD_INGLES_PUNT, 0.75, na.rm = TRUE),
            Max = max(MOD_INGLES_PUNT, na.rm = TRUE))

print(cuartiles_por_estrato)

# Conteo de observaciones por estrato
conteo_por_estrato <- ingles_tyt_filtrado %>%
  group_by(Estrato) %>%
  summarise(Conteo = n())

print(conteo_por_estrato)

cat("\n### Diagnóstico Completo de los Resultados SaberTyT ###\n\n")

# Descripción general de las estadísticas
cat("La base de datos contiene", nrow(ingles_tyt_filtrado), "observaciones de estudiantes que tomaron la prueba SaberTyT en inglés, con datos diferenciados por estratos 1 y 2. \n")
cat("Las puntuaciones de inglés varían entre", min(ingles_tyt_filtrado$MOD_INGLES_PUNT), "y", max(ingles_tyt_filtrado$MOD_INGLES_PUNT), ".\n")

# Diagnóstico por estrato
for (estrato in unique(ingles_tyt_filtrado$Estrato)) {
  cat("\nPara el", estrato, ":\n")
  
  media <- media_por_estrato[media_por_estrato$Estrato == estrato, "Media"]
  mediana <- media_por_estrato[media_por_estrato$Estrato == estrato, "Mediana"]
  sd_estrato <- media_por_estrato[media_por_estrato$Estrato == estrato, "Desviacion"]
  min_val <- cuartiles_por_estrato[cuartiles_por_estrato$Estrato == estrato, "Min"]
  q1 <- cuartiles_por_estrato[cuartiles_por_estrato$Estrato == estrato, "Q1"]
  q3 <- cuartiles_por_estrato[cuartiles_por_estrato$Estrato == estrato, "Q3"]
  max_val <- cuartiles_por_estrato[cuartiles_por_estrato$Estrato == estrato, "Max"]
  n <- conteo_por_estrato[conteo_por_estrato$Estrato == estrato, "Conteo"]
  
  cat("   - La media de las puntuaciones es de", round(media, 2), ".\n")
  cat("   - La mediana de las puntuaciones es de", round(mediana, 2), ".\n")
  cat("   - La desviación estándar es de", round(sd_estrato, 2), ", indicando la dispersión de las puntuaciones respecto a la media.\n")
  cat("   - Las puntuaciones varían entre un mínimo de", min_val, "y un máximo de", max_val, ".\n")
  cat("   - El rango intercuartílico es [", q1, "-", q3, "], lo que significa que el 50% central de las puntuaciones cae en este rango.\n")
  cat("   - Este estrato cuenta con un total de", n, "observaciones.\n")
}

##########

# Generar el diagnóstico correctamente asegurando que los valores extraídos son numéricos
cat("\n### Diagnóstico Completo de los Resultados SaberTyT ###\n\n")

# Descripción general de las estadísticas
cat("La base de datos contiene", nrow(ingles_tyt_filtrado), "observaciones de estudiantes que tomaron la prueba SaberTyT en inglés, con datos diferenciados por estratos 1 y 2. \n")
cat("Las puntuaciones de inglés varían entre", min(ingles_tyt_filtrado$MOD_INGLES_PUNT), "y", max(ingles_tyt_filtrado$MOD_INGLES_PUNT), ".\n")

# Diagnóstico por estrato
for (estrato in unique(ingles_tyt_filtrado$Estrato)) {
  cat("\nPara el", estrato, ":\n")
  
  # Asegurarse de extraer los valores como numéricos
  media <- as.numeric(media_por_estrato[media_por_estrato$Estrato == estrato, "Media"])
  mediana <- as.numeric(media_por_estrato[media_por_estrato$Estrato == estrato, "Mediana"])
  sd_estrato <- as.numeric(media_por_estrato[media_por_estrato$Estrato == estrato, "Desviacion"])
  min_val <- as.numeric(cuartiles_por_estrato[cuartiles_por_estrato$Estrato == estrato, "Min"])
  q1 <- as.numeric(cuartiles_por_estrato[cuartiles_por_estrato$Estrato == estrato, "Q1"])
  q3 <- as.numeric(cuartiles_por_estrato[cuartiles_por_estrato$Estrato == estrato, "Q3"])
  max_val <- as.numeric(cuartiles_por_estrato[cuartiles_por_estrato$Estrato == estrato, "Max"])
  n <- as.numeric(conteo_por_estrato[conteo_por_estrato$Estrato == estrato, "Conteo"])
  
  # Generar el diagnóstico
  cat("   - La media de las puntuaciones es de", round(media, 2), ".\n")
  cat("   - La mediana de las puntuaciones es de", round(mediana, 2), ".\n")
  cat("   - La desviación estándar es de", round(sd_estrato, 2), ", indicando la dispersión de las puntuaciones respecto a la media.\n")
  cat("   - Las puntuaciones varían entre un mínimo de", min_val, "y un máximo de", max_val, ".\n")
  cat("   - El rango intercuartílico es [", q1, "-", q3, "], lo que significa que el 50% central de las puntuaciones cae en este rango.\n")
  cat("   - Este estrato cuenta con un total de", n, "observaciones.\n")
}

create_github_token()
gitcreds::gitcreds_set()

which git
## /usr/bin/git




cd '/Users/cristianespinal/Library/CloudStorage/GoogleDrive-cristian.espinal@pascualbravo.edu.co/Unidades compartidas/UVIC - Unidad de Vigilancia Tecnologica e Inteligencia Competitiva/6. Lago de datos/CODE'

