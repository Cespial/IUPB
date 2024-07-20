# Econometría

# Cargando las bibliotecas necesarias
library(tidyverse)
library(broom)
library(lmtest)
library(sandwich)
library(dplyr)

# 1. CARGE DE LOS DATOS ------------------------------------------------------------------------------------------------------------------------------

# Establece la ruta donde se encuentran los archivos .csv
ruta <- "/Users/cristianespinal/Library/CloudStorage/GoogleDrive-cristianjosue07@gmail.com/Mi unidad/University/Economía Aplicada/MAE. Econometría/Proyecto/Mutate/Ocupados"

# Lista todos los archivos .csv en el directorio
archivos_csv <- list.files(path = ruta, pattern = "\\.CSV$", full.names = TRUE)

# Lee todos los archivos .csv con el separador ";" y los combina en un solo dataframe
geih_ocupados <- lapply(archivos_csv, read.csv, sep = ";") %>% bind_rows()

# Exporta el dataframe combinado a un nuevo archivo .csv en la misma ruta con el separador ";"
write.csv(datos_agrupados, file = paste0(ruta, "/geih_ocupados.csv"), row.names = FALSE, sep = ";")

# 2. PREPARACIÓN DE LOS DATOS ------------------------------------------------------------------------------------------------------------------------------

# 2.1. SUBSET DE DATOS -- OCUPADOS

geih_ocupados_m1 <- select(geih_ocupados, PERIODO, MES, PER, DIRECTORIO, SECUENCIA_P, 
                           ORDEN, HOGAR, REGIS, AREA, CLASE, FEX_C18, DPTO, P1800, P6426, 
                           P6422, P3047, P6460, P6440, P3069, P6426, RAMA2D_R4, P6422, INGLABO, 
                           P1802, P6765, P1801S2, P6880, P1805, P1881,P6410, P515, P6780)

# 2.2. SUBSET DE DATOS -- OCUPADOS FORMALES CON SUBORDINACIÓN

geih_ocupados_m1 <- geih_ocupados_m1 %>%
  # Filtrar las filas según las condiciones especificadas
  filter(P1800 != "1", P6426 != "999", P6426 != "000", P3047 != "2", P6440 != "1", P3069 != "1", P3069 != "2", P3069 != "3") %>%
  # Transformar los valores de P3047 donde 3 y 4 se convierten en 2
  mutate(P3047 = case_when(
    P3047 == "3" ~ "2",
    P3047 == "4" ~ "2",
    TRUE ~ as.character(P3047)
  ))

# 3. ESTIMACIÓN ------------------------------------------------------------------------------------------------------------------------------

library(dplyr)
library(caret)
library(glmnet)

# Preparar los datos (filtrando valores extremos o no informativos)
geih_ocupados_m1 <- geih_ocupados_m1 %>% 
  filter(P6426 != "999", P6426 != "000") %>%
  mutate(
    P6426 = as.numeric(P6426),
    INGLABO = as.numeric(INGLABO),
    P3047 = factor(P3047),
    P1800 = factor(P1800),
    P1802 = factor(P1802),
    P1805 = factor(P1805),
    P6460 = factor(P6460),
    P6422 = factor(P6422),
    P515 = factor(P515),
    P3047 = factor(P3047, levels = c("1", "2")),
    P3069 = factor(P3069)
  )

# Modelo de regresión logística
set.seed(123)
train_index <- createDataPartition(geih_ocupados_m1$P3047, p = 0.8, list = FALSE)
train_data <- geih_ocupados_m1[train_index, ]
test_data <- geih_ocupados_m1[-train_index, ]

# Ajustar el modelo de regresión logística excluyendo variables problemáticas
if (all(sapply(train_data[, c("P3047", "P3069", "P1802", "P1805", "P515")], function(x) length(levels(x)) > 1))) {
  modelo_logistico <- glm(P3047 ~ INGLABO + P1802 + P1805 + P515 + P3069, 
                          data = train_data, family = binomial())
  summary(modelo_logistico)
} else {
  print("Al menos una variable factorial tiene menos de dos niveles en el conjunto de entrenamiento.")
}

# Resumen del modelo
summary(modelo_logistico)

# Predicciones y evaluación del modelo
predictions <- predict(modelo_logistico, test_data, type = "response")
pred_label <- ifelse(predictions > 0.2, "1", "2")
pred_label <- factor(pred_label, levels = levels(test_data$P3047))

# Ahora calcula la matriz de confusión
library(caret)  # Asegúrate de que la biblioteca caret está cargada
conf_mat <- confusionMatrix(pred_label, test_data$P3047)
print(conf_mat)

# 4. ESTIMACIÓN POR GRUPOS CIIU

# Asegurándonos de que RAMA2D_R4 sea tratada como un string
geih_ocupados_m1$RAMA2D_R4 <- as.character(geih_ocupados_m1$RAMA2D_R4)

# Crear una nueva columna con el primer dígito
geih_ocupados_m1$FirstDigit <- substr(geih_ocupados_m1$RAMA2D_R4, 1, 1)

# Iterar sobre los dígitos del 1 al 9 y aplicar el modelo de regresión logística
results <- list()  # Para almacenar los resultados de cada modelo

for(digit in 1:9) {
  # Filtrar los datos para cada primer dígito
  subset_data <- filter(geih_ocupados_m1, FirstDigit == as.character(digit))
  
  # Correr el modelo si hay suficientes datos
  if(nrow(subset_data) > 10) {  # Asegúrate de tener suficientes filas para estimar el modelo
    model <- glm(P3047 ~ INGLABO + P1802 + P1805 + P515 + P3069, family = binomial(), data = subset_data)
    results[[as.character(digit)]] <- summary(model)  # Guardar el resumen del modelo
  } else {
    results[[as.character(digit)]] <- "Not enough data"
  }
}

# Imprimir o examinar los resultados
results


