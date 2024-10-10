
# CREACIÓN DE GRUPOS DE INVESTIGACIÓN

# CALCULAR LOS TITULOS ÚNICOS SIN LINEA

# 4. WEB.SCRAPPING TRANSFORMACIÓN

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

web_scraping_raw %>%
  list(
    names(.),
    str = str(.)
  )

web_scraping_clean %>%
  list(
    names(.),
    str = str(.)
  )

# LEER DATOS --------------

# 1. CARGAR WEB-SCRAPING

# Definir la ruta del web-scraping totales separados
web_scraping_raw <- '../DATA CONSOLIDADA/web_scraping_totales_separados.xlsx'

# Leer la hoja de cálculo Excel
web_scraping_raw <- read_excel(web_scraping_raw)

# Leer la hoja de cálculo Excel
id_investigacion_docente <- "/Users/cristianespinal/Downloads/id_investigacion_docentes.xlsx"

# Leer excel
id_investigacion_docente <- read_excel(id_investigacion_docente)

# Ver las primeras filas del archivo para verificar la carga
head(id_investigacion_docente)

names(web_scraping_raw)
names(id_investigacion_docente)

# Filtrar las líneas no identificadas, NA, No aplica, y undefined
lineas_no_identificadas <- web_scraping_raw %>%
  filter(Linea %in% c("Línea no identificada") | is.na(Linea))

# Calcular el número único de títulos en esas líneas
num_titulos_unicos_lineas_no_identificadas <- lineas_no_identificadas %>%
  summarise(Titulos_Unicos = n_distinct(Titulo)) %>%
  pull(Titulos_Unicos)  # Extraer el valor

# Mostrar el resultado
cat("Número único de títulos en las líneas no identificadas:", num_titulos_unicos_lineas_no_identificadas, "\n")

# 2. SEPARAR LAS LINEAS COMPLETAS ---------

library(dplyr)
library(tidyr)
library(stringr)

# Agregar "Línea no identificada" a las líneas completas
lineas_completas <- c("Diseño Sostenible", "Pensamiento Social", "Agroambiental", "Desarrollo Sostenible",
                      "Energías Renovables y Sistemas Eléctricos", "Materiales Sostenibles",
                      "Movilidad Sostenible", "Sistemas Inteligentes Sostenibles", "Gestión Energética",
                      "Gestión del Mantenimiento", "Materiales y Procesos en Ingeniería", "Sistemas de Potencia",
                      "Gestión de proyectos con orientación a los ODS", "Gestión del conocimiento y de la innovación",
                      "Logística y cadena de suministro", "Productividad y calidad", "Educación y TIC",
                      "Instrumentación Científica e Industrial", "Modelado Computacional", "Nanotecnología",
                      "Telecomunicaciones", "No aplica", "Línea no identificada")

# Unir las líneas completas en un patrón de regex para la separación
patron_regex <- paste0("(", paste(lineas_completas, collapse = "|"), ")")

# Expandir la columna `Linea` en nuevas filas
web_scraping_clean <- web_scraping_raw %>%
  rowwise() %>%
  mutate(Linea = list(str_extract_all(Linea, patron_regex)[[1]])) %>%
  unnest_longer(Linea) %>%
  ungroup()

# Verificar el resultado
print(web_scraping_clean)

# 3. TABLA DE RESUMEN ---------

# Crear el resumen para cada métrica

# Conteo de títulos y autores únicos por línea
titulos_y_autores_unicos_por_linea <- web_scraping_clean %>%
  group_by(Linea) %>%
  summarise(
    Titulos_Unicos = n_distinct(Titulo),
    Autores_Unicos = n_distinct(id)
  ) %>%
  ungroup()

# Número de títulos únicos
titulos_unicos <- web_scraping_clean %>%
  summarise(Numero_Titulos_Unicos = n_distinct(Titulo))

# Número de autores únicos
autores_unicos <- web_scraping_clean %>%
  summarise(Numero_Autores_Unicos = n_distinct(id))

# Número de productos por año
productos_por_ano <- web_scraping_clean %>%
  group_by(Año) %>%
  summarise(Numero_Productos = n()) %>%
  ungroup()

# Número de "Línea no identificada"
linea_no_identificada <- web_scraping_clean %>%
  filter(Linea == "Línea no identificada") %>%
  summarise(Numero_Linea_No_Identificada = n())

# Número de autores asociados a "Línea no identificada"
autores_linea_no_identificada <- web_scraping_clean %>%
  filter(Linea == "Línea no identificada") %>%
  summarise(Numero_Autores_Linea_No_Identificada = n_distinct(Autores))

# Número de títulos únicos asociados a "Línea no identificada"
titulos_linea_no_identificada <- web_scraping_clean %>%
  filter(Linea == "Línea no identificada") %>%
  summarise(Numero_Titulos_Linea_No_Identificada = n_distinct(Titulo))

# Número de títulos únicos y autores por "Nombre del grupo"
titulos_y_autores_por_grupo <- web_scraping_clean %>%
  group_by(`Nombre del grupo`) %>%
  summarise(
    Numero_Titulos_Unicos = n_distinct(Titulo),
    Numero_Autores_Unicos = n_distinct(Autores)
  ) %>%
  ungroup()

# Número de autores únicos del Pascual Bravo (autores que tienen un valor en `id`)
autores_pascual_bravo <- web_scraping_clean %>%
  filter(!is.na(id)) %>%
  summarise(Numero_Autores_Pascual_Bravo = n_distinct(Autores))

# Mostrar el resumen utilizando `cat`
cat("Resumen de la base de datos:\n")
cat("1. Número de productos y autores únicos por línea:\n")
print(titulos_y_autores_unicos_por_linea, n = 22)

cat("\n2. Número de títulos únicos:\n")
print(titulos_unicos)

cat("\n3. Número de autores únicos:\n")
print(autores_unicos)

cat("\n4. Número de autores únicos del Pascual Bravo:\n")
print(autores_pascual_bravo)

cat("\n5. Número de productos por año:\n")
print(productos_por_ano, n = 21)

cat("\n6. Número de 'Línea no identificada':\n")
print(linea_no_identificada)

cat("\n7. Número de autores asociados a 'Línea no identificada':\n")
print(autores_linea_no_identificada)

cat("\n8. Número de títulos únicos asociados a 'Línea no identificada':\n")
print(titulos_linea_no_identificada)

cat("\n9. Número de títulos únicos y autores por 'Nombre del grupo':\n")
print(titulos_y_autores_por_grupo)

# 4. ÍNDICE DE TALENTO Y PRODUCCIÓN ----------

# Crear función para el procesamiento y normalización considerando productos e identificaciones
procesar_conteo <- function(data, grupo_var) {
  # Filtrar para excluir las filas con "Línea no identificada"
  data_filtrada <- data %>%
    filter(Linea != "Línea no identificada")
  
  # Contar por grupo_var cuántas identificaciones únicas (Talento)
  talento_conteo <- data_filtrada %>%
    group_by({{ grupo_var }}) %>%
    summarise(Talento = n_distinct(id, na.rm = TRUE))
  
  # Contar el número único de productos por grupo_var (Producción)
  produccion_conteo <- data_filtrada %>%
    group_by({{ grupo_var }}) %>%
    summarise(Producción = n_distinct(Titulo, na.rm = TRUE))
  
  # Unir los conteos de Talento y Producción
  conteo <- left_join(talento_conteo, produccion_conteo, by = as_label(enquo(grupo_var)))
  
  # Crear una variable que sea un índice de productividad (Producción / Talento)
  conteo <- conteo %>%
    mutate(Indice_Productividad = Producción / Talento)
  
  # Asegurar que el índice de productividad esté entre 0 y 1
  conteo <- conteo %>%
    mutate(Indice_Productividad = (Indice_Productividad - min(Indice_Productividad, na.rm = TRUE)) / 
             (max(Indice_Productividad, na.rm = TRUE) - min(Indice_Productividad, na.rm = TRUE)))
  
  # Crear variable instrumento_talento normalizando Talento de 0 a 1
  conteo <- conteo %>%
    mutate(instrumento_talento = (Talento - min(Talento, na.rm = TRUE)) / 
             (max(Talento, na.rm = TRUE) - min(Talento, na.rm = TRUE)))
  
  # Crear variable instrumento_producción normalizando Producción de 0 a 1
  conteo <- conteo %>%
    mutate(instrumento_producción = (Producción - min(Producción, na.rm = TRUE)) / 
             (max(Producción, na.rm = TRUE) - min(Producción, na.rm = TRUE)))
  
  return(conteo)
}

# Verificar el resultado
print(indice_talento_produccion)

# Aplicaciones del indice
indices_grupos <- procesar_conteo(web_scraping_clean, `Nombre del grupo`)
indices_categoria <- procesar_conteo(web_scraping_clean, `Categoria`)
indices_tipologia <- procesar_conteo(web_scraping_clean, `Tipologia`)
indices_tipologia_2 <- procesar_conteo(web_scraping_clean, `Tipologia 2`)
indices_nme_pd_categoria <- procesar_conteo(web_scraping_clean, `NME_CATEGORIA_PD`)
indices_linea <- procesar_conteo(web_scraping_clean, `Linea`)

# Aplicaciones del indice
indices_grupos <- procesar_conteo(web_scraping_raw, `Nombre del grupo`)

# GUARDAR DATOS 
guardar_datos(indices_grupos, path_base, "indices_grupos")
guardar_datos(indices_categoria, path_base, "indices_categoria")
guardar_datos(indices_tipologia, path_base, "indices_tipologia")
guardar_datos(indices_tipologia_2, path_base, "indices_tipologia_2")
guardar_datos(indices_nme_pd_categoria, path_base, "indices_nme_pd_categoria")
guardar_datos(indices_linea, path_base, "indices_linea")

# 5. ÍNDICE DE PRODUCCIÓN CIENTIFICA ----------

# Función para calcular el índice de producción considerando las líneas sin necesidad de definirlas ni separarlas
calcular_indice_produccion <- function(data) {
  
  # Calcular el número de productos (Conteo) y el talento por grupo de variables sin separar ni filtrar previamente
  data_conteo <- data %>%
    group_by(Linea, Categoria, Tipologia, `Tipologia 2`) %>%
    summarise(
      Conteo = n_distinct(Titulo),  # Contar el número de productos distintos
      Talento = n_distinct(id, na.rm = TRUE),  # Contar el número de investigadores únicos usando `id`
      .groups = 'drop'  # Evitar agrupaciones automáticas
    )
  
  # Calcular subíndices ajustados según la categoría y tipo, con pesos específicos para ACTIVIDADES DE FORMACIÓN y otras
  subindices <- data_conteo %>%
    mutate(
      Subindice = case_when(
        Categoria == "ACTIVIDADES DE FORMACIÓN" ~ (Conteo * 0.15) + (Talento * 0.85),  # Peso ajustado para ACTIVIDADES DE FORMACIÓN
        TRUE ~ (Conteo * 0.5) + (Talento * 0.5)  # Peso equitativo para las demás categorías
      )
    )
  
  # Asignar los pesos a las categorías y tipologías
  subindices <- subindices %>%
    mutate(
      Indice_ponderado = case_when(
        Categoria == "NUEVO CONOCIMIENTO" ~ Subindice * 0.3,
        Categoria == "PRODUCCIÓN TÉCNICA Y TECNOLÓGICA" & Tipologia %in% c("Innovaciones en procesos y procedimientos", "Innovaciones generadas en la Gestión Empresarial", "Signos distintivos", "Prototipos") ~ Subindice * 0.275,
        Categoria == "PRODUCCIÓN TÉCNICA Y TECNOLÓGICA" & Tipologia == "Softwares" ~ Subindice * 0.1,
        Categoria == "ACTIVIDADES DE FORMACIÓN" & `Tipologia 2` == "Trabajos de grado de pregrado" ~ Subindice * 0.05,
        Categoria == "ACTIVIDADES DE FORMACIÓN" ~ Subindice * 0.125,
        Categoria == "APROPIACIÓN SOCIAL Y DIVULGACIÓN PÚBLICA DE LA CIENCIA" ~ Subindice * 0.05,
        TRUE ~ 0.1  # Default en caso de que no haya coincidencia
      )
    )
  
  # Calcular el índice final por línea, categoría, tipología, y tipología 2
  indice_final <- subindices %>%
    group_by(Linea) %>% # MODIFICAR SEGÚN SE REQUIERA
    summarise(Indice_Final = sum(Indice_ponderado, na.rm = TRUE)) %>%
    arrange(desc(Indice_Final))
  
  return(indice_final)
}

# Aplicar la función a tu base de datos
indice_produccion <- calcular_indice_produccion(web_scraping_clean)

# Mostrar el top 20 líneas, categorías, tipologías y tipos con el índice final
ranking_resultado <- indice_produccion %>%
  arrange(desc(Indice_Final)) %>%
  head(23)

# Imprimir el resultado
print(ranking_resultado, n = 23) 

# 6. MAPA DE CALOR

library(dplyr)
library(tidyr)
library(ggplot2)

# Crear la tabla con el conteo de títulos únicos por Línea, Categoría y Tipología
tabla_conteo_2 <- web_scraping_clean %>%
  filter(Linea != "Línea no identificada") %>%  # Excluir "Línea no identificada"
  group_by(Linea, Categoria, `Tipologia 2`) %>%  # Agrupar por Línea, Categoría y Tipología
  summarise(Conteo_Titulos_Unicos = n_distinct(Titulo), .groups = 'drop')  # Contar títulos únicos

# Transformar la tabla en un formato adecuado para visualización de mapa de calor
tabla_heatmap_2 <- tabla_conteo_2 %>%
  pivot_wider(
    names_from = c(Categoria, `Tipologia 2`),  # Crear columnas a partir de la combinación de Categoría y Tipología
    values_from = Conteo_Titulos_Unicos,  # Llenar las celdas con el conteo de títulos únicos
    values_fill = list(Conteo_Titulos_Unicos = 0)  # Reemplazar NA con 0 para celdas sin datos
  )

# Ver la tabla resultante
print(tabla_heatmap)

guardar_datos(tabla_heatmap_2, path_base, "tabla_heatmap_2")
guardar_datos(ranking_resultado, path_base, "ranking_resultado")
