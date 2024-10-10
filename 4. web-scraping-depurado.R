
# CREACIÓN DE GRUPOS DE INVESTIGACIÓN

# CALCULAR LOS TITULOS ÚNICOS SIN LINEA

# 4. WEB.SCRAPPING

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

web_scrapping_investigacion %>%
  list(
    head = head(.),
    summary = summary(.),
    str = str(.),
    glimpse = glimpse(.),
    types = sapply(., class),
    uniques = sapply(., function(x) length(unique(x))),
    na_count = sapply(., function(x) sum(is.na(x)))
  )

web_scrapping_investigacion %>%
  list(
    names(.),
    str = str(.)
  )

creacion_de_grupos %>%
  list(
    names(.),
    str = str(.)
  )

id_docentes %>%
  list(
    names(.),
    str = str(.)
  )

names(web_scrapping_investigacion)
names(encuesta_tabulada)

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

# 2. NÚMERO ÚNICO DE TALENTO Y PRODUCTOS

# Contar el número único de títulos
num_titulos_unicos <- length(unique(web_scraping_raw$Titulo))

# Contar el número único de autores
num_autores_unicos <- length(unique(web_scraping_raw$Identificación))

# Imprimir los resultados
cat("Número único de títulos:", num_titulos_unicos, "\n")
cat("Número único de autores:", num_autores_unicos, "\n")

# 3. INDICE DE TALENTO Y PRODUCCIÓN

# Crear función para el procesamiento y normalización considerando productos e identificaciones
procesar_conteo <- function(data, grupo_var) {
  # Contar por grupo_var cuántas identificaciones únicas (Talento)
  talento_conteo <- data %>%
    group_by({{ grupo_var }}) %>%
    summarise(Talento = n_distinct(Identificación))
  
  # Contar el número único de productos por grupo_var (Producción)
  produccion_conteo <- data %>%
    group_by({{ grupo_var }}) %>%
    summarise(Producción = n_distinct(Titulo))
  
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

# Aplicaciones del indice
indices_grupos <- procesar_conteo(web_scraping_raw, `Nombre del grupo`)
indices_categoria <- procesar_conteo(web_scraping_raw, `Categoria`)
indices_tipologia <- procesar_conteo(web_scraping_raw, `Tipologia`)
indices_tipologia_2 <- procesar_conteo(web_scraping_raw, `Tipologia 2`)
indices_nme_pd_categoria <- procesar_conteo(web_scraping_raw, `NME_CATEGORIA_PD`)
indices_linea <- procesar_conteo(web_scraping_raw, `Linea`)

# GUARDAR DATOS 

guardar_datos(indices_grupos, path_base, "indices_grupos_scraping_raw")
guardar_datos(indices_grupos, path_base, "indices_grupos_scraping_raw")
guardar_datos(indices_categoria, path_base, "indices_categoria_scraping_raw")
guardar_datos(indices_tipologia, path_base, "indices_tipologia_scraping_raw")
guardar_datos(indices_tipologia_2, path_base, "indices_tipologia_2_scraping_raw")
guardar_datos(indices_tipologia_2, path_base, "indices_tipologia_2_scraping_raw")
guardar_datos(indices_nme_pd_categoria, path_base, "indices_nme_pd_categoria_scraping_raw")
guardar_datos(indices_lineas, path_base, "indices_lineas_scraping_raw")

# 3. INDICE DE TALENTO Y PRODUCCIÓN (CON LINEAS)

# Cargar las librerías necesarias
library(dplyr)

# Definir las líneas de investigación válidas
lineas_validas <- c(
  "Ciencias Naturales", "Ingeniería y Tecnología", "Ciencias Médica y De La Salud", 
  "Ciencias Agrícolas", "Ciencias Sociales", "Humanidades", "Agroambiental", 
  "Desarrollo Sostenible", "Energías Renovables y Sistemas Eléctricos", 
  "Materiales Sostenibles", "Movilidad Sostenible", "Sistemas Inteligentes Sostenibles", 
  "Gestión Energética", "Gestión del Mantenimiento", "Materiales y Procesos en Ingeniería", 
  "Sistemas de Potencia", "Educación y TIC", "Instrumentación Científica e Industrial", 
  "Modelado Computacional", "Nanotecnología", "Telecomunicaciones", 
  "Gestión de proyectos con orientación a los ODS", "Gestión del conocimiento y de la innovación", 
  "Logística y cadena de suministro", "Productividad y calidad"
)

# Crear la función para procesar el conteo y calcular índices considerando líneas válidas
procesar_conteo <- function(data, grupo_var) {
  # Filtrar solo las líneas de investigación válidas
  data <- data %>%
    filter(Linea %in% lineas_validas)
  
  # Contar el número de investigadores (Talento) por grupo_var
  talento_conteo <- data %>%
    group_by({{ grupo_var }}) %>%
    summarise(Talento = n_distinct(Identificación))
  
  # Contar el número de productos (Producción) por grupo_var
  produccion_conteo <- data %>%
    group_by({{ grupo_var }}) %>%
    summarise(Producción = n_distinct(Titulo))
  
  # Unir los conteos de Talento y Producción
  conteo <- left_join(talento_conteo, produccion_conteo, by = as_label(enquo(grupo_var)))
  
  # Calcular el índice de productividad
  conteo <- conteo %>%
    mutate(Indice_Productividad = Producción / Talento)
  
  # Normalizar el índice de productividad entre 0 y 1
  conteo <- conteo %>%
    mutate(Indice_Productividad = (Indice_Productividad - min(Indice_Productividad, na.rm = TRUE)) / 
             (max(Indice_Productividad, na.rm = TRUE) - min(Indice_Productividad, na.rm = TRUE)))
  
  # Normalizar Talento e Instrumento Producción
  conteo <- conteo %>%
    mutate(instrumento_talento = (Talento - min(Talento, na.rm = TRUE)) / 
             (max(Talento, na.rm = TRUE) - min(Talento, na.rm = TRUE)),
           instrumento_producción = (Producción - min(Producción, na.rm = TRUE)) / 
             (max(Producción, na.rm = TRUE) - min(Producción, na.rm = TRUE)))
  
  return(conteo)
}

# Filtrar los datos solo para las líneas de investigación válidas
web_scraping_filtrado <- web_scraping_raw %>%
  filter(Linea %in% lineas_validas)

# Aplicar la función procesar_conteo a las líneas de investigación válidas
indices_lineas <- procesar_conteo(web_scraping_filtrado, Linea)

# Mostrar los índices calculados
print(indices_lineas)

# 3. INDICE DE PRODUCCIÓN CIENTÍFICA

names(web_scraping_raw)

# Crear función ajustada para calcular el índice de producción científica sin la columna 'Lineas'
calcular_indice_produccion <- function(data) {
  
  # Calcular el número de productos (Conteo) y el talento por grupo de variables
  data <- data %>%
    group_by(Categoria, Tipologia, `Tipologia 2`) %>%
    summarise(
      Conteo = n_distinct(Titulo),  # Contar el número de productos distintos
      Talento = n_distinct(Identificación),  # Contar el número de investigadores únicos
      .groups = 'drop'  # Evitar agrupaciones automáticas
    )
  
  # Calcular subíndices ajustados según la categoría y tipo, con pesos específicos para ACTIVIDADES DE FORMACIÓN y otras
  subindices <- data %>%
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
        Categoria == "PRODUCCIÓN TÉCNICA Y TECNOLÓGICA" & Tipologia %in% c("Innovaciones en procesos y procedimientos", "Innovaciones generadas en la Gestión Empresarial", "Signos distintivos", "Prototipos") ~ Subindice * 0.125,
        Categoria == "PRODUCCIÓN TÉCNICA Y TECNOLÓGICA" & Tipologia == "Softwares" ~ Subindice * 0.1,
        Categoria == "ACTIVIDADES DE FORMACIÓN" & `Tipologia 2` == "Trabajos de grado de pregrado" ~ Subindice * 0.05,
        Categoria == "ACTIVIDADES DE FORMACIÓN" ~ Subindice * 0.125,
        Categoria == "APROPIACIÓN SOCIAL Y DIVULGACIÓN PÚBLICA DE LA CIENCIA" ~ Subindice * 0.05,
        TRUE ~ 0.1  # Default en caso de que no haya coincidencia
      )
    )
  
  # Calcular el índice final por categoría, tipología, y tipología 2
  indice_final <- subindices %>%
    group_by(Linea, Categoria, Tipologia) %>%
    summarise(Indice_Final = sum(Indice_ponderado, na.rm = TRUE)) %>%
    arrange(desc(Indice_Final))
  
  return(indice_final)
}

# Aplicar la función a tu base de datos
indice_produccion <- calcular_indice_produccion(web_scraping_raw)

# Mostrar el top 20 categorías, tipologías y tipos con el índice final
ranking_resultado <- indice_produccion %>%
  arrange(desc(Indice_Final)) %>%
  head(20)

# Imprimir el resultado
print(ranking_resultado)

# 4. INDICE DE PRODUCCIÓN CIENTIFICA (POR LINEAS)

# Cargar librerías necesarias
library(dplyr)
library(tidyr)

# Definir las líneas de investigación válidas
lineas_validas <- c(
  "Ciencias Naturales", "Ingeniería y Tecnología", "Ciencias Médica y De La Salud", 
  "Ciencias Agrícolas", "Ciencias Sociales", "Humanidades", "Agroambiental", 
  "Desarrollo Sostenible", "Energías Renovables y Sistemas Eléctricos", 
  "Materiales Sostenibles", "Movilidad Sostenible", "Sistemas Inteligentes Sostenibles", 
  "Gestión Energética", "Gestión del Mantenimiento", "Materiales y Procesos en Ingeniería", 
  "Sistemas de Potencia", "Educación y TIC", "Instrumentación Científica e Industrial", 
  "Modelado Computacional", "Nanotecnología", "Telecomunicaciones", 
  "Gestión de proyectos con orientación a los ODS", "Gestión del conocimiento y de la innovación", 
  "Logística y cadena de suministro", "Productividad y calidad"
)

# Función para calcular el índice de producción considerando las líneas válidas
calcular_indice_produccion <- function(data) {
  
  # Separar las líneas agrupadas por coma en filas individuales
  data_separada <- data %>%
    separate_rows(Linea, sep = ",") %>%
    mutate(Linea = trimws(Linea))  # Eliminar espacios adicionales
  
  # Filtrar solo las líneas válidas
  data_filtrada <- data_separada %>%
    filter(Linea %in% lineas_validas)
  
  # Calcular el número de productos (Conteo) y el talento por grupo de variables
  data_conteo <- data_filtrada %>%
    group_by(Linea, Categoria, Tipologia, `Tipologia 2`) %>%
    summarise(
      Conteo = n_distinct(Titulo),  # Contar el número de productos distintos
      Talento = n_distinct(Identificación),  # Contar el número de investigadores únicos
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
        Categoria == "PRODUCCIÓN TÉCNICA Y TECNOLÓGICA" & Tipologia %in% c("Innovaciones en procesos y procedimientos", "Innovaciones generadas en la Gestión Empresarial", "Signos distintivos", "Prototipos") ~ Subindice * 0.125,
        Categoria == "PRODUCCIÓN TÉCNICA Y TECNOLÓGICA" & Tipologia == "Softwares" ~ Subindice * 0.1,
        Categoria == "ACTIVIDADES DE FORMACIÓN" & `Tipologia 2` == "Trabajos de grado de pregrado" ~ Subindice * 0.05,
        Categoria == "ACTIVIDADES DE FORMACIÓN" ~ Subindice * 0.125,
        Categoria == "APROPIACIÓN SOCIAL Y DIVULGACIÓN PÚBLICA DE LA CIENCIA" ~ Subindice * 0.05,
        TRUE ~ 0.1  # Default en caso de que no haya coincidencia
      )
    )
  
  # Calcular el índice final por línea, categoría, tipología, y tipología 2
  indice_final <- subindices %>%
    group_by(Linea, `Categoria`, `Tipologia`, `Tipologia 2`) %>%
    summarise(Indice_Final = sum(Indice_ponderado, na.rm = TRUE)) %>%
    arrange(desc(Indice_Final))
  
  return(indice_final)
}

# Aplicar la función a tu base de datos
indice_produccion <- calcular_indice_produccion(web_scraping_raw)

# Mostrar el top 20 líneas, categorías, tipologías y tipos con el índice final
ranking_resultado <- indice_produccion %>%
  arrange(desc(Indice_Final)) %>%
  head(20)

# Tabla con la cantidad de productos por categoría

tabla_productos_por_linea <- web_scraping_raw %>%
  group_by(Linea) %>%
  summarise(Num_Productos = n()) %>%  # Contar el número de productos por línea
  arrange(desc(Num_Productos))  # Ordenar de mayor a menor

# Imprimir el resultado
print(ranking_resultado)

# GUARDAR DATOS 

guardar_datos(ranking_resultado, path_base, "ranking_resultado_desagregado_scraping_raw")
