
# CREACIÓN DE GRUPOS DE INVESTIGACIÓN

# 3. INNER-JOINT

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
    types = sapply(., class),
    str = str(.)
  )

creacion_de_grupos %>%
  list(
    names(.),
    str = str(.)
  )

names(web_scrapping_investigacion)
names(encuesta_tabulada)

# LEER DATOS --------------

# 1. INNER-JOINT

creacion_de_grupos <- inner_join(encuesta_tabulada, web_scrapping_investigacion, 
                                 by = c("Identificacion" = "Identificación"))

# Verificar los primeros resultados de la nueva base de datos
head(creacion_de_grupos)

# 2. ELIMINAR DUPLICADOS EXACTOS

# Eliminar duplicados exactos en todas las columnas
creacion_de_grupos <- creacion_de_grupos %>%
  distinct()

# 3. INDICE DE TALENTO Y PRODUCCIÓN

## IMPORTANTE!!!!! AJUSTAR ESTA FUNCIÓN PARA QUE EN EL CONTEO EN EL CASO DE GRUPOS DE INVESTIGACIÓN U OTROS NO SE CUENTEN VARIAS VECES LOS PRODUCTOS (TITULOS) SINO UNA SOLA VEZ. EL CONTEO DEL TALENTO SÍ PUEDE SER VARIAS VECES

# Crear función para el procesamiento y normalización
procesar_conteo <- function(data, grupo_var) {
  # Contar por grupo_var cuántos investigadores tenemos (Talento)
  talento_conteo <- data %>%
    group_by({{ grupo_var }}) %>%
    summarise(Talento = n_distinct(Identificacion))
  
  # Contar el número de productos por grupo_var (Producción)
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
indices_lineas <- procesar_conteo(creacion_de_grupos, Lineas)
indices_grupos <- procesar_conteo(creacion_de_grupos, `Nombre del grupo`)
indices_categoria <- procesar_conteo(creacion_de_grupos, `Categoria`)
indices_tipologia <- procesar_conteo(creacion_de_grupos, `Tipologia`)
indices_programa <- procesar_conteo(creacion_de_grupos, `Elija el programa académico institucional al cual aporta_Respuesta`)
indices_departamento <- procesar_conteo(creacion_de_grupos, `Elija el departamento al cual pertenece_Respuesta`)

# 4. INDICE DE TALENTO Y PRODUCCIÓN ESPECIFICAMENTE PARA LAS LÍNEAS SEPARADAS

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

# Crear función para procesar el conteo y cálculo de índices
procesar_conteo <- function(data, grupo_var) {
  # Contar el número de investigadores (Talento)
  talento_conteo <- data %>%
    group_by({{ grupo_var }}) %>%
    summarise(Talento = n_distinct(Identificacion))
  
  # Contar el número de productos (Producción)
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
creacion_de_grupos_filtrados <- creacion_de_grupos %>%
  filter(Lineas %in% lineas_validas)

# Aplicar la función procesar_conteo a las líneas de investigación válidas
indices_lineas <- procesar_conteo(creacion_de_grupos_filtrados, Lineas)

# Mostrar los índices calculados
print(indices_lineas)

# GUARDAR DATOS 

guardar_datos(indices_lineas, path_base, "indices_lineas")
guardar_datos(indices_grupos, path_base, "indices_grupos")
guardar_datos(indices_categoria, path_base, "indices_categoria")
guardar_datos(indices_tipologia, path_base, "indices_tipologia")
guardar_datos(indices_programa, path_base, "indices_programa")
guardar_datos(indices_departamento, path_base, "indices_departamento")

# 4. INDICE DE PRODUCCIÓN CIENTIFICA ------------------------------------

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

# Función para separar líneas de investigación en filas individuales
expandir_lineas <- function(data) {
  data %>%
    separate_rows(Lineas, sep = ",") %>%  # Separa las líneas de investigación por comas
    mutate(Lineas = str_trim(Lineas)) %>%  # Elimina espacios en blanco adicionales
    filter(Lineas %in% lineas_validas)     # Mantiene solo las líneas de investigación válidas
}

# Función actualizada para calcular el índice de producción científica con los nuevos pesos y subíndices ajustados
calcular_indice_produccion <- function(data) {
  
  # Separar las líneas de investigación en filas individuales
  data <- expandir_lineas(data)
  
  # Calcular el número de productos (Conteo) y el talento por línea de investigación
  data <- data %>%
    group_by(Lineas, Categoria, Tipologia, `Tipologia 2`) %>%
    summarise(
      Conteo = n_distinct(Titulo),  # Contar el número de productos distintos
      Talento = n_distinct(Identificacion),  # Contar el número de investigadores únicos
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
  
  # Asignar los pesos a las categorías, tipologías y tipología 2, incluyendo la nueva categoría ACTIVIDADES COMO EVALUADOR
  subindices <- subindices %>%
    mutate(
      Indice_ponderado = case_when(
        Categoria == "NUEVO CONOCIMIENTO" ~ Subindice * 0.3,
        Categoria == "PRODUCCIÓN TÉCNICA Y TECNOLÓGICA" & Tipologia %in% c("Innovaciones en procesos y procedimientos", "Innovaciones generadas en la Gestión Empresarial", "Signos distintivos", "Prototipos") ~ Subindice * 0.125,
        Categoria == "PRODUCCIÓN TÉCNICA Y TECNOLÓGICA" & Tipologia == "Softwares" ~ Subindice * 0.1,
        Categoria == "ACTIVIDADES DE FORMACIÓN" & `Tipologia 2` == "Trabajos de grado de pregrado" ~ Subindice * 0.05,
        Categoria == "ACTIVIDADES DE FORMACIÓN" ~ Subindice * 0.125,
        Categoria == "APROPIACIÓN SOCIAL Y DIVULGACIÓN PÚBLICA DE LA CIENCIA" ~ Subindice * 0.05,
        Categoria == "ACTIVIDADES COMO EVALUADOR" ~ Subindice * 0.05,  # Ponderación agregada para ACTIVIDADES COMO EVALUADOR
        TRUE ~ 0.1  # Default en caso de que no haya coincidencia
      )
    )
  
  # Calcular el índice final por línea de investigación
  indice_final <- subindices %>%
    group_by(Lineas) %>%
    summarise(Indice_Final = sum(Indice_ponderado, na.rm = TRUE)) %>%
    arrange(desc(Indice_Final))
  
  return(indice_final)
}

# Aplicar la función actualizada a tu base de datos
indice_produccion <- calcular_indice_produccion(creacion_de_grupos)

# Mostrar las top 20 líneas de investigación
ranking_lineas <- indice_produccion %>%
  arrange(desc(Indice_Final))

print(ranking_lineas, n = 20)

# 5. TABLA PARA EL MAPA DE CALOR ------------------------------------

# Definir las líneas de investigación válidas (únicas)
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

# Función para separar líneas de investigación en filas individuales
expandir_lineas <- function(data) {
  data %>%
    separate_rows(Lineas, sep = ",") %>%  # Separa las líneas de investigación por comas
    mutate(Lineas = str_trim(Lineas)) %>%  # Elimina espacios en blanco adicionales
    filter(Lineas %in% lineas_validas)     # Mantiene solo las líneas de investigación válidas
}

# Función para calcular tanto el índice de productividad como el índice de producción
calcular_indices <- function(data) {
  
  # Separar las líneas de investigación en filas individuales
  data <- expandir_lineas(data)
  
  # Calcular el número de productos (Conteo) y el talento por línea de investigación
  data <- data %>%
    group_by(Lineas, Categoria, Tipologia, `Tipologia 2`) %>%
    summarise(
      Conteo = n(),  # Calcular el número de productos distintos
      Talento = n_distinct(Identificacion),  # Calcular el número de talentos únicos
      Indice_Productividad = Conteo / Talento  # Calcular el índice de productividad
    ) %>%
    ungroup() %>%
    complete(Categoria, Lineas, Tipologia, `Tipologia 2`, fill = list(Conteo = 0, Talento = 0, Indice_Productividad = 0)) %>%
    arrange(Lineas, Categoria, Tipologia)
  
  # Calcular subíndices ajustados según la categoría
  data <- data %>%
    mutate(
      Subindice = case_when(
        Categoria == "ACTIVIDADES DE FORMACIÓN" ~ (Conteo * 0.15) + (Talento * 0.85),  # Peso ajustado para ACTIVIDADES DE FORMACIÓN
        TRUE ~ (Conteo * 0.5) + (Talento * 0.5)  # Peso equitativo para las demás categorías
      )
    )
  
  # Asignar los pesos a las categorías
  pesos <- c(
    "NUEVO CONOCIMIENTO" = 0.3,
    "PRODUCCIÓN TÉCNICA Y TECNOLÓGICA_Innovaciones en procesos y procedimientos" = 0.125,
    "PRODUCCIÓN TÉCNICA Y TECNOLÓGICA_Innovaciones generadas en la Gestión Empresarial" = 0.125,
    "PRODUCCIÓN TÉCNICA Y TECNOLÓGICA_Signos distintivos" = 0.125,
    "PRODUCCIÓN TÉCNICA Y TECNOLÓGICA_Prototipos" = 0.125,
    "PRODUCCIÓN TÉCNICA Y TECNOLÓGICA_Softwares" = 0.1,
    "ACTIVIDADES DE FORMACIÓN_Trabajos de grado de pregrado" = 0.05,
    "ACTIVIDADES DE FORMACIÓN" = 0.125,
    "APROPIACIÓN SOCIAL Y DIVULGACIÓN PÚBLICA DE LA CIENCIA" = 0.05,
    "ACTIVIDADES COMO EVALUADOR" = 0.05
  )
  
  # Aplicar los pesos y calcular el índice de producción ponderado
  data <- data %>%
    mutate(
      Indice_Produccion = case_when(
        Categoria == "PRODUCCIÓN TÉCNICA Y TECNOLÓGICA" & Tipologia == "Innovaciones en procesos y procedimientos" ~ Subindice * pesos["PRODUCCIÓN TÉCNICA Y TECNOLÓGICA_Innovaciones en procesos y procedimientos"],
        Categoria == "PRODUCCIÓN TÉCNICA Y TECNOLÓGICA" & Tipologia == "Innovaciones generadas en la Gestión Empresarial" ~ Subindice * pesos["PRODUCCIÓN TÉCNICA Y TECNOLÓGICA_Innovaciones generadas en la Gestión Empresarial"],
        Categoria == "PRODUCCIÓN TÉCNICA Y TECNOLÓGICA" & Tipologia == "Signos distintivos" ~ Subindice * pesos["PRODUCCIÓN TÉCNICA Y TECNOLÓGICA_Signos distintivos"],
        Categoria == "PRODUCCIÓN TÉCNICA Y TECNOLÓGICA" & Tipologia == "Prototipos" ~ Subindice * pesos["PRODUCCIÓN TÉCNICA Y TECNOLÓGICA_Prototipos"],
        Categoria == "PRODUCCIÓN TÉCNICA Y TECNOLÓGICA" & Tipologia == "Softwares" ~ Subindice * pesos["PRODUCCIÓN TÉCNICA Y TECNOLÓGICA_Softwares"],
        Categoria == "ACTIVIDADES DE FORMACIÓN" & `Tipologia 2` == "Trabajos de grado de pregrado" ~ Subindice * pesos["ACTIVIDADES DE FORMACIÓN_Trabajos de grado de pregrado"],
        Categoria == "ACTIVIDADES DE FORMACIÓN" ~ Subindice * pesos["ACTIVIDADES DE FORMACIÓN"],
        Categoria == "APROPIACIÓN SOCIAL Y DIVULGACIÓN PÚBLICA DE LA CIENCIA" ~ Subindice * pesos["APROPIACIÓN SOCIAL Y DIVULGACIÓN PÚBLICA DE LA CIENCIA"],
        Categoria == "ACTIVIDADES COMO EVALUADOR" ~ Subindice * pesos["ACTIVIDADES COMO EVALUADOR"],
        TRUE ~ Subindice * pesos["NUEVO CONOCIMIENTO"]
      )
    )
  
  # Eliminar la columna 'Tipologia 2' después de realizar los cálculos
  data <- data %>%
    select(-`Tipologia 2`)
  
  return(data)
}

# Aplicar la función a tu base de datos
indices_calculados <- calcular_indices(creacion_de_grupos)

# Mostrar las top 20 líneas de investigación con los índices calculados
ranking_lineas <- indices_calculados %>%
  arrange(desc(Indice_Produccion))

print(ranking_lineas, n = 20)

# Aplicar la función a tu base de datos
tabla_con_indices <- calcular_indices(creacion_de_grupos)

# Mostrar la tabla resultante sin la columna 'Tipologia 2'
print(tabla_con_indices)

# INTENTO DE MAPA DE CALOR ---------

# Cargar las librerías necesarias
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

# Función para separar líneas de investigación en filas individuales
expandir_lineas <- function(data) {
  data %>%
    separate_rows(Lineas, sep = ",") %>%  # Separa las líneas de investigación por comas
    mutate(Lineas = str_trim(Lineas)) %>%  # Elimina espacios en blanco adicionales
    filter(Lineas %in% lineas_validas)     # Mantiene solo las líneas de investigación válidas
}

# Función para calcular los conteos y reorganizar en formato ancho (pivot_wider)
calcular_conteos <- function(data) {
  
  # Separar las líneas de investigación en filas individuales
  data <- expandir_lineas(data)
  
  # Calcular el número de productos (Conteo) por línea de investigación y tipología
  data <- data %>%
    group_by(Lineas, Categoria, Tipologia) %>%
    summarise(
      Conteo = n(),  # Calcular el número de productos distintos
      .groups = 'drop'
    ) %>%
    ungroup()
  
  # Crear una nueva columna con la combinación de 'Categoria' y 'Tipologia' con el sufijo "_Conteo"
  data <- data %>%
    mutate(Tipologia_Conteo = paste(Categoria, Tipologia, sep = "_Conteo")) %>%
    select(Lineas, Tipologia_Conteo, Conteo)
  
  # Usar pivot_wider para transformar las tipologías en columnas
  data_ancha <- data %>%
    pivot_wider(names_from = Tipologia_Conteo, values_from = Conteo, values_fill = list(Conteo = 0))
  
  return(data_ancha)
}

# Aplicar la función a tu base de datos
tabla_conteos <- calcular_conteos(creacion_de_grupos)

# Mostrar la tabla resultante
print(tabla_conteos)

# GUARDAR DATOS 

guardar_datos(tabla_con_indices, path_base, "tabla_con_indices")
guardar_datos(ranking_lineas, path_base, "ranking_lineas")

# 6. CONTEO DE PRODUCCIÓN POR GRUPOS Y POR CATEGORIAS

# Tabla con la cantidad de productos por categoría
tabla_productos_por_categoria <- creacion_de_grupos %>%
  group_by(Categoria) %>%
  summarise(Conteo_Productos = n_distinct(Titulo)) %>%  # Calcular el número de productos únicos por categoría
  arrange(desc(Conteo_Productos))  # Ordenar de mayor a menor

# Mostrar la tabla con la cantidad de productos por categoría
print(tabla_productos_por_categoria)

# Tabla con la cantidad de productos por grupos de investigación
tabla_productos_por_grupo <- creacion_de_grupos %>%
  group_by(`Nombre del grupo`) %>%
  summarise(Conteo_Productos = n_distinct(Titulo)) %>%  # Calcular el número de productos únicos por grupo
  arrange(desc(Conteo_Productos))  # Ordenar de mayor a menor

# Mostrar la tabla con la cantidad de productos por grupos de investigación
print(tabla_productos_por_grupo)

# 7. ESTADOS DE CONSOLIDACIÓN POR LINEA

# GUARDAR DATOS 

guardar_datos(tabla_productos_por_grupo, path_base, "tabla_productos_por_grupo")
guardar_datos(tabla_productos_por_categoria, path_base, "tabla_productos_por_categoria")
guardar_datos(tabla_con_indices, path_base, "tabla_con_indices")
guardar_datos(encuesta_tabulada, path_base, "encuesta_tabulada")



