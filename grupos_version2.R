
# CREACIÓN DE GRUPOS DE INVESTIGACIÓN

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

encuesta_tabulada %>%
  list(
    head = head(.),
    summary = summary(.),
    str = str(.),
    glimpse = glimpse(.),
    types = sapply(., class),
    uniques = sapply(., function(x) length(unique(x))),
    na_count = sapply(., function(x) sum(is.na(x)))
  )

encuesta_tabulada %>%
  list(
    head = head(.),
    types = sapply(., class),
    str = str(.)
  )

# LEER DATOS --------------

# 1. CARGAR ENCUESTA DE SICA

file_path <- "../DATA CONSOLIDADA/encuesta_investigacion_resumida.xlsx"
encuesta_sicau <- read_excel(file_path)
head(encuesta_sicau)
names(encuesta_sicau)

# 2. MANEJO DE LOS VALORES NULOS

encuesta_sicau <- encuesta_sicau %>%
  mutate(Respuesta = coalesce(RespuestaCerrada, RespuestaAbierta))

# 3. FILTRAR NULOS NO DESEADOS

# Filtrar filas donde ambas respuestas son NA
encuesta_sicau <- encuesta_sicau %>%
  filter(!is.na(RespuestaCerrada) | !is.na(RespuestaAbierta))

# Ver la cantidad de valores nulos restantes
sapply(encuesta_sicau, function(x) sum(is.na(x)))

# 3. TABULAR LOS DATOS

# Tabular por pregunta con la respuesta combinada
encuesta_tabulada <- encuesta_sicau %>%
  select(NombreDePersona, Identificacion, Colegio, Pregunta, Respuesta, DepartamentoAcademico) %>%
  pivot_wider(
    names_from = Pregunta,
    values_from = Respuesta,
    names_glue = "{Pregunta}_Respuesta"
  )

# Ver las primeras filas de la tabla resultante
print(encuesta_tabulada)

# 4. AGRUPAR COLUMNAS DE LÍNEAS DE INVESTIGACIÓN

# Definir las columnas que deseas agrupar
columnas_agrupadas <- c(
  "[Grupo GIIAM] Elija la(s) línea(s) de investigación que respalda en su grupo. Si no apoya ninguna, marque \"No aplica\"_Respuesta",
  "Elija la(s) línea(s) de investigación de su grupo que respalda. Si no apoya ninguna, marque \"No aplica\"._Respuesta",
  "[Grupo GICEI] Elija la(s) línea(s) de investigación que respalda en su grupo. Si no apoya ninguna, marque \"No aplica\"_Respuesta",
  "[Grupo GIIEN] Elija la(s) línea(s) de investigación que respalda en su grupo. Si no apoya ninguna, marque \"No aplica\"_Respuesta",
  "[Grupo ICONO] Elija la(s) línea(s) de investigación que respalda en su grupo. Si no apoya ninguna, marque \"No aplica\"_Respuesta",
  "[Grupo QUALIPO] Elija la(s) línea(s) de investigación que respalda en su grupo. Si no apoya ninguna, marque \"No aplica\"_Respuesta"
)

# Crear una nueva columna agrupando las respuestas, separando por comas
encuesta_tabulada <- encuesta_tabulada %>%
  mutate(Lineas = apply(select(., all_of(columnas_agrupadas)), 1, function(row) {
    # Filtrar los valores no nulos y unirlos por comas
    paste(na.omit(row), collapse = ", ")
  }))

# Verificar los primeros resultados
head(encuesta_tabulada %>% select(Lineas))

# 5. ELIMINAR COLUMNAS SOBRANTES

# Definir las columnas que quieres eliminar
columnas_a_eliminar <- c(
  "RespuestaAgrupada",
  "Cédula  (no incluir puntos ni comas, solo números ej: 1281052350)_Respuesta",
  "[Grupo GIIAM] Elija la(s) línea(s) de investigación que respalda en su grupo. Si no apoya ninguna, marque \"No aplica\"_Respuesta",
  "Elija la(s) línea(s) de investigación de su grupo que respalda. Si no apoya ninguna, marque \"No aplica\"._Respuesta",
  "[Grupo GICEI] Elija la(s) línea(s) de investigación que respalda en su grupo. Si no apoya ninguna, marque \"No aplica\"_Respuesta",
  "[Grupo GIIEN] Elija la(s) línea(s) de investigación que respalda en su grupo. Si no apoya ninguna, marque \"No aplica\"_Respuesta",
  "[Grupo ICONO] Elija la(s) línea(s) de investigación que respalda en su grupo. Si no apoya ninguna, marque \"No aplica\"_Respuesta",
  "[Grupo QUALIPO] Elija la(s) línea(s) de investigación que respalda en su grupo. Si no apoya ninguna, marque \"No aplica\"_Respuesta"
)

columnas_a_eliminar <- c("Cédula  (no incluir puntos ni comas, solo números ej: 1281052350)_Respuesta")

# Eliminar las columnas sobrantes
encuesta_tabulada <- encuesta_tabulada %>%
  select(-all_of(columnas_a_eliminar))

# 6. ELIMINAR DUPLICADOS DE DOCENTES Y QUEDARNOS CON LOS QUE TIENEN MÁS INFORMACIÓN

# Definir una función para contar los valores no nulos por fila
contar_datos_completos <- function(row) {
  sum(!sapply(row, is.null)) + sum(!sapply(row, function(x) length(x) == 0))
}

# Aplicar la función para contar datos completos y crear una nueva columna que cuente cuántos datos completos hay en cada fila
encuesta_tabulada <- encuesta_tabulada %>%
  rowwise() %>%
  mutate(NumDatosCompletos = contar_datos_completos(c_across(starts_with("Elija") | starts_with("Escriba") | starts_with("Cédula") | starts_with("Lineas")))) %>%
  ungroup()

# Eliminar duplicados por identificación, quedándonos con la fila que tiene más datos completos
encuesta_tabulada <- encuesta_tabulada %>%
  group_by(Identificacion) %>%
  slice_max(NumDatosCompletos, with_ties = FALSE) %>%
  ungroup()

# Eliminar la columna NumDatosCompletos ya que no es necesaria después de la selección
encuesta_tabulada <- encuesta_tabulada %>%
  select(-NumDatosCompletos)

# 7. LIMPIAR LA COLUMNA LÍNEAS DE NULL

# Limpiar la columna Lineas
encuesta_tabulada <- encuesta_tabulada %>%
  mutate(Lineas = sapply(Lineas, function(x) {
    # Si es NULL o el string "NULL", lo convertimos a NA
    if (is.null(x) || x == "NULL") {
      return(NA)
    }
    # Limpiar los saltos de línea y espacios en blanco innecesarios
    x <- gsub("\r\n", "", x)
    
    # Si es un vector o lista, concatenamos los valores
    if (is.list(x) || grepl("c\\(", x)) {
      # Eliminar los valores "NULL" dentro de la lista
      x <- gsub("NULL", "", x)
      # Eliminar los paréntesis de la lista y las comillas
      x <- gsub('c\\(|\\)', '', x)
      x <- gsub('\"', '', x)
      # Unir los valores restantes con una coma si es una lista
      return(paste(trimws(unlist(strsplit(x, ","))), collapse = ", "))
    }
    
    # De lo contrario, devolvemos el valor tal cual
    return(trimws(x))
  }))

# Verificar los primeros resultados
head(encuesta_tabulada$Lineas)

# Limpiar la columna Lineas
encuesta_tabulada <- encuesta_tabulada %>%
  mutate(Lineas = sapply(Lineas, function(x) {
    # Si es NULL o el string "NULL", lo convertimos a NA
    if (is.null(x) || x == "NULL") {
      return(NA)
    }
    # Limpiar los saltos de línea y comas innecesarias
    x <- gsub("\r\n", "", x)  # Eliminar saltos de línea
    x <- gsub("NULL", "", x)  # Eliminar el texto "NULL"
    x <- gsub(",\\s*,", ",", x)  # Eliminar comas duplicadas o seguidas de espacios
    x <- gsub("^,\\s*", "", x)  # Eliminar comas al inicio
    x <- gsub(",\\s*$", "", x)  # Eliminar comas al final
    
    # Si es un vector o lista, concatenamos los valores
    if (is.list(x) || grepl("c\\(", x)) {
      x <- gsub('c\\(|\\)', '', x)  # Eliminar paréntesis de lista
      x <- gsub('\"', '', x)  # Eliminar comillas
      x <- paste(trimws(unlist(strsplit(x, ","))), collapse = ", ")  # Unir valores
    }
    
    # Eliminar comas sueltas o seguidas de espacios nuevamente
    x <- gsub(",\\s*,", ",", x)
    x <- gsub("^,\\s*", "", x)
    x <- gsub(",\\s*$", "", x)
    
    # Devolver el valor limpio
    return(trimws(x))
  }))

# Verificar los primeros resultados
head(encuesta_tabulada$Lineas)


