
# CREACIÓN DE GRUPOS DE INVESTIGACIÓN

# 2. PROCESAMIENTO DEL WEB SCRAPPING

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
    head = head(.),
    types = sapply(., class),
    str = str(.)
  )

names(web_scrapping_investigacion)

# LEER DATOS --------------

# 1. LECTURA DEL WEB-SCRAPPING

file_scrapping <- "../DATA CONSOLIDADA/Minciencias_Por_Autor_Pascual.xlsx"
web_scrapping_investigacion <- read_excel(file_scrapping)

# 2. ELIMINAR COLUMNAS NO NECESARIAS

# Eliminar las columnas especificadas
web_scrapping_investigacion <- web_scrapping_investigacion %>%
  select(
    -`Otra informacion`, 
    -`Codigo del grupo`, 
    -`NME_CATEGORIA_PD`, 
    -`ID_CONVOCATORIA`, 
    -`ID_PRODUCTO_PD`, 
    -`ID_PERSONA_PD`, 
    -`FCREACION_PD`, 
    -`NME_GRUPO_GR`, 
    -`Productos asociados`,
    - `ID_TIPO_PD_MED`
  )

web_scrapping_investigacion <- web_scrapping_investigacion %>%
  select(- `ID_TIPO_PD_MED`)

# Verificar los primeros resultados
head(web_scrapping_investigacion)

