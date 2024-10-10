
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

# BASE MEN - SNIES

# IAM
base_iam <- "/Users/cristianespinal/Downloads/SNIES/base_basica.xlsx"
base_iam <- read_excel(base_iam)
str(base_iam)

# GEO-IES
geo_ies <- '/Users/cristianespinal/Downloads/SNIES/geo_ies_final (1).xlsx'
geo_ies <- read_excel(geo_ies)
str(geo_ies)

# PROGRAMAS SNIES
programas_snies <- '/Users/cristianespinal/Downloads/SNIES/programas.xlsx'
programas_snies <- read_excel(programas_snies)
str(programas_snies)

# ADMINISTRATIVOS SNIES
administrativos <- '/Users/cristianespinal/Downloads/SNIES/administrativos.xlsx'
administrativos <- read_excel(administrativos)
str(administrativos)

# DOCENTES SNIES
docentes_snies <- '/Users/cristianespinal/Downloads/SNIES/docentes.xlsx'
docentes_snies <- read_excel(docentes_snies)
str(docentes_snies)

str(base_iam)
str(programas_snies)
str(geo_ies)
str(administrativos)
str(docentes_snies)

names(base_iam)
names(programas_snies)
names(geo_ies)
names(administrativos)
names(docentes_snies)

# 2. JOIN PROGRAMAS: IAM CON INFO

# Realizar el left join entre base_iam y programas_snies
iam_snies <- base_iam %>%
  left_join(programas_snies, by = c("codigo_snies_del_programa" = "codigo_snies_del_programa"))

iam_snies <- iam_snies %>%
  mutate(codigo_institucion.x = as.character(codigo_institucion.x)) %>%
  left_join(geo_ies, by = c("codigo_institucion.x" = "codigo_institucion"))

# Filtrar la base para Pregrado
iam_snies_no_acreditadas <- iam_snies %>%
  filter(acreditada_alta_calidad == "N")

administrativos_snies <- administrativos %>%
  mutate(codigo_institucion = as.character(codigo_institucion)) %>%
  left_join(geo_ies, by = c("codigo_institucion" = "codigo_institucion"))

docentes_snies <- docentes_snies %>%
  mutate(codigo_institucion = as.character(codigo_institucion)) %>%
  left_join(geo_ies, by = c("codigo_institucion" = "codigo_institucion"))

iam_snies_acreditadas <- iam_snies %>%
  filter(acreditada_alta_calidad == "S")

# Crear la columna 'time' agrupando año y semestre
iam_snies <- iam_snies %>%
  mutate(time = paste(ano, semestre, sep = "-"))

# Mostrar las primeras filas para verificar los resultados
head(iam_snies_pregrado)
head(iam_snies_posgrado)

# GUARDAR DATOS 

guardar_datos(docentes_snies, path_base, "docentes_snies")
guardar_datos(administrativos_snies, path_base, "administrativos_snies")

unique(iam_snies$nivel_academico)