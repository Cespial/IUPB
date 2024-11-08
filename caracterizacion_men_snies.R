
# CARACTERIZACIÓN MEN -------

# 1. CARGAR BASES DE DATOS

# Cargar el paquete readxl
library(readxl)

# Definir una lista con las rutas de los archivos
files <- c(
  "~/Library/CloudStorage/GoogleDrive-cristian.espinal@pascualbravo.edu.co/Unidades compartidas/UVIC - Unidad de Vigilancia Tecnologica e Inteligencia Competitiva/6. Lago de datos/RAW DATA/SNIES_caracterizacion/Matricula_sexo.xlsx",
  "~/Library/CloudStorage/GoogleDrive-cristian.espinal@pascualbravo.edu.co/Unidades compartidas/UVIC - Unidad de Vigilancia Tecnologica e Inteligencia Competitiva/6. Lago de datos/RAW DATA/SNIES_caracterizacion/matricula_programas_reprotan_matricula.xlsx",
  "~/Library/CloudStorage/GoogleDrive-cristian.espinal@pascualbravo.edu.co/Unidades compartidas/UVIC - Unidad de Vigilancia Tecnologica e Inteligencia Competitiva/6. Lago de datos/RAW DATA/SNIES_caracterizacion/Matricula_nivel_formacion.xlsx",
  "~/Library/CloudStorage/GoogleDrive-cristian.espinal@pascualbravo.edu.co/Unidades compartidas/UVIC - Unidad de Vigilancia Tecnologica e Inteligencia Competitiva/6. Lago de datos/RAW DATA/SNIES_caracterizacion/Matricula_nivel_academico.xlsx",
  "~/Library/CloudStorage/GoogleDrive-cristian.espinal@pascualbravo.edu.co/Unidades compartidas/UVIC - Unidad de Vigilancia Tecnologica e Inteligencia Competitiva/6. Lago de datos/RAW DATA/SNIES_caracterizacion/Matricula_modalidad.xlsx",
  "~/Library/CloudStorage/GoogleDrive-cristian.espinal@pascualbravo.edu.co/Unidades compartidas/UVIC - Unidad de Vigilancia Tecnologica e Inteligencia Competitiva/6. Lago de datos/RAW DATA/SNIES_caracterizacion/Matricula_cine.xlsx",
  "~/Library/CloudStorage/GoogleDrive-cristian.espinal@pascualbravo.edu.co/Unidades compartidas/UVIC - Unidad de Vigilancia Tecnologica e Inteligencia Competitiva/6. Lago de datos/RAW DATA/SNIES_caracterizacion/Matricula_area_conocimiento.xlsx",
  "~/Library/CloudStorage/GoogleDrive-cristian.espinal@pascualbravo.edu.co/Unidades compartidas/UVIC - Unidad de Vigilancia Tecnologica e Inteligencia Competitiva/6. Lago de datos/RAW DATA/SNIES_caracterizacion/matricula_AC_NF.xlsx",
  "~/Library/CloudStorage/GoogleDrive-cristian.espinal@pascualbravo.edu.co/Unidades compartidas/UVIC - Unidad de Vigilancia Tecnologica e Inteligencia Competitiva/6. Lago de datos/RAW DATA/SNIES_caracterizacion/indicadores_generales.xlsx",
  "~/Library/CloudStorage/GoogleDrive-cristian.espinal@pascualbravo.edu.co/Unidades compartidas/UVIC - Unidad de Vigilancia Tecnologica e Inteligencia Competitiva/6. Lago de datos/RAW DATA/SNIES_caracterizacion/IAM.xlsx",
  "~/Library/CloudStorage/GoogleDrive-cristian.espinal@pascualbravo.edu.co/Unidades compartidas/UVIC - Unidad de Vigilancia Tecnologica e Inteligencia Competitiva/6. Lago de datos/RAW DATA/SNIES_caracterizacion/graduados_nivel.xlsx",
  "~/Library/CloudStorage/GoogleDrive-cristian.espinal@pascualbravo.edu.co/Unidades compartidas/UVIC - Unidad de Vigilancia Tecnologica e Inteligencia Competitiva/6. Lago de datos/RAW DATA/SNIES_caracterizacion/docentes_tipo_contrato.xlsx",
  "~/Library/CloudStorage/GoogleDrive-cristian.espinal@pascualbravo.edu.co/Unidades compartidas/UVIC - Unidad de Vigilancia Tecnologica e Inteligencia Competitiva/6. Lago de datos/RAW DATA/SNIES_caracterizacion/docentes_nivel.xlsx",
  "~/Library/CloudStorage/GoogleDrive-cristian.espinal@pascualbravo.edu.co/Unidades compartidas/UVIC - Unidad de Vigilancia Tecnologica e Inteligencia Competitiva/6. Lago de datos/RAW DATA/SNIES_caracterizacion/docentes_dedicacion_sexo.xlsx",
  "~/Library/CloudStorage/GoogleDrive-cristian.espinal@pascualbravo.edu.co/Unidades compartidas/UVIC - Unidad de Vigilancia Tecnologica e Inteligencia Competitiva/6. Lago de datos/RAW DATA/SNIES_caracterizacion/desercion_NF.xlsx"
)

# Leer todos los archivos y almacenarlos en una lista de data frames
data_frames <- lapply(files, read_excel)

# Asignar nombres a cada data frame en la lista
names(data_frames) <- c(
  "Matricula_sexo",
  "matricula_programas_reprotan_matricula",
  "Matricula_nivel_formacion",
  "Matricula_nivel_academico",
  "Matricula_modalidad",
  "Matricula_cine",
  "Matricula_area_conocimiento",
  "matricula_AC_NF",
  "indicadores_generales",
  "IAM",
  "graduados_nivel",
  "docentes_tipo_contrato",
  "docentes_nivel",
  "docentes_dedicacion_sexo",
  "desercion_NF"
)

# Ahora puedes acceder a cada data frame por su nombre, por ejemplo:
head(data_frames$Matricula_sexo)

# Mostrar la estructura de cada base de datos
for (name in names(data_frames)) {
  cat("\n\nEstructura de la base de datos:", name, "\n")
  print(str(data_frames[[name]]))
}

# Si deseas guardar esta estructura para revisar después:
sink("estructura_bases_datos.txt")
for (name in names(data_frames)) {
  cat("\n\nEstructura de la base de datos:", name, "\n")
  print(str(data_frames[[name]]))
}
sink()

# Puedes visualizar el archivo "estructura_bases_datos.txt" para ver la estructura de todas las bases de datos.

# ... AGRUPACIONES

library(dplyr)
library(readxl)

# Definir las rutas de los archivos
files <- list(
  Matricula_sexo = "~/Library/CloudStorage/GoogleDrive-cristian.espinal@pascualbravo.edu.co/Unidades compartidas/UVIC - Unidad de Vigilancia Tecnologica e Inteligencia Competitiva/6. Lago de datos/RAW DATA/SNIES_caracterizacion/Matricula_sexo.xlsx",
  matricula_programas_reprotan_matricula = "~/Library/CloudStorage/GoogleDrive-cristian.espinal@pascualbravo.edu.co/Unidades compartidas/UVIC - Unidad de Vigilancia Tecnologica e Inteligencia Competitiva/6. Lago de datos/RAW DATA/SNIES_caracterizacion/matricula_programas_reprotan_matricula.xlsx",
  Matricula_nivel_formacion = "~/Library/CloudStorage/GoogleDrive-cristian.espinal@pascualbravo.edu.co/Unidades compartidas/UVIC - Unidad de Vigilancia Tecnologica e Inteligencia Competitiva/6. Lago de datos/RAW DATA/SNIES_caracterizacion/Matricula_nivel_formacion.xlsx",
  Matricula_nivel_academico = "~/Library/CloudStorage/GoogleDrive-cristian.espinal@pascualbravo.edu.co/Unidades compartidas/UVIC - Unidad de Vigilancia Tecnologica e Inteligencia Competitiva/6. Lago de datos/RAW DATA/SNIES_caracterizacion/Matricula_nivel_academico.xlsx",
  Matricula_modalidad = "~/Library/CloudStorage/GoogleDrive-cristian.espinal@pascualbravo.edu.co/Unidades compartidas/UVIC - Unidad de Vigilancia Tecnologica e Inteligencia Competitiva/6. Lago de datos/RAW DATA/SNIES_caracterizacion/Matricula_modalidad.xlsx",
  Matricula_cine = "~/Library/CloudStorage/GoogleDrive-cristian.espinal@pascualbravo.edu.co/Unidades compartidas/UVIC - Unidad de Vigilancia Tecnologica e Inteligencia Competitiva/6. Lago de datos/RAW DATA/SNIES_caracterizacion/Matricula_cine.xlsx",
  Matricula_area_conocimiento = "~/Library/CloudStorage/GoogleDrive-cristian.espinal@pascualbravo.edu.co/Unidades compartidas/UVIC - Unidad de Vigilancia Tecnologica e Inteligencia Competitiva/6. Lago de datos/RAW DATA/SNIES_caracterizacion/Matricula_area_conocimiento.xlsx",
  graduados_nivel = "~/Library/CloudStorage/GoogleDrive-cristian.espinal@pascualbravo.edu.co/Unidades compartidas/UVIC - Unidad de Vigilancia Tecnologica e Inteligencia Competitiva/6. Lago de datos/RAW DATA/SNIES_caracterizacion/graduados_nivel.xlsx",
  desercion_NF = "~/Library/CloudStorage/GoogleDrive-cristian.espinal@pascualbravo.edu.co/Unidades compartidas/UVIC - Unidad de Vigilancia Tecnologica e Inteligencia Competitiva/6. Lago de datos/RAW DATA/SNIES_caracterizacion/desercion_NF.xlsx",
  docentes_tipo_contrato = "~/Library/CloudStorage/GoogleDrive-cristian.espinal@pascualbravo.edu.co/Unidades compartidas/UVIC - Unidad de Vigilancia Tecnologica e Inteligencia Competitiva/6. Lago de datos/RAW DATA/SNIES_caracterizacion/docentes_tipo_contrato.xlsx",
  docentes_nivel = "~/Library/CloudStorage/GoogleDrive-cristian.espinal@pascualbravo.edu.co/Unidades compartidas/UVIC - Unidad de Vigilancia Tecnologica e Inteligencia Competitiva/6. Lago de datos/RAW DATA/SNIES_caracterizacion/docentes_nivel.xlsx",
  docentes_dedicacion_sexo = "~/Library/CloudStorage/GoogleDrive-cristian.espinal@pascualbravo.edu.co/Unidades compartidas/UVIC - Unidad de Vigilancia Tecnologica e Inteligencia Competitiva/6. Lago de datos/RAW DATA/SNIES_caracterizacion/docentes_dedicacion_sexo.xlsx"
)

# Leer todas las bases de datos
data_frames <- lapply(files, read_excel)

# Unificar bases de datos de matrícula
matriculas <- bind_rows(
  data_frames$Matricula_sexo %>% mutate(tipo = "sexo"),
  data_frames$Matricula_nivel_formacion %>% mutate(tipo = "nivel_formacion"),
  data_frames$Matricula_nivel_academico %>% mutate(tipo = "nivel_academico"),
  data_frames$Matricula_modalidad %>% mutate(tipo = "modalidad"),
  data_frames$Matricula_cine %>% mutate(tipo = "cine"),
  data_frames$Matricula_area_conocimiento %>% mutate(tipo = "area_conocimiento")
)

# Unificar bases de datos de graduados y deserción
graduados_desercion <- bind_rows(
  data_frames$graduados_nivel %>% mutate(tipo = "graduados"),
  data_frames$desercion_NF %>% mutate(tipo = "desercion")
)

# Unificar bases de datos de docentes
docentes <- bind_rows(
  data_frames$docentes_tipo_contrato %>% mutate(tipo = "tipo_contrato"),
  data_frames$docentes_nivel %>% mutate(tipo = "nivel"),
  data_frames$docentes_dedicacion_sexo %>% mutate(tipo = "dedicacion_sexo")
)

# Guardar las bases unificadas para PowerBI
write.csv(matriculas, "matriculas_unificadas.csv", row.names = FALSE)
write.csv(graduados_desercion, "graduados_desercion_unificada.csv", row.names = FALSE)
write.csv(docentes, "docentes_unificados.csv", row.names = FALSE)

# .. MEJORAR 

library(dplyr)
library(readxl)

# Leer y estandarizar todas las bases de datos
unificar_datos <- function(file_path, tipo) {
  df <- read_excel(file_path) %>%
    mutate(tipo = tipo) %>%                      # Añadir columna tipo para identificar la fuente
    rename_all(tolower)                          # Pasar todos los nombres de columnas a minúsculas
  
  # Verificar si la columna "año" o "year" existe y estandarizarla
  if ("año" %in% colnames(df)) {
    df <- df %>% rename(year = año)
  } else if ("year" %in% colnames(df)) {
    df <- df %>% rename(year = year)
  }
  
  # Asegurar tipos de datos consistentes si "year" existe
  if ("year" %in% colnames(df)) {
    df <- df %>%
      mutate(year = as.numeric(gsub("[^0-9]", "", year))) %>%  # Limpiar y convertir "year" a numérico
      mutate(year = ifelse(is.na(year), NA, year))             # Reemplazar valores no convertibles con NA
  }
  
  # Convertir "codigo_ies_padre" a texto para consistencia
  if ("codigo_ies_padre" %in% colnames(df)) {
    df <- df %>%
      mutate(codigo_ies_padre = as.character(codigo_ies_padre))
  }
  
  return(df)
}

# Definir las rutas de los archivos
files <- list(
  Matricula_sexo = "~/Library/CloudStorage/GoogleDrive-cristian.espinal@pascualbravo.edu.co/Unidades compartidas/UVIC - Unidad de Vigilancia Tecnologica e Inteligencia Competitiva/6. Lago de datos/RAW DATA/SNIES_caracterizacion/Matricula_sexo.xlsx",
  matricula_programas_reprotan_matricula = "~/Library/CloudStorage/GoogleDrive-cristian.espinal@pascualbravo.edu.co/Unidades compartidas/UVIC - Unidad de Vigilancia Tecnologica e Inteligencia Competitiva/6. Lago de datos/RAW DATA/SNIES_caracterizacion/matricula_programas_reprotan_matricula.xlsx",
  Matricula_nivel_formacion = "~/Library/CloudStorage/GoogleDrive-cristian.espinal@pascualbravo.edu.co/Unidades compartidas/UVIC - Unidad de Vigilancia Tecnologica e Inteligencia Competitiva/6. Lago de datos/RAW DATA/SNIES_caracterizacion/Matricula_nivel_formacion.xlsx",
  Matricula_nivel_academico = "~/Library/CloudStorage/GoogleDrive-cristian.espinal@pascualbravo.edu.co/Unidades compartidas/UVIC - Unidad de Vigilancia Tecnologica e Inteligencia Competitiva/6. Lago de datos/RAW DATA/SNIES_caracterizacion/Matricula_nivel_academico.xlsx",
  Matricula_modalidad = "~/Library/CloudStorage/GoogleDrive-cristian.espinal@pascualbravo.edu.co/Unidades compartidas/UVIC - Unidad de Vigilancia Tecnologica e Inteligencia Competitiva/6. Lago de datos/RAW DATA/SNIES_caracterizacion/Matricula_modalidad.xlsx",
  Matricula_cine = "~/Library/CloudStorage/GoogleDrive-cristian.espinal@pascualbravo.edu.co/Unidades compartidas/UVIC - Unidad de Vigilancia Tecnologica e Inteligencia Competitiva/6. Lago de datos/RAW DATA/SNIES_caracterizacion/Matricula_cine.xlsx",
  Matricula_area_conocimiento = "~/Library/CloudStorage/GoogleDrive-cristian.espinal@pascualbravo.edu.co/Unidades compartidas/UVIC - Unidad de Vigilancia Tecnologica e Inteligencia Competitiva/6. Lago de datos/RAW DATA/SNIES_caracterizacion/Matricula_area_conocimiento.xlsx",
  graduados_nivel = "~/Library/CloudStorage/GoogleDrive-cristian.espinal@pascualbravo.edu.co/Unidades compartidas/UVIC - Unidad de Vigilancia Tecnologica e Inteligencia Competitiva/6. Lago de datos/RAW DATA/SNIES_caracterizacion/graduados_nivel.xlsx",
  desercion_NF = "~/Library/CloudStorage/GoogleDrive-cristian.espinal@pascualbravo.edu.co/Unidades compartidas/UVIC - Unidad de Vigilancia Tecnologica e Inteligencia Competitiva/6. Lago de datos/RAW DATA/SNIES_caracterizacion/desercion_NF.xlsx",
  docentes_tipo_contrato = "~/Library/CloudStorage/GoogleDrive-cristian.espinal@pascualbravo.edu.co/Unidades compartidas/UVIC - Unidad de Vigilancia Tecnologica e Inteligencia Competitiva/6. Lago de datos/RAW DATA/SNIES_caracterizacion/docentes_tipo_contrato.xlsx",
  docentes_nivel = "~/Library/CloudStorage/GoogleDrive-cristian.espinal@pascualbravo.edu.co/Unidades compartidas/UVIC - Unidad de Vigilancia Tecnologica e Inteligencia Competitiva/6. Lago de datos/RAW DATA/SNIES_caracterizacion/docentes_nivel.xlsx",
  docentes_dedicacion_sexo = "~/Library/CloudStorage/GoogleDrive-cristian.espinal@pascualbravo.edu.co/Unidades compartidas/UVIC - Unidad de Vigilancia Tecnologica e Inteligencia Competitiva/6. Lago de datos/RAW DATA/SNIES_caracterizacion/docentes_dedicacion_sexo.xlsx"
)

# Leer y estandarizar las bases de datos
data_frames <- lapply(names(files), function(name) {
  file_path <- files[[name]]
  unificar_datos(file_path, name)
})

# Unir las bases de datos de matrícula
matriculas <- bind_rows(
  data_frames[[1]], data_frames[[3]], data_frames[[4]], data_frames[[5]], data_frames[[6]], data_frames[[7]]
)

# Unificar bases de datos de graduados y deserción
graduados_desercion <- bind_rows(
  data_frames[[8]], data_frames[[9]]
)

# Unificar bases de datos de docentes
docentes <- bind_rows(
  data_frames[[10]], data_frames[[11]], data_frames[[12]]
)

# Ajustar los nombres de columna para consistencia
matriculas <- matriculas %>% rename(codigo_institucion = codigo_ies_padre)
graduados_desercion <- graduados_desercion %>% rename(codigo_institucion = codigo_ies_padre)
docentes <- docentes %>% rename(codigo_institucion = codigo_ies_padre)

# CARGAR GEO-IES

file_geo_ies <- "~/Library/CloudStorage/GoogleDrive-cristian.espinal@pascualbravo.edu.co/Unidades compartidas/UVIC - Unidad de Vigilancia Tecnologica e Inteligencia Competitiva/6. Lago de datos/RAW DATA/Proyecto-MEN/geo_ies_final.xlsx"
geo_ies <- read_excel(file_geo_ies) %>%
  rename_all(tolower) %>%               # Pasar todos los nombres de columnas a minúsculas
  mutate(across(where(is.character), as.character)) # Convertir todas las columnas de texto a tipo character
str(geo_ies)
names(geo_ies)
names(matriculas)
names(graduados_desercion)
names(docentes)

# NOMBRES DE IES

file_geo_ies <- "~/Library/CloudStorage/GoogleDrive-cristian.espinal@pascualbravo.edu.co/Unidades compartidas/UVIC - Unidad de Vigilancia Tecnologica e Inteligencia Competitiva/6. Lago de datos/RAW DATA/Proyecto-MEN/geo_ies_final.xlsx"
geo_ies <- read_excel(file_geo_ies) %>%
  rename_all(tolower) %>%                      # Convertir todos los nombres de columna a minúsculas
  select(codigo_ies_padre, nombre_institucion) # Seleccionar solo las columnas necesarias

# Unir el nombre de la institución a la base de datos de matrículas
matriculas <- matriculas %>%
  left_join(geo_ies, by = c("codigo_institucion" = "codigo_ies_padre"))

# Unir el nombre de la institución a la base de datos de graduados y deserción
graduados_desercion <- graduados_desercion %>%
  left_join(geo_ies, by = c("codigo_institucion" = "codigo_ies_padre"))

# Unir el nombre de la institución a la base de datos de docentes
docentes <- docentes %>%
  left_join(geo_ies, by = c("codigo_institucion" = "codigo_ies_padre"))

# Guardar las bases de datos para visualización en PowerBI
guardar_datos(matriculas, path_base, "matriculas_perfiles_snies")
guardar_datos(graduados_desercion, path_base, "graduados_desercion_perfiles_snies")
guardar_datos(docentes, path_base, "docentes_perfiles_snies")

# ... 

# PROCESAMIENTO DE LAS BASES

Lkp_cruces <- "~/Library/CloudStorage/GoogleDrive-cristian.espinal@pascualbravo.edu.co/Unidades compartidas/UVIC - Unidad de Vigilancia Tecnologica e Inteligencia Competitiva/4. Proyectos Transversales/4.5 Proyecto MEN/Tablero_SNIES/consildadas_ubicacion/Lkp_cruces.xlsx"
Lkp_cruces <- read_excel(Lkp_cruces)

cobertura <- "~/Library/CloudStorage/GoogleDrive-cristian.espinal@pascualbravo.edu.co/Unidades compartidas/UVIC - Unidad de Vigilancia Tecnologica e Inteligencia Competitiva/4. Proyectos Transversales/4.5 Proyecto MEN/Tablero_SNIES/consildadas_ubicacion/cobertura.xlsx"
cobertura <- read_excel(cobertura)

transito_plus <- "~/Library/CloudStorage/GoogleDrive-cristian.espinal@pascualbravo.edu.co/Unidades compartidas/UVIC - Unidad de Vigilancia Tecnologica e Inteligencia Competitiva/4. Proyectos Transversales/4.5 Proyecto MEN/Tablero_SNIES/consildadas_ubicacion/transito_plus.xlsx"
transito_plus <- read_excel(transito_plus)

geo_ies_final <- "~/Library/CloudStorage/GoogleDrive-cristian.espinal@pascualbravo.edu.co/Unidades compartidas/UVIC - Unidad de Vigilancia Tecnologica e Inteligencia Competitiva/6. Lago de datos/RAW DATA/Proyecto-MEN/geo_ies_final.xlsx"
geo_ies_final <- read_excel(geo_ies_final)

# GEO IES CON LKP

# Asegurarnos de que 'mpio_cdpmp' en 'geo_ies' no tenga espacios en blanco
geo_ies$mpio_cdpmp <- trimws(geo_ies$mpio_cdpmp)

# Convertir 'CÓDIGO MUNICIPIO' en 'Lkp_cruces' a carácter y formatear con ceros a la izquierda
Lkp_cruces$mpio_cdpmp <- sprintf("%05d", Lkp_cruces$`CÓDIGO MUNICIPIO`)

# Realizar la unión
geo_lkp <- geo_ies %>%
  left_join(Lkp_cruces, by = "mpio_cdpmp")

# COBERTURA CON LKP

lkp_subset <- Lkp_cruces %>%
  select(files, `CÓDIGO DEPARTAMENTO`, `DEPARTAMENTO`, `CÓDIGO MUNICIPIO`, `MUNICIPIO`)

# Eliminar espacios en blanco al inicio y al final, y convertir a minúsculas para asegurar consistencia
lkp_subset$files <- trimws(tolower(lkp_subset$files))
cobertura$files <- trimws(tolower(cobertura$files))

# Encontrar los valores en 'cobertura' que no tienen coincidencia en 'lkp_subset'
no_match <- setdiff(cobertura$files, lkp_subset$files)
no_match

lkp_subset$files <- as.character(lkp_subset$files)
cobertura$files <- as.character(cobertura$files)

# Realizar la unión después de la limpieza
cobertura_lkp <- cobertura %>%
  left_join(lkp_subset, by = "files")

# Verificar el resultado
head(cobertura_lkp)

# TRANSITO CON LKP

lkp_subset <- Lkp_cruces %>%
  select(files, `CÓDIGO DEPARTAMENTO`, `DEPARTAMENTO`, `CÓDIGO MUNICIPIO`, `MUNICIPIO`)

# Eliminar espacios en blanco al inicio y al final, y convertir a minúsculas para asegurar consistencia
lkp_subset$files <- trimws(tolower(lkp_subset$files))
transito_plus$files <- trimws(tolower(transito_plus$files))

# Encontrar los valores en 'cobertura' que no tienen coincidencia en 'lkp_subset'
no_match <- setdiff(transito_plus$files, lkp_subset$files)
no_match

lkp_subset$files <- as.character(lkp_subset$files)
transito_plus$files <- as.character(transito_plus$files)

# Realizar la unión después de la limpieza
transito_lkp <- transito_plus %>%
  left_join(lkp_subset, by = "files")

# Verificar el resultado
head(cobertura_lkp)

# GUARDAR

# Guardar las bases de datos para visualización en PowerBI
guardar_datos(cobertura_lkp, path_base, "cobertura_lkp_perfiles_snies")
guardar_datos(transito_lkp, path_base, "transito_lkp_perfiles_snies")
guardar_datos(graduados_perfiles_snies, path_base, "graduados_perfiles_snies")
guardar_datos(geo_lkp, path_base, "geo_lkp")

# GRADUADOS

graduados_salarios <- "~/Library/CloudStorage/GoogleDrive-cristian.espinal@pascualbravo.edu.co/Unidades compartidas/UVIC - Unidad de Vigilancia Tecnologica e Inteligencia Competitiva/4. Proyectos Transversales/4.5 Proyecto MEN/Tablero_SNIES/bases consolidadas/graduados_salarios.xlsx"
graduados_salarios <- read_excel(graduados_salarios)

graduados_vinculacion <- "~/Library/CloudStorage/GoogleDrive-cristian.espinal@pascualbravo.edu.co/Unidades compartidas/UVIC - Unidad de Vigilancia Tecnologica e Inteligencia Competitiva/4. Proyectos Transversales/4.5 Proyecto MEN/Tablero_SNIES/bases consolidadas/graduados_vinculacion.xlsx"
graduados_vinculacion <- read_excel(graduados_vinculacion)

str(graduados_salarios)
str(graduados_vinculacion)

# Seleccionar las columnas específicas de 'graduados_salarios' y 'graduados_vinculacion'
# Unir ambas bases de datos usando las columnas en común
graduados_perfiles_snies <- full_join(graduados_salarios, graduados_vinculacion, by = c("files", "nombre_institucion", "codigo_ies_padre", "Nivel de Formación", "name", "year"))

# Verificar el resultado inicial
head(graduados_perfiles_snies)

# Eliminar filas con valores NA en ambas columnas 'value'
graduados_perfiles_snies <- graduados_perfiles_snies %>%
  filter(!is.na(value.x) | !is.na(value.y))

# Verificar el resultado nuevamente
glimpse(graduados_perfiles_snies)

# Exportar el data frame combinado a un archivo CSV para visualizarlo en Power BI
write.csv(graduados_perfiles_snies, "graduados_combined.csv", row.names = FALSE)

# Confirmar que el archivo fue creado
cat("El archivo 'graduados_combined.csv' se ha creado con éxito y está listo para ser importado a Power BI.")



