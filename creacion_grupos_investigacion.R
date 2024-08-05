
# 1. ENCUESTA PROCESAMIENTO -----------

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
file_path <- "../DATA CONSOLIDADA/encuesta_investigacion_uvic_tabulada.xlsx"
encuesta_talento <- read_excel(file_path)
head(encuesta_talento)
names(encuesta_talento)

# Agrupa las líneas de investigación en una sola columna
encuesta_talento <- encuesta_talento %>%
  unite("Lineas_de_Investigacion", 
        c("Elija la(s) línea(s) de investigación de su grupo que respalda. Si no apoya ninguna, marque \"No aplica\". [Agroambiental]",
          "Elija la(s) línea(s) de investigación de su grupo que respalda. Si no apoya ninguna, marque \"No aplica\". [Desarrollo Sostenible]",
          "Elija la(s) línea(s) de investigación de su grupo que respalda. Si no apoya ninguna, marque \"No aplica\". [Energías Renovables y Sistemas Eléctricos]",
          "Elija la(s) línea(s) de investigación de su grupo que respalda. Si no apoya ninguna, marque \"No aplica\". [Materiales Sostenibles]",
          "Elija la(s) línea(s) de investigación de su grupo que respalda. Si no apoya ninguna, marque \"No aplica\". [Movilidad Sostenible]",
          "Elija la(s) línea(s) de investigación de su grupo que respalda. Si no apoya ninguna, marque \"No aplica\". [Sistemas Inteligentes Sostenibles]",
          "Elija la(s) línea(s) de investigación de su grupo que respalda. Si no apoya ninguna, marque \"No aplica\". [Gestión de proyectos con orientación a los ODS]",
          "Elija la(s) línea(s) de investigación de su grupo que respalda. Si no apoya ninguna, marque \"No aplica\". [Gestión del conocimiento y de la Innovación]",
          "Elija la(s) línea(s) de investigación de su grupo que respalda. Si no apoya ninguna, marque \"No aplica\". [Logística y Cadena De Suministro]",
          "Elija la(s) línea(s) de investigación de su grupo que respalda. Si no apoya ninguna, marque \"No aplica\". [Productividad y Calidad]",
          "Elija la(s) línea(s) de investigación de su grupo que respalda. Si no apoya ninguna, marque \"No aplica\". [Educación y TIC]",
          "Elija la(s) línea(s) de investigación de su grupo que respalda. Si no apoya ninguna, marque \"No aplica\". [Instrumentación Científica e Industrial]",
          "Elija la(s) línea(s) de investigación de su grupo que respalda. Si no apoya ninguna, marque \"No aplica\". [Modelado Computacional]",
          "Elija la(s) línea(s) de investigación de su grupo que respalda. Si no apoya ninguna, marque \"No aplica\". [Nanotecnología]",
          "Elija la(s) línea(s) de investigación de su grupo que respalda. Si no apoya ninguna, marque \"No aplica\". [Telecomunicaciones]",
          "Elija la(s) línea(s) de investigación de su grupo que respalda. Si no apoya ninguna, marque \"No aplica\". [Gestión Energética]",
          "Elija la(s) línea(s) de investigación de su grupo que respalda. Si no apoya ninguna, marque \"No aplica\". [Gestión del Mantenimiento]",
          "Elija la(s) línea(s) de investigación de su grupo que respalda. Si no apoya ninguna, marque \"No aplica\". [Materiales y Procesos en Ingeniería]",
          "Elija la(s) línea(s) de investigación de su grupo que respalda. Si no apoya ninguna, marque \"No aplica\". [Sistemas de Potencia]",
          "Elija la(s) línea(s) de investigación de su grupo que respalda. Si no apoya ninguna, marque \"No aplica\". [Creatividad, interacción y mediación]",
          "Elija la(s) línea(s) de investigación de su grupo que respalda. Si no apoya ninguna, marque \"No aplica\". [Diseño Sostenible]",
          "Elija la(s) línea(s) de investigación de su grupo que respalda. Si no apoya ninguna, marque \"No aplica\". [Pensamiento Social]",
          "Elija la(s) línea(s) de investigación de su grupo que respalda. Si no apoya ninguna, marque \"No aplica\". [No aplica]"), 
        sep = "; ", na.rm = TRUE)

# Agrupa las disciplinas en una sola columna
encuesta_talento <- encuesta_talento %>%
  unite("Disciplinas", 
        c("Elija la disciplina en la que se enfoca su actividad investigativa [Ciencias Naturales]",
          "Elija la disciplina en la que se enfoca su actividad investigativa [Ingeniería y Tecnología]",
          "Elija la disciplina en la que se enfoca su actividad investigativa [Ciencias Médica y De La Salud]",
          "Elija la disciplina en la que se enfoca su actividad investigativa [Ciencias Agrícolas]",
          "Elija la disciplina en la que se enfoca su actividad investigativa [Ciencias Sociales]",
          "Elija la disciplina en la que se enfoca su actividad investigativa [Humanidades]",
          "Elija la disciplina en la que se enfoca su actividad investigativa [No aplica]"), 
        sep = "; ", na.rm = TRUE)

# Separa las filas
encuesta_talento <- encuesta_talento %>%
  separate_rows(Lineas_de_Investigacion, sep = "; ")
encuesta_talento <- encuesta_talento %>%
  separate_rows(Disciplinas, sep = "; ")

# Elimina duplicados basados en Identificación, Lineas_de_Investigacion y Disciplinas
encuesta_talento <- encuesta_talento %>%
  distinct(Identificación, Lineas_de_Investigacion, Disciplinas, .keep_all = TRUE)

# Contar las líneas de investigación
lineas_conteo <- encuesta_talento %>%
  group_by(Lineas_de_Investigacion) %>%
  summarise(conteo = n()) %>%
  mutate(instrumento_lineas = (conteo - min(conteo)) / (max(conteo) - min(conteo)))

# Contar las disciplinas
disciplinas_conteo <- encuesta_talento %>%
  group_by(Disciplinas) %>%
  summarise(conteo = n()) %>%
  mutate(instrumento_disciplinas = (conteo - min(conteo)) / (max(conteo) - min(conteo)))

# Contar los enfoques investigativos y normalizar los valores
enfoque_conteo <- encuesta_talento %>%
  group_by(Enfoque_Investigativo) %>%
  summarise(conteo = n()) %>%
  mutate(instrumento_enfoque = (conteo - min(conteo)) / (max(conteo) - min(conteo)))

# Contar los núcleos básicos de conocimiento y normalizar los valores
nucleo_conteo <- encuesta_talento %>%
  group_by(Nucleo_Basico_Conocimiento) %>%
  summarise(conteo = n()) %>%
  mutate(instrumento_nucleo = (conteo - min(conteo)) / (max(conteo) - min(conteo)))

# Muestra las primeras filas de los datos de conteo
head(lineas_conteo)
head(disciplinas_conteo)
head(enfoque_conteo)
head(nucleo_conteo)

# Renombrar las variables según lo especificado
encuesta_talento <- encuesta_talento %>%
  rename(
    Cédula = `Cédula  (no incluir puntos ni comas, solo números ej: 1281052350)`,
    Departamento = `Elija el departamento al cual pertenece`,
    Programa_Academico = `Elija el programa académico institucional al cual aporta`,
    Grupo_Investigacion = `Elija el grupo de investigación al cual pertenece. De lo contrario, marque la opción no aplica.`,
    Enfoque_Investigativo = `Escriba el nombre específico de su enfoque investigativo (por ejemplo: Síntesis de materiales, modelamiento de procesos, modelado computacional, sistemas de potencia, etc)`,
    Nucleo_Basico_Conocimiento = `Elija el núcleo básico de conocimiento en el cuál se enfoca su quehacer investigativo`
  )

# 2. WEB-SCRAPPING PROCESAMIENTO -----------

file_scrapping <- "../DATA CONSOLIDADA/Minciencias_Por_Autor_Pascual.xlsx"
produccion_productividad <- read_excel(file_scrapping)
names(produccion_productividad)

# 3. AGRUPACIÓN INDEX

# Convertir los espacios en blanco a NA y luego a 0 en Identificación
encuesta_talento$Identificación[encuesta_talento$Identificación == ""] <- NA
produccion_productividad$Identificación[produccion_productividad$Identificación == ""] <- NA

# Convertir Identificación a tipo numérico
encuesta_talento$Identificación <- as.numeric(encuesta_talento$Identificación)
produccion_productividad$Identificación <- as.numeric(produccion_productividad$Identificación)

# Unir ambas bases de datos por la variable Identificación
index_talento_produccion <- left_join(encuesta_talento, produccion_productividad, by = "Identificación")

# 2.1. SUBSET POR LINEAS -----------

# Crear un subset que se llame index_lineas
index_lineas <- index_talento_produccion %>%
  select(Identificación, Lineas_de_Investigacion, Titulo) %>%
  distinct()

# Contar por línea de investigación cuántos investigadores tenemos (Talento)
index_lineas_talento <- index_lineas %>%
  group_by(Lineas_de_Investigacion) %>%
  summarise(Talento = n_distinct(Identificación))

# Contar el número de productos por línea (Producción)
index_lineas_produccion <- index_lineas %>%
  group_by(Lineas_de_Investigacion) %>%
  summarise(Producción = n_distinct(Titulo))

# Unir los conteos de Talento y Producción
index_lineas <- left_join(index_lineas_talento, index_lineas_produccion, by = "Lineas_de_Investigacion")

# Crear una variable que sea un índice de productividad (Producción / Talento)
index_lineas <- index_lineas %>%
  mutate(Indice_Productividad = Producción / Talento)

# Asegurar que el índice de productividad esté entre 0 y 1
index_lineas <- index_lineas %>%
  mutate(Indice_Productividad = (Indice_Productividad - min(Indice_Productividad)) / (max(Indice_Productividad) - min(Indice_Productividad)))

# Crear variable instrumento_talento normalizando Talento de 0 a 1
index_lineas <- index_lineas %>%
  mutate(instrumento_talento = (Talento - min(Talento)) / (max(Talento) - min(Talento)))

# Crear variable instrumento_producción normalizando Producción de 0 a 1
index_lineas <- index_lineas %>%
  mutate(instrumento_producción = (Producción - min(Producción)) / (max(Producción) - min(Producción)))

# Muestra las primeras filas del índice de líneas de investigación
head(index_lineas)

# 2.2. SUBSET PARA LOS DEMÁS ----------

# Crear funciones para el procesamiento y normalización
procesar_conteo <- function(data, grupo_var) {
  # Contar por grupo_var cuántos investigadores tenemos (Talento)
  talento_conteo <- data %>%
    group_by({{ grupo_var }}) %>%
    summarise(Talento = n_distinct(Identificación))
  
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

# Crear subsets específicos
index_talento <- index_talento_produccion %>%
  select(Identificación, Lineas_de_Investigacion, Titulo, Enfoque_Investigativo, Nucleo_Basico_Conocimiento, Disciplinas, 
         Grupo_Investigacion, Categoria, Tipologia) %>%
  distinct()

# Procesar conteo para cada grupo
index_lineas <- procesar_conteo(index_talento, Lineas_de_Investigacion)
index_enfoques <- procesar_conteo(index_talento, Enfoque_Investigativo)
index_nucleos <- procesar_conteo(index_talento, Nucleo_Basico_Conocimiento)
index_disciplinas <- procesar_conteo(index_talento, Disciplinas)
index_grupo <- procesar_conteo(index_talento, Grupo_Investigacion)
index_categoria <- procesar_conteo(index_talento, Categoria)
index_tipologia <- procesar_conteo(index_talento, Tipologia)

# Muestra las primeras filas de cada índice
head(index_lineas)
head(index_enfoques)
head(index_nucleos)
head(index_disciplinas)

# 2.3. TIPOLOGIAS POR CATEGORÍAS --------

# Obtener las categorías únicas de index_talento_produccion
categorias <- unique(index_talento_produccion$Categoria)

# Función para procesar datos por Tipología dentro de cada Categoría
procesar_tipologia <- function(data) {
  data %>%
    group_by(Tipologia) %>%
    summarise(
      Talento = n_distinct(Identificación),
      Producción = n_distinct(Titulo)
    ) %>%
    mutate(
      Indice_Productividad = Producción / Talento
    )
}

# Crear una lista para almacenar los resultados
resultados_tipologia <- list()

# Definir la ruta base para guardar los archivos
base_path <- "/Users/cristianespinal/Library/CloudStorage/GoogleDrive-cristian.espinal@pascualbravo.edu.co/Unidades compartidas/UVIC - Unidad de Vigilancia Tecnologica e Inteligencia Competitiva/6. Lago de datos/DATA CONSOLIDADA"

# Procesar datos por cada categoría y tipología
for (categoria in categorias) {
  # Filtrar por categoría
  data_categoria <- filter(index_talento_produccion, Categoria == categoria)
  
  # Procesar datos por Tipología
  resultado_tipologia <- procesar_tipologia(data_categoria)
  
  # Recalcular los valores de los instrumentos dentro de cada subset
  resultado_tipologia <- resultado_tipologia %>%
    mutate(
      instrumento_talento = (Talento - min(Talento)) / (max(Talento) - min(Talento)),
      instrumento_producción = (Producción - min(Producción)) / (max(Producción) - min(Producción)),
      Indice_Productividad = ifelse(is.nan(Indice_Productividad), 0, Indice_Productividad)
    )
  
  # Guardar el resultado en la lista
  resultados_tipologia[[categoria]] <- resultado_tipologia
  
  # Guardar el resultado en archivos CSV y XLSX
  write.csv(resultado_tipologia, 
            file = paste0(base_path, "/index_tipologia_", gsub(" ", "_", categoria), ".csv"), 
            row.names = FALSE)
  write.xlsx(resultado_tipologia, 
             file = paste0(base_path, "/index_tipologia_", gsub(" ", "_", categoria), ".xlsx"), 
             rowNames = FALSE)
}

# Guardar los resultados en un archivo .RData
save(resultados_tipologia, file = paste0(base_path, "/resultados_tipologia.RData"))


# GUARDAR -----------

guardar_datos(index_lineas, path_base, "index_lineas")
guardar_datos(index_nucleos, path_base, "index_nucleos")
guardar_datos(index_disciplinas, path_base, "index_disciplinas")
guardar_datos(index_enfoques, path_base, "index_enfoques")
guardar_datos(index_grupo, path_base, "index_grupo")
guardar_datos(index_categoria, path_base, "index_categoria")
guardar_datos(index_tipologia, path_base, "index_tipologia")
