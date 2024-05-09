
## Agrupación de Planes de Trabajo

# Instala el paquete readxl si no lo tienes instalado
if (!require(readxl)) install.packages("readxl", dependencies = TRUE)
if (!require(dplyr)) install.packages("dplyr", dependencies = TRUE)

# Carga las bibliotecas necesarias
library(readxl)
library(dplyr)

PD_20241 <- read_xlsx("/Users/cristianespinal/Downloads/Planes de trabajo/Copia de ListadoDeReporteDePlanesDeTrabajo_20241.xlsx")
PD_20232 <- read_xlsx("/Users/cristianespinal/Downloads/Planes de trabajo/Copia de ListadoDeReporteDePlanesDeTrabajo_20232.xlsx")
PD_20231 <- read_xlsx("/Users/cristianespinal/Downloads/Planes de trabajo/Copia de ListadoDeReporteDePlanesDeTrabajo_20231.xlsx")
PD_20222 <- read_xlsx("/Users/cristianespinal/Downloads/Planes de trabajo/Copia de ListadoDeReporteDePlanesDeTrabajo_20222.xlsx")
PD_20221 <- read_xlsx("/Users/cristianespinal/Downloads/Planes de trabajo/Copia de ListadoDeReporteDePlanesDeTrabajo_20221.xlsx")

# Organizar 2da. filas

# Función para cargar los datos con la segunda fila como nombres de columnas y eliminando la primera fila
load_data_with_correct_headers <- function(file_path) {
  # Carga el archivo empezando desde la primera fila, pero sin establecer nombres de columnas
  temp_data <- read_xlsx(file_path, col_names = FALSE)
  # Establece los nombres de las columnas basándose en la segunda fila
  colnames(temp_data) <- as.character(temp_data[2, ])
  # Elimina las dos primeras filas para empezar los datos desde la tercera fila
  temp_data <- temp_data[-(1:2), ]
  return(temp_data)
}

## Cargar

# Rutas a los archivos
file_20241 <- "/Users/cristianespinal/Downloads/Planes de trabajo/Copia de ListadoDeReporteDePlanesDeTrabajo_20241.xlsx"
file_20232 <- "/Users/cristianespinal/Downloads/Planes de trabajo/Copia de ListadoDeReporteDePlanesDeTrabajo_20232.xlsx"
file_20231 <- "/Users/cristianespinal/Downloads/Planes de trabajo/Copia de ListadoDeReporteDePlanesDeTrabajo_20231.xlsx"
file_20222 <- "/Users/cristianespinal/Downloads/Planes de trabajo/Copia de ListadoDeReporteDePlanesDeTrabajo_20222.xlsx"
file_20221 <- "/Users/cristianespinal/Downloads/Planes de trabajo/Copia de ListadoDeReporteDePlanesDeTrabajo_20221.xlsx"

# Carga de los datos con los nombres de las columnas correctos
PD_20241 <- load_data_with_correct_headers(file_20241)
PD_20232 <- load_data_with_correct_headers(file_20232)
PD_20231 <- load_data_with_correct_headers(file_20231)
PD_20222 <- load_data_with_correct_headers(file_20222)
PD_20221 <- load_data_with_correct_headers(file_20221)

# 2. Limpiar nombres

# Función para limpiar los nombres de las columnas
clean_column_names <- function(df) {
  # Elimina cualquier prefijo innecesario en los nombres de columnas
  colnames(df) <- gsub("/ListadoDeReporteDePlanDeTrabajoParaExcelViewModel/", "", colnames(df))
  
  # Elimina caracteres especiales adicionales, como los agregados por la función `read_xlsx`
  colnames(df) <- gsub("#agg", "", colnames(df))
  colnames(df) <- gsub("[^[:alnum:] ]", "", colnames(df))
  
  return(df)
}

# Aplica la función a cada DataFrame cargado
PD_20241 <- clean_column_names(PD_20241)
PD_20232 <- clean_column_names(PD_20232)
PD_20231 <- clean_column_names(PD_20231)
PD_20222 <- clean_column_names(PD_20222)
PD_20221 <- clean_column_names(PD_20221)

# Estandarizar nombres <- agrupar 

# Verificar los nombres y tipos de las columnas de cada DataFrame
sapply(data_list, colnames)
sapply(data_list, function(df) sapply(df, class))

# Función para estandarizar los nombres de columnas y tipos de datos
standardize_df <- function(df) {
  # Define los nombres de las columnas esperados y sus tipos
  # (ajusta esta parte según tus necesidades específicas)
  expected_cols <- c("Docente", "EstadoDePlanDeTrabajo", "Facultad", "Identificacion",
                    "PorcentajeDeApoyo", "PorcentajeDeDocenciaDirecta",
                    "PorcentajeDeExtensionYOtra",
                    "PorcentajeDeInvestigacion", "TotalTiempoApoyo",
                    "TotalTiempoDocenciaDirecta",
                    "TotalTiempoExtensionYOtra", "TotalTiempoInvestigacion")
  # Reordenar las columnas según los nombres esperados y convertir tipos si es necesario
  df <- df[, expected_cols, drop = FALSE]
  # Aquí podrías añadir conversiones de tipo si fuera necesario
  return(df)
}

# Aplicar la estandarización a cada DataFrame
data_list <- lapply(data_list, standardize_df)

# Combinar los DataFrames limpios
planes_de_trabajo <- bind_rows(data_list, .id = "file_name") %>%
  mutate(fecha = extract_fecha(file_name)) %>%
  select(-file_name)  # Elimina la columna file_name si no es necesaria

# Función para reemplazar el punto decimal por una coma
replace_decimal_separator <- function(df) {
  # Recorre cada columna del DataFrame
  for (col_name in colnames(df)) {
    # Si la columna es numérica, conviértela primero a carácter
    if (is.numeric(df[[col_name]])) {
      df[[col_name]] <- as.character(df[[col_name]])
    }
    # Reemplaza el punto por una coma si es una columna de tipo carácter
    if (is.character(df[[col_name]])) {
      df[[col_name]] <- gsub("\\.", ",", df[[col_name]])
    }
  }
  return(df)
}

# Aplica la función a `planes_de_trabajo`
planes_de_trabajo <- replace_decimal_separator(planes_de_trabajo)

# Verifica el resultado
head(planes_de_trabajo)

# Guardar

# Instalar el paquete openxlsx si no está instalado
if (!require(openxlsx)) install.packages("openxlsx")

# Cargar la librería openxlsx
library(openxlsx)

# Definir la función guardar_datos con el uso correcto del paquete openxlsx para escribir archivos .xlsx
guardar_datos <- function(data, path_base, nombre_base) {
  # Asegurar que la ruta base termina con '/'
  if (!grepl("/$", path_base)) {
    path_base <- paste0(path_base, "/")
  }
  
  # Guardar los datos en diferentes formatos con codificación UTF-8
  save(data, file = paste0(path_base, nombre_base, ".rdata"))
  write.xlsx(data, file = paste0(path_base, nombre_base, ".xlsx"))
  write.csv2(data, file = paste0(path_base, nombre_base, ".csv"), row.names = FALSE, fileEncoding = "UTF-8")
  write.table(data, file = paste0(path_base, nombre_base, ".txt"), sep = "\t", row.names = FALSE, fileEncoding = "UTF-8")
}

# Ruta base donde se guardarán los archivos
path_base <- "/Users/cristianespinal/Library/CloudStorage/GoogleDrive-cristian.espinal@pascualbravo.edu.co/Unidades compartidas/UVIC - Unidad de Vigilancia Tecnologica e Inteligencia Competitiva/6. Lago de datos/DATA CONSOLIDADA/"

# Nombre base para los archivos
nombre_base <- "Planes_de_Trabajo"

# Aplicar la función guardar_datos al dataframe planes_de_trabajo
guardar_datos(planes_de_trabajo, path_base, nombre_base)

