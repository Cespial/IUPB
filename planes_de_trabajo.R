
### PLANES DE TRABAJO --------------------------------------------------------------------------------

# Definir la ruta y obtener la lista de archivos
folder_path <- '/Users/cristianespinal/Library/CloudStorage/GoogleDrive-cristian.espinal@pascualbravo.edu.co/Unidades compartidas/UVIC - Unidad de Vigilancia Tecnologica e Inteligencia Competitiva/6. Lago de datos/RAW DATA/Planes de trabajo'
files <- list.files(folder_path, pattern = "PlanesDeTrabajo_\\d{5}\\.xlsx", full.names = TRUE)

# Función para cargar los datos con la primera fila como nombres de columnas y añadir el nombre del archivo como columna
load_data_with_filename <- function(file_path) {
  # Cargar el archivo con la primera fila como nombres de columnas
  temp_data <- read_xlsx(file_path, col_names = TRUE)
  # Añadir el nombre del archivo como una nueva columna
  temp_data$source_file <- basename(file_path)
  return(temp_data)
}

# Cargar y combinar todos los archivos
planes_trabajo <- lapply(files, load_data_with_filename) %>% bind_rows()

# Extraer año y semestre del nombre del archivo
generate_year_and_semester <- function(file_name) {
  year_semester <- gsub("PlanesDeTrabajo_|\\.xlsx", "", file_name)
  year <- substr(year_semester, 1, 4)
  semester <- substr(year_semester, 5, 5)
  return(data.frame(ano = as.numeric(year), semestre = as.numeric(semester)))
}

# Añadir columnas de año y semestre
year_semester_df <- do.call(rbind, lapply(planes_trabajo$source_file, generate_year_and_semester))
planes_trabajo <- cbind(planes_trabajo, year_semester_df)

# Estandarizar los nombres de columnas y tipos de datos
standardize_df <- function(df) {
  # Define los nombres de las columnas esperados y sus tipos
  expected_cols <- c("Docente", "EstadoDePlanDeTrabajo", "Facultad", "Identificacion",
                     "PorcentajeDeApoyo", "PorcentajeDeDocenciaDirecta",
                     "PorcentajeDeExtensionYOtra",
                     "PorcentajeDeInvestigacion", "TotalTiempoApoyo",
                     "TotalTiempoDocenciaDirecta",
                     "TotalTiempoExtensionYOtra", "TotalTiempoInvestigacion", "source_file", "ano", "semestre")
  # Filtrar las columnas que existen en el DataFrame
  available_cols <- intersect(expected_cols, colnames(df))
  df <- df[, available_cols, drop = FALSE]
  
  # Añadir columnas faltantes como NA
  missing_cols <- setdiff(expected_cols, available_cols)
  for (col in missing_cols) {
    df[[col]] <- NA
  }
  
  return(df)
}

# Aplicar estandarización a todas las columnas sin perder información
planes_trabajo <- planes_trabajo %>%
  rename_with(~ make.names(.), everything()) %>% # Asegurar que los nombres de columnas sean válidos
  standardize_df()

# Convertir los porcentajes y tiempos a valores numéricos
planes_trabajo <- planes_trabajo %>%
  mutate(
    across(starts_with("Porcentaje"), ~ as.numeric(gsub(",", ".", .)) / 100),
    across(starts_with("TotalTiempo"), ~ as.numeric(gsub(",", ".", .)))
  )

# Ejecución de la función de guardado para Planes de Trabajo
path_base <- "../DATA CONSOLIDADA/"
guardar_datos(planes_trabajo, path_base, "Planes_de_Trabajo")