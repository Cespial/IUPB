library(readxl)
library(dplyr)
library(stringr)
library(purrr)

# INTENTO 1 -------
evaluados_mapping <- list(
  "Gerente y Coordinador" = c("SERGIO ANDRÉS LOPERA PÉREZ", "CARLOS ARTURO NIETO GUERRERO", "RAFAEL ELÍAS PACHECO ATENCIO", "ADA LUPE DÍAZ FONTALVO", "JUAN ALFONSO ORTEGA CHICA", "YESICA MILENA NEGRETE RODRÍGUEZ"),
  "Técnicos Operativos G2" = c("CESAR DARIO VÁSQUEZ BENITEZ", "DANOBIS JOSE YEPES RIOS", "YONHEIDER ENRIQUE ROJAS GARAVITO", "FABIÁN ANDRÉS TORCEDILLA ARROYO", "ISABELLA TIRADO ARAUJO", "ISAIAS CUITIVA HERNANDEZ", "JHONIER DAVID POLO SOLERA"),
  "Técnicos Operativos G1" = c("CESAR DARIO VÁSQUEZ BENITEZ", "JHONATAN DAVID QUIÑONES PEREIRA", "PEDRO JULIO MARTINEZ MARTINEZ", "JOSE LUIS PATERNINA DIEZ", "ESTEBAN LUIS VEGLIANTE ARRIETA", "CAMILO ERNESTO PUCHE GONZÁLEZ", "OSCAR DARIO VILLADA SÁNCHEZ"),
  "Ejecutivos equipo Comercial" = c("CARLOS ARTURO NIETO GUERRERO", "DELFIN IGNACIO VIDAL DURANGO", "DANIEL FERNANDO MARQUEZ ESPRIELLA", "LOLY LUZ CASTILLO MONTES"),
  "Lider Operativo a G2 Técnicos" = c("YOHEIDER ENRIQUE ROJAS", "DANOBIS JOSE YEPES RIOS"),
  "Gerente a Ejecutivo Contratación" = c("DANIEL FERNANDO MARQUEZ ESPRIELLA"),
  "Gerente a Ejecutivos" = c("DELFIN IGNACIO VIDAL DURANGO", "DANIEL FERNANDO MARQUEZ ESPRIELLA", "LOLY LUZ CASTILLO MONTES"),
  "Ejecutivo Contratacion equipo Ccial" = c("CARLOS ARTURO NIETO GUERRERO", "RAFAEL ELÍAS PACHECO ATENCIO", "ADA LUPE DÍAZ FONTALVO", "DANIEL FERNANDO MARQUEZ ESPRIELLA"),
  "Gerente a Operativos" = c("NINI JHOANNA FERNANDEZ BERRIO", "CESAR DARIO VASQUEZ BENITEZ", "LUIS HERNÁN GONZÁLEZ CORREA", "SANTIAGO ALBERTO NEGRETE DONADO"),
  "Líder, Supervisor, Ingeniero Opera" = c("ADA LUPE DÍAZ FONTALVO", "CESAR DARIO VÁSQUEZ BENITEZ", "GARY ALBERTO RUÍZ BUZON", "NINI JHOANNA FERNANDEZ BERRIO", "LUIS HERNÁN GONZÁLEZ CORREA", "SANTIAGO ALBERTO NEGRETE DONADO"),
  "Líder Administrativo y equipo" = c("SERGIO ANDRÉS LOPERA PÉREZ", "YULY MARCELA ROMERO MANGONEZ", "YULIS ESTHER PÉREZ ARRIETA", "CARLOS ANTONIO MARTINEZ MARTINEZ", "DIANA PATRICIA NIETO GUERRERO", "CARMEN JULIA JIMÉNEZ GUERRA")
)

adjust_and_add_evaluado <- function(df, file_name) {
  # Limpiar el nombre del archivo para extraer el grupo
  grupo <- str_extract(file_name, "Evaluación de Competencias y Habilidades Funcionales - [^\\(]+")
  grupo <- str_trim(str_replace(grupo, "Evaluación de Competencias y Habilidades Funcionales - ", ""))
  names_list <- evaluados_mapping[[grupo]]
  
  # Añadir la columna 'Evaluado'
  df$Evaluado <- rep(names_list, each=nrow(df))
  
  df
}

files <- list.files(path, pattern = "\\.xlsx$", full.names = TRUE)

list_data <- lapply(files, function(file) {
  df <- read_excel(file)
  adjust_and_add_evaluado(df, file)
})

combined_data <- bind_rows(list_data)

# Establecer la ruta de los archivos
path <- "/Users/cristianespinal/Library/CloudStorage/GoogleDrive-cristianjosue07@gmail.com/Mi unidad/Business/2024/SYNERGYTECH/DATA/xlsx_2"

# Listar todos los archivos Excel en el directorio
files <- list.files(path, pattern = "\\.xlsx$", full.names = TRUE)

# Función para extraer el nombre resumido del archivo
get_short_name <- function(file_path) {
  file_name <- tools::file_path_sans_ext(basename(file_path))
  str_remove(file_name, pattern = "Evaluación de Competencias y Habilidades Funcionales - ")
}

# Leer los archivos y añadir una columna con el nombre resumido
list_data <- lapply(files, function(file) {
  df <- read_excel(file)
  df$Source <- get_short_name(file)  # Añadir columna con el nombre resumido
  return(df)
})

normalize_column_names <- function(df) {
  names(df) <- tolower(names(df))  # Convertir a minúsculas para uniformidad
  names(df) <- gsub(" +", " ", names(df))  # Eliminar espacios extra
  return(df)
}

list_data <- lapply(list_data, normalize_column_names)

# INTENTO 2 ------

# Función para ajustar los nombres de las columnas y añadir la columna 'Evaluado'
adjust_data <- function(df, file_name) {
  grupo <- str_extract(file_name, "Evaluación de Competencias y Habilidades Funcionales - [^(]+")
  grupo <- str_replace(grupo, "Evaluación de Competencias y Habilidades Funcionales - ", "")
  grupo <- str_replace(grupo, "\\(.*", "") # Eliminar cualquier cosa después de '(' incluyendo '('
  grupo <- trimws(grupo) # Quitar espacios en blanco adicionales
  
  # Añadir la columna 'Evaluado'
  df$Evaluado <- evaluados_mapping[[grupo]][1]  # Ajustar para repetir según el número de filas si necesario
  
  # Consolidar columnas
  cols <- names(df)
  unique_cols <- unique(sub(" .+$", "", cols))
  df <- df %>%
    summarise(across(all_of(unique_cols), ~ mean(.x, na.rm = TRUE), .names = "{.col}")) 
  
  return(df)
}

# Leer los archivos, ajustar y combinar
path <- "/Users/cristianespinal/Library/CloudStorage/GoogleDrive-cristianjosue07@gmail.com/Mi unidad/Business/2024/SYNERGYTECH/DATA/xlsx_2"
files <- list.files(path, pattern = "\\.xlsx$", full.names = TRUE)
combined_data <- bind_rows(lapply(files, function(file) {
  df <- read_excel(file)
  adjust_data(df, file)
}))

# Verificar los resultados
print(head(combined_data))


# INTENTO 3 --------
library(readxl)
library(dplyr)

# Ruta al archivo
file_path <- "/Users/cristianespinal/Library/CloudStorage/GoogleDrive-cristianjosue07@gmail.com/Mi unidad/Business/2024/SYNERGYTECH/DATA/xlsx_2/Evaluación de Competencias y Habilidades Funcionales - Gerente y Coordinador.(1-6).xlsx"

# Leer los datos
data <- read_excel(file_path)

# Nombres de las personas evaluadas
evaluados <- c("SERGIO ANDRÉS LOPERA PÉREZ", "CARLOS ARTURO NIETO GUERRERO", "RAFAEL ELÍAS PACHECO ATENCIO", "ADA LUPE DÍAZ FONTALVO", "JUAN ALFONSO ORTEGA CHICA", "YESICA MILENA NEGRETE RODRÍGUEZ")

# Asegurarse de que hay suficientes nombres para todas las filas del dataframe
if (nrow(data) > length(evaluados)) {
  stop("No hay suficientes nombres de evaluados para todas las filas del dataframe.")
}

# Añadir la columna 'Evaluado'
data$Evaluado <- evaluados[1:nrow(data)]

# Mostrar las primeras filas del dataframe modificado
print(head(data))

###

# Asumiendo que ya tienes el dataframe cargado en 'data'

# Listado de nombres de evaluados según el orden proporcionado anteriormente
evaluados <- c("SERGIO ANDRÉS LOPERA PÉREZ", "CARLOS ARTURO NIETO GUERRERO", "RAFAEL ELÍAS PACHECO ATENCIO", "ADA LUPE DÍAZ FONTALVO", "JUAN ALFONSO ORTEGA CHICA", "YESICA MILENA NEGRETE RODRÍGUEZ")

# Asegurando que el dataframe tiene una columna 'Seleccione su nombre' para identificar al evaluador
if(!"Seleccione su nombre" %in% names(data)) {
  stop("La columna 'Seleccione su nombre' no está presente en el dataframe.")
}

# Creando una nueva columna 'Evaluado' basada en el orden de aparición repetida por evaluador
data$Evaluado <- rep(evaluados, each = nrow(data) / length(evaluados))

# Revisar si el orden de evaluados corresponde con el número de filas en el dataframe
if(length(data$Evaluado) != nrow(data)) {
  warning("La cantidad de nombres de 'Evaluados' no coincide con el número de filas del dataframe. Verifica las entradas.")
}

# Reorganizando las columnas para colocar 'Evaluado' después de 'Seleccione su nombre'
data <- data %>%
  dplyr::select(`ID`, `Hora de inicio`, `Hora de finalización`, `Correo electrónico`, `Nombre`, `Seleccione su nombre`, `Evaluado`, everything())


# Verificar la estructura del dataframe
str(data)

# Mostrar las primeras filas para confirmar
head(data)


# INTENTO 4 ------

library(dplyr)
library(tidyr)

# Suponiendo que 'data' es tu dataframe original
# Creando un ejemplo de columnas para el primer evaluado (sin números al final)
evaluado1 <- data %>%
  select(ID, 
         Hora_inicio = `Hora de inicio`, 
         Hora_fin = `Hora de finalización`, 
         Email = `Correo electrónico`, 
         Nombre, 
         Evaluador = `Seleccione su nombre`, 
         matches("^[^0-9]*$"))  # Selecciona columnas que no terminan en números

# Para el segundo evaluado (columnas que terminan en '2')
evaluado2 <- data %>%
  select(ID, 
         Hora_inicio = `Hora de inicio`, 
         Hora_fin = `Hora de finalización`, 
         Email = `Correo electrónico`, 
         Nombre, 
         Evaluador = `Seleccione su nombre`, 
         matches("2$"))  # Selecciona columnas que terminan en '2'

# Y así sucesivamente para cada evaluado...

# Si es necesario combinar todos en un solo dataframe y asignar manualmente el nombre del evaluado:
data_final <- bind_rows(
  evaluado1 %>% mutate(Evaluado = "SERGIO ANDRÉS LOPERA PÉREZ"),
  evaluado2 %>% mutate(Evaluado = "CARLOS ARTURO NIETO GUERRERO"),
  # Agrega más evaluados aquí...
)

# Verificar la estructura del dataframe resultante
str(data_final)
# INTENTO 5 -------

library(dplyr)
library(tidyr)
library(stringr)

# Ejemplo de cómo identificar y contar las repeticiones de la columna clave
nombre_columna_clave <- "[Calidad del trabajo] El o la colaborador (a) entrega resultados de alta calidad y mínimos errores en su trabajo."
indice_reinicio <- which(names(data) == nombre_columna_clave)

# Si no hay repetición, el bloque es único y no es necesario dividir nada.
if(length(indice_reinicio) > 1) {
  # Crear un identificador de bloque basado en las repeticiones
  bloque_id <- rep(1:length(indice_reinicio), diff(c(indice_reinicio, ncol(data) + 1)))
  
  # Asignar este bloque_id al dataframe para que cada conjunto de columnas tenga un id único
  data_long <- data %>%
    mutate(bloque_id = bloque_id) %>%
    pivot_longer(cols = -bloque_id, names_to = "variable", values_to = "valor") %>%
    mutate(variable = str_remove(variable, "\\d+$"),  # Opcional: limpiar nombres de columna eliminando números al final
           evaluado_id = ceiling(row_number() / (ncol(data)/length(indice_reinicio)))) %>%
    pivot_wider(names_from = variable, values_from = valor)
  
  # Asegúrate de que 'evaluado_id' o alguna columna similar marque cada bloque de evaluado correctamente
  print(head(data_long))
} else {
  print("No hay repeticiones de bloques de columnas.")
}



# INTENTO 6 -------

library(dplyr)
library(tidyr)
library(stringr)

# Función para identificar y transformar las columnas
prep_data <- function(data) {
  # Extraer la base del nombre de columna y el índice
  data %>%
    mutate(across(everything(), ~ .x, .names = "{str_extract(colnames(.), '.*(?=\\.)')}_{str_extract(colnames(.), '(?<=\\.)\\d+$')}")) %>%
    pivot_longer(cols = -c(ID, `Hora de inicio`, `Hora de finalización`, `Correo electrónico`, Nombre, `Seleccione su nombre`), names_to = c(".value", "set"), names_sep = "_") %>%
    arrange(ID, set)
}

# Aplicar la función al dataframe
data_long <- prep_data(data)

# Revisar la estructura del dataframe transformado
print(head(data_long))

head(data)

# INTENTO 7 -------

library(dplyr)
library(tidyr)
library(stringr)

# Crear el dataframe pivoteado
data_long <- data %>%
  mutate(ID = row_number()) %>%
  pivot_longer(
    cols = starts_with("["),
    names_to = c("variable", "set"),
    names_pattern = "\\[(.*)\\](\\d*)",
    values_to = "valor"
  ) %>%
  mutate(set = if_else(set == "", "1", set),
         variable = str_c(variable, set)) %>%
  select(-set) %>%
  pivot_wider(
    names_from = variable,
    values_from = valor
  ) %>%
  arrange(ID)

# Revisar el dataframe resultante
print(head(data_long))

# INTENTO 8 ------

# Extraer y revisar los sufijos numéricos de las columnas
pattern <- "\\d+$"  # Buscar uno o más dígitos al final del nombre de la columna
sufijos <- sapply(colnames(data), function(x) {
  match <- regmatches(x, regexec(pattern, x))
  if (length(match[[1]]) > 1) {
    return(as.numeric(match[[1]][2]))
  } else {
    return(NA)  # Devolver NA si no hay coincidencia
  }
})

# Mostrar la tabla de sufijos numéricos extraídos para verificar
table(sufijos, useNA = "ifany")

# Si los sufijos son correctos, intentar calcular el máximo nuevamente
max_evaluados <- max(sufijos, na.rm = TRUE)
print(max_evaluados)


# INTENTO 9 -------

# Cargar las librerías necesarias
library(readxl)
library(dplyr)
library(writexl)

# Leer los datos del archivo "ejemplo"
data <- read_excel("/Users/cristianespinal/Library/CloudStorage/GoogleDrive-cristianjosue07@gmail.com/Mi unidad/Business/2024/SYNERGYTECH/DATA/xlsx_2/Evaluación de Competencias y Habilidades Funcionales - Ejecutivo Contratacion equipo Ccial(1-1).xlsx")

# Definir los nombres de las columnas informativas
info_columns <- c(
  'ID', 'Hora de inicio', 'Hora de finalización', 'Correo electrónico',
  'Seleccione su nombre', 'Evaluado', 'Elige el nivel del cargo que ocupas',
  'Correo electrónico2', 'Correo electrónico3'
)

# Identificar el índice inicial para las preguntas de calificación
first_question_col_index <- which(names(data) == 'Correo electrónico2') + 1

# Obtener las columnas de las 31 preguntas de calificación
questions_cols <- names(data)[first_question_col_index:(first_question_col_index + 30)]

# Lista de evaluadores
eval_list <- c("SERGIO ANDRÉS LOPERA PÉREZ",
               "CARLOS ARTURO NIETO GUERRERO",
               "RAFAEL ELÍAS PACHECO ATENCIO",
               "ADA LUPE DÍAZ FONTALVO",
               "JUAN ALFONSO ORTEGA CHICA",
               "YESICA MILENA NEGRETE RODRÍGUEZ")

# Transformar los datos
transformed_rows_corrected <- list()

for (i in 1:length(eval_list)) {
  # Ajustar el índice de las columnas de respuestas para cada evaluado
  start_col <- first_question_col_index + (i - 1) * 31
  end_col <- start_col + 30
  
  responses <- data[, start_col:end_col]
  
  # Crear la fila correspondiente para cada evaluado incluyendo solo las preguntas de calificación
  row <- cbind(data[1, info_columns], responses)
  transformed_rows_corrected <- rbind(transformed_rows_corrected, row)
}

# Convertir a data frame
final_questions_df <- as.data.frame(transformed_rows_corrected)

# Asignar los nombres de las columnas correctos
colnames(final_questions_df) <- c(info_columns, questions_cols)

# Guardar el DataFrame resultante en un archivo Excel
write_xlsx(final_questions_df, "/mnt/data/final_questions_df.xlsx")

# INTENTO 10 ---------

# Cargar las librerías necesarias
library(readxl)
library(dplyr)
library(writexl)

# Leer los datos del archivo "ejemplo"
data <- read_excel("~/Downloads/Gerente y Coordinador.(1-6).xlsx")

# Definir los nombres de las columnas informativas
info_columns <- c(
  'ID', 'Hora de inicio', 'Hora de finalización', 'Correo electrónico',
  'Seleccione su nombre', 'Evaluado', 'Elige el nivel del cargo que ocupas',
  'Correo electrónico2'
)

# Crear la columna 'Evaluado' con los nombres de los evaluadores
eval_list <- c("SERGIO ANDRÉS LOPERA PÉREZ",
               "CARLOS ARTURO NIETO GUERRERO",
               "RAFAEL ELÍAS PACHECO ATENCIO",
               "ADA LUPE DÍAZ FONTALVO",
               "JUAN ALFONSO ORTEGA CHICA",
               "YESICA MILENA NEGRETE RODRÍGUEZ")

# Identificar el índice inicial para las preguntas de calificación
first_question_col_index <- which(names(data) == 'Correo electrónico2') + 1

# Obtener las columnas de las 31 preguntas de calificación
questions_cols <- names(data)[first_question_col_index:(first_question_col_index + 30)]

# Transformar los datos
transformed_rows_corrected <- list()

for (i in 1:length(eval_list)) {
  # Ajustar el índice de las columnas de respuestas para cada evaluado
  start_col <- first_question_col_index + (i - 1) * 31
  end_col <- start_col + 30
  
  responses <- data[, start_col:end_col]
  
  # Crear la fila correspondiente para cada evaluado incluyendo solo las preguntas de calificación
  row <- cbind(data[1, 1:4], # ID, Hora de inicio, Hora de finalización, Correo electrónico
               data[1, 5],   # Seleccione su nombre
               eval_list[i],  # Evaluado
               data[1, 6:8], # Elige el nivel del cargo que ocupas, Correo electrónico2
               responses)
  
  transformed_rows_corrected <- rbind(transformed_rows_corrected, row)
}

# Convertir a data frame
final_questions_df <- as.data.frame(transformed_rows_corrected)

# Asignar los nombres de las columnas correctos
colnames(final_questions_df) <- c('ID', 'Hora de inicio', 'Hora de finalización', 'Correo electrónico',
                                  'Seleccione su nombre', 'Evaluado', 'Elige el nivel del cargo que ocupas',
                                  'Correo electrónico2', questions_cols)

# Guardar el DataFrame resultante en un archivo Excel
write_xlsx(final_questions_df, "/mnt/data/final_questions_df.xlsx")


# INTENTO 11 ------

# Definir los nombres de los evaluadores
eval_list <- c("SERGIO ANDRÉS LOPERA PÉREZ",
               "CARLOS ARTURO NIETO GUERRERO",
               "RAFAEL ELÍAS PACHECO ATENCIO",
               "ADA LUPE DÍAZ FONTALVO",
               "JUAN ALFONSO ORTEGA CHICA",
               "YESICA MILENA NEGRETE RODRÍGUEZ")

# Número de columnas por cada evaluado
num_cols_per_eval <- 31

# Definir el índice de inicio de las preguntas (después de las columnas informativas)
start_col <- 10 # Asumiendo que las primeras 9 columnas son informativas

# Preparar el DataFrame final con las columnas deseadas
column_names <- c('Seleccione su nombre', 'Evaluado', names(data)[start_col:(start_col + num_cols_per_eval - 1)])
final_df <- data.frame(matrix(ncol = length(column_names), nrow = 0))
colnames(final_df) <- column_names

# Función para reorganizar los datos
reformat_data <- function(df, num_cols_per_eval, eval_names) {
  temp_df <- final_df # Crear una copia temporal de final_df
  for (row_index in 1:nrow(df)) {
    evaluator_name <- df[row_index, 'Seleccione su nombre']
    for (i in seq_along(eval_names)) {
      start_col_index <- start_col + (i - 1) * num_cols_per_eval
      end_col_index <- start_col_index + num_cols_per_eval - 1
      new_row <- c(evaluator_name, eval_names[i], df[row_index, start_col_index:end_col_index])
      temp_df <- rbind(temp_df, new_row) # Añadir la fila al DataFrame temporal
    }
  }
  return(temp_df) # Devolver el DataFrame completo
}

# Aplicar la reformatación a los datos
final_df <- reformat_data(data, num_cols_per_eval, eval_list)

# Convertir a data frame
final_df <- as.data.frame(final_df)

# Asignar los nombres de las columnas correctos
colnames(final_df) <- column_names

data %>%
  list(
    head = head(.),
    summary = summary(.),
    str = str(.),
    glimpse = glimpse(.),
    types = sapply(., class),
    uniques = sapply(., function(x) length(unique(x))),
    na_count = sapply(., function(x) sum(is.na(x)))
  )

data %>%
  list(
    head = head(.),
    summary = summary(.),
    str = str(.),
    glimpse = glimpse(.),
    types = sapply(., class),
  )

# INTENTO 12 --------

library(readxl)
library(dplyr)
library(stringr)

# Cargar los datos
data <- read_excel("/Users/cristianespinal/Downloads/Consolidada/Consolidada.xlsx")

library(readxl)
library(dplyr)
library(stringr)
library(writexl)

# Cargar los datos
data <- read_excel("ruta_del_archivo.xlsx")

# Extraer las categorías de las columnas de preguntas
categorias <- str_extract(names(data), "\\[(.*?)\\]")
categorias_unique <- na.omit(unique(categorias))

# Preparar el dataframe de salida conservando las columnas de identificación
data_promedios <- data %>%
  select(ID, Encuesta, `Hora de inicio`, `Hora de finalización`, Evaluador, Evaluado, 
         `Rol evaluador`, `Rol evaluado`, Área, `Elije el nivel del cargo que ocupas`, 
         `Correo electrónico2`)

# Calcular el promedio por cada categoría y agregar al dataframe
for (categoria in categorias_unique) {
  columnas_categoria <- names(data)[str_detect(names(data), sprintf("\\[%s\\]", categoria))]
  nombre_promedio <- str_replace_all(categoria, " ", "_") # Asegura nombres de variable válidos
  if (length(columnas_categoria) > 0) {
    # Calcula el promedio solo si hay columnas que coincidan con la categoría
    data_promedios[[paste0(nombre_promedio, "_Promedio")]] <- rowMeans(data[, columnas_categoria, drop = FALSE], na.rm = TRUE)
  }
}

# Diagnósticos para entender la estructura del resultado
str(data_promedios)

# Guardar el nuevo dataframe en un archivo Excel
write_xlsx(data_promedios, "Promedios_Consolidados.xlsx")


# LECTURA DE LOS DATOS -----

data_long %>%
  list(
    head = head(.),
    summary = summary(.),
    str = str(.),
    glimpse = glimpse(.),
    types = sapply(., class),
    uniques = sapply(., function(x) length(unique(x))),
    na_count = sapply(., function(x) sum(is.na(x)))
  )

data_promedios %>%
  list(
    head = head(.),
    summary = summary(.),
    str = str(.),
    types = sapply(., class),
  )
