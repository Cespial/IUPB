library(tidyverse)
library(readxl)
library(janitor)
library(dplyr)
library(purrr)
library(tidyr)
library(openxlsx)
library(stringi)

## 1. LECTURA DE LA FUNCIÓN --------------------------------------------------------------------------------

lectura_base <- function(path_a){ 
  print(path_a)
  temp<-
    read_excel(path = path_a) %>% 
    clean_names()
  return(temp)
}

## 2. ESTUDIANTES POR CLASES  --------------------------------------------------------------------------------

base_estu_clases <-
  data.frame(file=list.files(path = "../RAW DATA/Listado de clases/", recursive = T), 
             ruta=list.files(path = "../RAW DATA/Listado de clases/", recursive = T, full.names = T)) %>% 
  mutate(data=map(.x=ruta, ~ lectura_base(.x))) %>% 
  unnest()

prueba <-
  base_estu_clases %>% 
  mutate(id=ifelse(is.na(nombre_de_estudiante),1,0)) %>% 
  group_by(file, id) %>% 
  tally()

save(base_estu_clases, file = "../DATA CONSOLIDADA/matricula_listado_de_clases.rdata")
openxlsx::write.xlsx(base_estu_clases, file ="../DATA CONSOLIDADA/matricula_listado_de_clases.xlsx" )

## 3. FUNCIÓN PARA AGRUPAR BASES DE DATOS  --------------------------------------------------------------------------------

lectura_base <- function(path_a) { 
  print(path_a)
  tryCatch({
    temp <- read_excel(path = path_a, col_types = "text") %>% 
      clean_names()
    return(temp)
  }, error = function(e) {
    cat("Error leyendo el archivo:", path_a, "\nError:", e$message, "\n")
    return(NULL)
  })
}

## 4. FUNCIÓN PARA GUARDAR BASES DE DATOS  --------------------------------------------------------------------------------

# Función para guardar datos en varios formatos
guardar_datos <- function(data, path_base, nombre_base) {
  # Asegurar que la ruta base termina con '/'
  if (!grepl("/$", path_base)) {
    path_base <- paste0(path_base, "/")
  }
  
  # Guardar los datos en diferentes formatos
  save(data, file = paste0(path_base, nombre_base, ".rdata"))
  write.xlsx(data, file = paste0(path_base, nombre_base, ".xlsx"))
  write.csv(data, file = paste0(path_base, nombre_base, ".csv"), row.names = FALSE, fileEncoding = "UTF-8")
  write.csv(data, file = paste0(path_base, nombre_base, ".csv"), row.names = FALSE, fileEncoding = "UTF-8")
  }

# Ruta base donde se guardarán los archivos
path_base <- "../DATA CONSOLIDADA/" 

## 5. FUNCIÓN PARA CORREGIR LAS FECHAS  --------------------------------------------------------------------------------

calcular_fecha <- function(periodo) {
  year <- as.numeric(substr(periodo, 1, 4))
  semester <- as.numeric(substr(periodo, 5, 6))
  
  # Usar case_when para manejar condiciones vectorizadas
  case_when(
    semester == 1 ~ as.Date(paste(year, "01", "01", sep = "-")),  # 1 de enero del año indicado
    semester == 2 ~ as.Date(paste(year, "08", "01", sep = "-")),  # 1 de agosto del año indicado
    TRUE ~ as.Date(NA)  # En caso de que no sea un valor válido
  )
}

## 5. AGRUPACIONES DE LAS BD  --------------------------------------------------------------------------------

### 5.1. ASPIRANTES --------------------------------------------------------------------------------

# Cargar y procesar los datos
aspirantes <- data.frame(
  file = list.files(path = "../RAW DATA/Aspirantes", recursive = TRUE), 
  ruta = list.files(path = "../RAW DATA/Aspirantes", recursive = TRUE, full.names = TRUE)
) %>%
  mutate(
    data = map(.x = ruta, ~ lectura_base(.x)),  # Cargar datos desde cada archivo
    data_length = map_int(data, nrow)            # Contar filas de cada dataframe
  ) %>%
  unnest(cols = c(data)) %>%
  mutate(
    fecha_de_inscripcion = calcular_fecha(periodo),
  )

# Convertir las cadenas de texto a UTF-8 con manejo de bytes desconocidos y validación
aspirantes[] <- lapply(aspirantes, function(x) {
  if (is.character(x)) {
    stri_enc_toutf8(x, is_unknown_8bit = TRUE, validate = TRUE)
  } else {
    x
  }
})

# Ejecución de la función de guardado para Aspirantes
guardar_datos(aspirantes, path_base, "Aspirantes")

### 5.2. ADMITIDOS --------------------------------------------------------------------------------

admitidos <- data.frame(
  file = list.files(path = "../RAW DATA/Admitidos", recursive = TRUE), 
  ruta = list.files(path = "../RAW DATA/Admitidos", recursive = TRUE, full.names = TRUE)
) %>%
  mutate(
    data = map(.x = ruta, ~ lectura_base(.x)),  # Cargar datos desde cada archivo
    data_length = map_int(data, nrow)            # Contar filas de cada dataframe
  ) %>%
  unnest(cols = c(data)) %>%
  mutate(
    fecha_de_inscripcion = calcular_fecha(periodo),
    fecha_de_admision = calcular_fecha(periodo)
  )

# Convertir las cadenas de texto a UTF-8 con manejo de bytes desconocidos y validación
admitidos[] <- lapply(admitidos, function(x) {
  if (is.character(x)) {
    stri_enc_toutf8(x, is_unknown_8bit = TRUE, validate = TRUE)
  } else {
    x
  }
})

guardar_datos(admitidos, path_base, "admitidos")

### 5.3. MATRICULADOS NUEVOS --------------------------------------------------------------------------------

matricula_nuevos <- data.frame(
  file = list.files(path = "../RAW DATA/Matriculados_nuevos", recursive = TRUE), 
  ruta = list.files(path = "../RAW DATA/Matriculados_nuevos", recursive = TRUE, full.names = TRUE)
) %>%
  mutate(
    data = map(.x = ruta, ~ lectura_base(.x)),  # Cargar datos desde cada archivo
    data_length = map_int(data, nrow)            # Contar filas de cada dataframe opcional
  ) %>%
  unnest(cols = c(data)) %>%
  mutate(
    fecha_de_matricula = calcular_fecha(periodo),
  )

# Convertir las cadenas de texto a UTF-8 con manejo de bytes desconocidos y validación
matricula_nuevos[] <- lapply(matricula_nuevos, function(x) {
  if (is.character(x)) {
    stri_enc_toutf8(x, is_unknown_8bit = TRUE, validate = TRUE)
  } else {
    x
  }
})

guardar_datos(matricula_nuevos, path_base, "matricula_nuevos")

### 5.4. MATRICULADOS TOTALES --------------------------------------------------------------------------------

matricula_totales <- data.frame(
  file = list.files(path = "../RAW DATA/Matriculados_totales", recursive = TRUE), 
  ruta = list.files(path = "../RAW DATA/Matriculados_totales", recursive = TRUE, full.names = TRUE)
) %>%
  mutate(
    data = map(.x = ruta, ~ lectura_base(.x)),  # Cargar datos desde cada archivo
    data_length = map_int(data, nrow)            # Contar filas de cada dataframe opcional
  ) %>%
  unnest(cols = c(data)) %>%
  mutate(
    fecha_de_matricula = calcular_fecha(periodo),
  )

# Convertir las cadenas de texto a UTF-8 con manejo de bytes desconocidos y validación
matricula_totales[] <- lapply(matricula_totales, function(x) {
  if (is.character(x)) {
    stri_enc_toutf8(x, is_unknown_8bit = TRUE, validate = TRUE)
  } else {
    x
  }
})

guardar_datos(matricula_totales, path_base, "matricula_totales")

### 5.5. IAM - AAM --------------------------------------------------------------------------------

lectura_base <- function(path_a) { 
  temp <-
    read_excel(path = path_a, col_types = "text") %>%  # Leer todas las columnas como texto
    clean_names()
  return(temp)
}

# Rutas de los archivos
path_base <- "../DATA CONSOLIDADA/"

# Leer los datos
matricula_totales <- read_excel(paste0(path_base, "matricula_totales.xlsx"))
matricula_nuevos <- read_excel(paste0(path_base, "matricula_nuevos.xlsx"))
admitidos <- read_excel(paste0(path_base, "admitidos.xlsx"))
aspirantes <- read_excel(paste0(path_base, "Aspirantes.xlsx"))

# Asignar Facultad

# Función para asignar la facultad basada en el programa
asignar_facultad <- function(programa) {
  case_when(
    programa %in% c("ESPECIALIZACIÓN EN BIG DATA", "INGENIERÍA DE MATERIALES", "INGENIERÍA DE SOFTWARE", 
                    "INGENIERÍA ELÉCTRICA", "INGENIERÍA MECÁNICA", "MAESTRÍA EN CIENCIAS COMPUTACIONALES - INVESTIGACION",
                    "MAESTRÍA EN CIENCIAS COMPUTACIONALES - PROFUNDIZACION", "MAESTRÍA EN ENERGÍA",
                    "TÉCNICA PROFESIONAL EN REDES ELÉCTRICAS DE DISTRIBUCIÓN DE ENERGÍA", "TECNOLOGÍA ELÉCTRICA",
                    "TECNOLOGÍA ELECTRÓNICA", "TECNOLOGÍA EN DESARROLLO DE SOFTWARE", "TECNOLOGÍA EN ELECTROMECÁNICA",
                    "TECNOLOGÍA EN GESTIÓN DE MANTENIMIENTO ELECTROMECÁNICO", "TECNOLOGÍA EN GESTIÓN DEL MANTENIMIENTO AERONÁUTICO",
                    "TECNOLOGÍA EN INFORMÁTICA", "TECNOLOGÍA EN MANTENIMIENTO DE AERONAVES", "TECNOLOGÍA EN MECÁNICA",
                    "TECNOLOGÍA EN MECÁNICA AUTOMOTRIZ", "TECNOLOGÍA EN MECATRÓNICA", "TECNOLOGÍA EN SISTEMAS ELECTROMECÁNICOS",
                    "TECNOLOGÍA EN SISTEMAS MECATRÓNICOS", "TECNOLOGÍA EN SUPERVISIÓN DE SISTEMAS DE GENERACIÓN Y DISTRIBUCIÓN DE ENERGÍA ELÉCTRICA",
                    "TECNOLOGÍA EN SUPERVISIÓN DE SISTEMAS ELÉCTRICOS DE POTENCIA", "TECNOLOGÍA MECÁNICA INDUSTRIAL") ~ "Facultad de Ingeniería",
    programa %in% c("ESPECIALIZACIÓN EN GESTIÓN DE PROYECTOS", "INGENIERÍA ADMINISTRATIVA", "INGENIERÍA EN LOGÍSTICA",
                    "INGENIERÍA INDUSTRIAL", "MAESTRÍA EN DISEÑO Y EVALUACIÓN DE PROYECTOS  REGIONALES", "MAESTRÍA EN GERENCIA DE LA TRANSFORMACIÓN DIGITAL PROFUNDIZACIÓN",
                    "PROFESIONAL EN DISEÑO DE VESTUARIO", "PROFESIONAL EN DISEÑO GRÁFICO", "PROFESIONAL EN GESTIÓN DEL DISEÑO",
                    "TECNOLOGÍA EN ANIMACIÓN DIGITAL", "TECNOLOGÍA EN DISEÑO GRÁFICO", "TECNOLOGÍA EN DISEÑO TEXTIL Y DE MODAS",
                    "TECNOLOGÍA EN DISEÑO TEXTIL Y PRODUCCIÓN DE MODAS", "TECNOLOGÍA EN DISEÑO Y GESTIÓN DE LA IMÁGEN",
                    "TECNOLOGÍA EN ELECTRÓNICA INDUSTRIAL", "TECNOLOGÍA EN GESTIÓN DEL DISEÑO GRÁFICO", "TECNOLOGÍA EN GESTIÓN DEL DISEÑO TEXTIL Y DE MODAS",
                    "TECNOLOGÍA EN GESTIÓN LOGÍSTICA", "TECNOLOGÍA EN OPERACIÓN INTEGRAL DEL TRANSPORTE",
                    "TECNOLOGÍA EN PRODUCCIÓN INDUSTRIAL") ~ "Facultad de Producción y Diseño",
    TRUE ~ "Otra Facultad"  # Para programas no listados
  )
}

asignar_grupo_pago <- function(forma_de_pago) {
  case_when(
    grepl("COOPERATIVAS|FUNDACIONES|EMPRESAS", forma_de_pago) ~ "COOPERATIVAS, FUNDACIONES Y EMPRESAS",
    grepl("FRACCIONAMIENTO DE MATRÍCULA", forma_de_pago) ~ "FRACCIONAMIENTO DE MATRÍCULA",
    grepl("AUXILIOS PÚBLICOS|ICETEX|GENERACION E|SAPIENCIA|PRESUPUESTO PARTICIPATIVO", forma_de_pago) ~ "AUXILIOS PÚBLICOS",
    grepl("RECURSOS PROPIOS", forma_de_pago) ~ "RECURSOS PROPIOS",
    TRUE ~ "Otros"
  )
}

# Combinar las bases de datos en una nueva llamada IAM
# Función para cargar datos desde una ruta específica

cargar_datos <- function(path) {
  files <- list.files(path = path, recursive = TRUE, full.names = TRUE)
  data <- map(files, ~ lectura_base(.x, encoding = "UTF-8"))  # Agrega encoding = "UTF-8"
  data_binded <- bind_rows(data, .id = "id") %>%
    mutate(file = basename(files))  # Añade el nombre del archivo base a los datos
  return(data_binded)
}

# Cargar datos para cada categoría
matriculados_totales <- cargar_datos("../RAW DATA/Matriculados_totales")
matriculados_nuevos <- cargar_datos("../RAW DATA/Matriculados_nuevos")
admitidos <- cargar_datos("../RAW DATA/Admitidos")
aspirantes <- cargar_datos("../RAW DATA/Aspirantes")

# Combinar todos los datos en un solo dataframe IAM

IAM <- bind_rows(matricula_totales, matricula_nuevos, admitidos, aspirantes) %>%
  mutate(
    fecha_iam = calcular_fecha(as.character(periodo)),
    semestre = as.numeric(substr(periodo, 5, 5)),
    file_grupos = gsub("_", " ", tools::file_path_sans_ext(file)),
    file_grupos = substr(file_grupos, 1, nchar(file_grupos) - 5),
    Facultad = asignar_facultad(programa),
    forma_de_pago_agrupada = asignar_grupo_pago(forma_de_pago),
    EdadConDefault = if_else(is.na(edad) | edad == "", 0, as.numeric(edad))
  )

# Convertir las cadenas de texto a UTF-8 con manejo de bytes desconocidos y validación
IAM[] <- lapply(IAM, function(x) {
  if (is.character(x)) {
    stri_enc_toutf8(x, is_unknown_8bit = TRUE, validate = TRUE)
  } else {
    x
  }
})

guardar_datos(IAM, path_base, "IAM")

### 5.6. OTROS --------------------------------------------------------------------------------

listado_notas <-
  data.frame(file=list.files(path = "../RAW DATA/Listado de notas/", recursive = T), 
             ruta=list.files(path = "../RAW DATA/Listado de notas/", recursive = T, full.names = T)) %>% 
  mutate(data=map(.x=ruta, ~ lectura_base(.x))) %>% 
  unnest()

save(listado_notas, file = "../DATA CONSOLIDADA/listado_notas.rdata")
openxlsx::write.xlsx(listado_notas, file ="../DATA CONSOLIDADA/listado_notas.xlsx" )

desertores <-
  data.frame(file=list.files(path = "../RAW DATA/Desertores/", recursive = T), 
             ruta=list.files(path = "../RAW DATA/Desertores/", recursive = T, full.names = T)) %>% 
  mutate(data=map(.x=ruta, ~ lectura_base(.x))) %>% 
  unnest()

save(desertores, file = "../DATA CONSOLIDADA/desertores.rdata")
openxlsx::write.xlsx(desertores, file ="../DATA CONSOLIDADA/desertores.xlsx")


