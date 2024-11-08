
# MODELO LONGITUDINAL

data_programas <- read.csv("/Users/cristianespinal/Desktop/data_programas.csv", stringsAsFactors = FALSE)

str(IAM)

library(dplyr)
library(stringr)

# Crear una versión resumida de la base de datos IAM conservando programa, facultad, nivel y file_grupos
IAM_resumido <- IAM %>%
  group_by(periodo, programa, facultad, nivel, file_grupos) %>%
  summarise(
    total_estudiantes = n(),  # Total de estudiantes por programa, facultad, nivel y file_grupos
    .groups = "drop"
  )

# Limpiar los valores de la columna file_grupos y renombrar
IAM_resumido <- IAM_resumido %>%
  mutate(file_grupos = str_trim(file_grupos),  # Eliminar espacios en blanco al inicio y final
         file_grupos = case_when(
           file_grupos == "matriculados " ~ "Matriculados Totales",
           file_grupos == "ListadoDeAdmitidos " ~ "Admitidos",
           file_grupos == "ListadoDeAspirantes " ~ "Aspirantes",
           file_grupos == "matriculados nuevos " ~ "Matriculados Nuevos",
           TRUE ~ file_grupos
         ))

# Continuar con el resto del código para cálculos y agregaciones
IAM_resumido <- IAM_resumido %>%
  mutate(
    anio = as.integer(substr(periodo, 1, 4)),  # Extraer los primeros cuatro caracteres del periodo para obtener el año
    semestre = as.integer(substr(periodo, 5, 5)),  # Extraer el último carácter del periodo para obtener el semestre
    fecha = as.Date(paste(anio, ifelse(semestre == 1, "01-01", "07-01"), sep = "-"))  # Crear la fecha con base en el año y semestre
  )

# Agrupar la información por programa, periodo y file_grupos, sumando los totales
IAM_resumido <- IAM_resumido %>%
  group_by(periodo, programa, file_grupos) %>%
  summarise(
    total_estudiantes = sum(total_estudiantes),
    facultad = first(facultad),
    nivel = first(nivel),
    anio = first(anio),
    semestre = first(semestre),
    fecha = first(fecha),
    .groups = "drop"
  )

# Limpiar los valores de la columna file_grupos y renombrar
IAM_resumido <- IAM_resumido %>%
  mutate(file_grupos = str_trim(file_grupos),  # Eliminar espacios en blanco al inicio y final
         file_grupos = case_when(
           str_detect(tolower(file_grupos), "matriculados nuevos") ~ "Matriculados Nuevos",
           str_detect(tolower(file_grupos), "matriculados") ~ "Matriculados Totales",
           str_detect(tolower(file_grupos), "listadodeadmitidos") ~ "Admitidos",
           str_detect(tolower(file_grupos), "listadodeaspirantes") ~ "Aspirantes",
           TRUE ~ file_grupos
         ))

# Verificar si se realizó el cambio correctamente
unique(IAM_resumido$file_grupos)

# Calcular tasas de selectividad, absorción y conversión directamente desde IAM_resumido
IAM_tasas <- IAM_resumido %>%
  group_by(periodo, programa) %>%
  summarise(
    facultad = first(facultad),
    nivel = first(nivel),
    Aspirantes = sum(total_estudiantes[file_grupos == "Aspirantes"], na.rm = TRUE),
    Admitidos = sum(total_estudiantes[file_grupos == "Admitidos"], na.rm = TRUE),
    Matriculados_Nuevos = sum(total_estudiantes[file_grupos == "Matriculados Nuevos"], na.rm = TRUE),
    Matriculados_Totales = sum(total_estudiantes[file_grupos == "Matriculados Totales"], na.rm = TRUE),
    Tasa_Selectividad = ifelse(Aspirantes > 0, Admitidos / Aspirantes, NA),
    Tasa_Absorcion = ifelse(Admitidos > 0, Matriculados_Nuevos / Admitidos, NA),
    Tasa_Conversion = ifelse(Aspirantes > 0, Matriculados_Nuevos / Aspirantes, NA),
    .groups = "drop"
  )

# Ver el resultado
print(IAM_tasas)

# Agregar nivel de formación desde data_programas a IAM_tasas
IAM_tasas <- IAM_tasas %>%
  left_join(data_programas %>% select(programa, Nivel_Formacion = `Nivel.de.formación..grupos.`), by = "programa")

IAM_tasas <- IAM_tasas %>%
  left_join(data_programas %>% select(programa, facultad = `Facultad.1..grupos.`), by = "programa")

IAM_tasas <- IAM_tasas %>%
  select(-facultad.x, -nivel) %>%  # Eliminar facultad.x y nivel
  rename(Facultad = facultad.y)  # Renombrar facultad.y como Facultad

# Ver el resultado
print(IAM_tasas)

# Ejecución de la función de guardado para Aspirantes
guardar_datos(IAM_tasas, path_base, "IAM_tasas")
