library(tidyverse)
library(readxl)
library(janitor)
library(dplyr)

#########
## MATRICUALA DE ESTUDIANTES POR CLASES
#######

lectura_base <- function(path_a){ 
  print(path_a)
  temp<-
    read_excel(path = path_a) %>% 
    clean_names()
  
  return(temp)
  
}

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

# Modificado por Crisitan Espinal
# Cambio en lectura_base en caso de error

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

admitidos <-
  data.frame(file=list.files(path = "../RAW DATA/Admitidos", recursive = T), 
             ruta=list.files(path = "../RAW DATA/Admitidos", recursive = T, full.names = T)) %>% 
  mutate(data=map(.x=ruta, ~ lectura_base(.x))) %>% 
  unnest()

save(admitidos, file = "../RAW DATA/DATA CONSOLIDADA/admitidos.rdata")
openxlsx::write.xlsx(admitidos, file ="../RAW DATA/DATA CONSOLIDADA/admitidos.xlsx" )

aspirantes <-
  data.frame(file=list.files(path = "../RAW DATA/RAW DATA/Aspirantes", recursive = T), 
             ruta=list.files(path = "../RAW DATA/RAW DATA/Aspirantes", recursive = T, full.names = T)) %>% 
  mutate(data=map(.x=ruta, ~ lectura_base(.x))) %>% 
  unnest()

aspirantes <- aspirantes %>%
  mutate(
    fecha_de_inscripcion = as.Date(fecha_de_inscripcion),
  ) %>%
  mutate(
    fecha_de_inscripcion = if_else(periodo == "20172", as.Date("2017-08-01"), fecha_de_inscripcion),
  )

save(aspirantes, file = "../RAW DATA/DATA CONSOLIDADA/Aspirantes.rdata")
write_xlsx(aspirantes, path = "../RAW DATA/DATA CONSOLIDADA/Aspirantes.xlsx")
write.csv(IAM, file = "../RAW DATA/DATA CONSOLIDADA/Aspirantes.csv", row.names = FALSE, fileEncoding = "UTF-8")

matricula_nuevos <-
  data.frame(file=list.files(path = "../RAW DATA/RAW DATA/Matriculados_nuevos", recursive = T), 
             ruta=list.files(path = "../RAW DATA/RAW DATA/Matriculados_nuevos", recursive = T, full.names = T)) %>% 
  mutate(data=map(.x=ruta, ~ lectura_base(.x))) %>% 
  unnest()

matricula_nuevos <- matricula_nuevos %>%
  mutate(
    fecha_de_matricula = as.Date(fecha_de_matricula),
  ) %>%
  mutate(
    fecha_de_matricula = if_else(periodo == "20172", as.Date("2017-08-01"), fecha_de_matricula),
  )

save(matricula_nuevos, file = "../RAW DATA/DATA CONSOLIDADA/Matriculados_nuevos.rdata")
openxlsx::write.xlsx(matricula_nuevos, file ="../RAW DATA/DATA CONSOLIDADA/Matriculados_nuevos.xlsx")

matriculados_totales <-
  data.frame(file=list.files(path = "../RAW DATA/RAW DATA/Matriculados_totales", recursive = T), 
             ruta=list.files(path = "../RAW DATA/RAW DATA/Matriculados_totales", recursive = T, full.names = T)) %>% 
  mutate(data=map(.x=ruta, ~ lectura_base(.x))) %>% 
  unnest()

matriculados_totales <- matriculados_totales %>%
  mutate(
    fecha_de_matricula = as.Date(fecha_de_matricula),
  ) %>%
  mutate(
    fecha_de_matricula = if_else(periodo == "20172", as.Date("2017-08-01"), fecha_de_matricula),
  )

save(matriculados_totales, file = "../RAW DATA/DATA CONSOLIDADA/matriculados_totales.rdata")
openxlsx::write.xlsx(matriculados_totales, file ="../RAW DATA/DATA CONSOLIDADA/matriculados_totales.xlsx" )

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
openxlsx::write.xlsx(desertores, file ="../DATA CONSOLIDADA/desertores.xlsx" )


admitidos <-
  data.frame(file=list.files(path = "../RAW DATA/Admitidos/", recursive = T), 
             ruta=list.files(path = "../RAW DATA/Admitidos/", recursive = T, full.names = T)) %>% 
  mutate(data=map(.x=ruta, ~ lectura_base(.x))) %>% 
  unnest()


save(admitidos, file = "../DATA CONSOLIDADA/admitidos.rdata")
openxlsx::write.xlsx(admitidos, file ="../DATA CONSOLIDADA/admitidos.xlsx" )

# Modificado por Crisitan Espinal
# Cambio en lectura_base para procesar IAM

lectura_base <- function(path_a) { 
  temp <-
    read_excel(path = path_a, col_types = "text") %>%  # Leer todas las columnas como texto
    clean_names()
  
  return(temp)
}

IAM <-
  data.frame(file=list.files(path = "../RAW DATA/IAM", recursive = T), 
             ruta=list.files(path = "../RAW DATA/IAM", recursive = T, full.names = T)) %>% 
  mutate(data=map(.x=ruta, ~ lectura_base(.x))) %>% 
  unnest(cols = c(data), names_sep = "_")

IAM <- IAM %>%
  mutate(
    data_fecha_de_inscripcion = as.Date(data_fecha_de_inscripcion),
    data_fecha_de_admision = as.Date(data_fecha_de_admision)
  ) %>%
  mutate(
    data_fecha_de_inscripcion = if_else(data_periodo == "20172", as.Date("2017-08-01"), data_fecha_de_inscripcion),
    data_fecha_de_admision = if_else(data_periodo == "20172", as.Date("2017-08-01"), data_fecha_de_admision)
  )

save(IAM, file = "../RAW DATA/DATA CONSOLIDADA/IAM.rdata")
write.xlsx(IAM, path = "../RAW DATA/DATA CONSOLIDADA/IAM.xlsx")
write.csv(IAM, file = "../RAW DATA/DATA CONSOLIDADA/IAM.csv", row.names = FALSE, fileEncoding = "UTF-8")
