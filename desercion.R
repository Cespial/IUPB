# CALCULO DE TASAS DE DESERCIÓN I.U. PASCUAL BRAVO
# REFERENCIA: https://www.mineducacion.gov.co/sistemasdeinformacion/1783/articles-415244_cambio_mtodologico_spadies_3.pdf

# TASA DE DESERCIÓN ANUAL -------

library(dplyr)

# Definir los periodos disponibles
años <- unique(IAM$periodo)

# Crear un dataframe vacío para almacenar los resultados de TDA
TDA_results <- data.frame(periodo = character(), TDA = numeric(), stringsAsFactors = FALSE)

# Calcular TDA para cada periodo t considerando semestres (t-2 se refiere al mismo semestre del año anterior)
for (t in años) {
  # Obtener el año y el semestre
  anio <- as.numeric(substr(t, 1, 4))
  semestre <- as.numeric(substr(t, 5, 5))
  
  # Determinar el periodo t-2
  if (semestre == 1) {
    t_menos_2 <- paste0(anio - 1, "1")
  } else {
    t_menos_2 <- paste0(anio - 1, "2")
  }
  
  # Filtrar matriculados en el periodo t-2 solo para Matriculados_totales
  matriculados_t_menos_2 <- IAM %>%
    filter(periodo == t_menos_2, grepl("matriculados", file, ignore.case = TRUE))
  
  # Verificar si existen matriculados en el periodo t-2
  if (nrow(matriculados_t_menos_2) == 0) {
    cat("No se encontraron matriculados para el periodo t-2:", t_menos_2, "\n")
    next
  }
  
  # Filtrar desertores en el periodo t
  desertores_t <- desertores %>%
    filter(periodo == t)
  
  # Verificar si existen desertores en el periodo t
  if (nrow(desertores_t) == 0) {
    cat("No se encontraron desertores para el periodo t:", t, "\n")
    next
  }
  
  # Unir bases de datos para identificar desertores que estuvieron matriculados en t-2
  desertores_en_matriculados <- desertores_t %>%
    inner_join(matriculados_t_menos_2, by = c("data_identificacion" = "identificacion"))
  
  # Verificar si existen desertores que estuvieron matriculados en t-2
  if (nrow(desertores_en_matriculados) == 0) {
    cat("No se encontraron desertores que estuvieran matriculados en t-2 para el periodo t:", t, "\n")
    next
  }
  
  # Calcular la Tasa de Desercion Anual (TDA)
  num_desertores <- nrow(desertores_en_matriculados)
  num_matriculados_t_menos_2 <- nrow(matriculados_t_menos_2)
  
  TDA <- (num_desertores / num_matriculados_t_menos_2) * 100
  
  # Imprimir valores para validación
  cat("Periodo t:", t, "\n")
  cat("Periodo t-2:", t_menos_2, "\n")
  cat("Número de desertores en t:", nrow(desertores_t), "\n")
  cat("Número de matriculados en t-2:", num_matriculados_t_menos_2, "\n")
  cat("Número de desertores que estuvieron matriculados en t-2:", num_desertores, "\n")
  cat("TDA calculada:", round(TDA, 2), "%\n\n")
  
  # Agregar el resultado al dataframe
  TDA_results <- rbind(TDA_results, data.frame(periodo = t, TDA = round(TDA, 2)))
}

# Mostrar resultados
print(TDA_results)

guardar_datos(TDA_results, path_base, "Tasa_Desercion_Anual")

# TASA DE DESERCIÓN POR COHORTES ------

library(dplyr)

# Definir los periodos disponibles
años <- unique(IAM$periodo)

# Crear un dataframe vacío para almacenar los resultados de TDC
TDC_results <- data.frame(cohorte = character(), semestre = character(), TDC = numeric(), stringsAsFactors = FALSE)

# Verificación previa de datos
# Verificar si existen registros con file_grupos == "matriculados nuevos"
if (!"file_grupos" %in% colnames(IAM)) {
  stop("La columna 'file_grupos' no existe en la base de datos IAM.")
}

# Verificar si hay registros de primíparos (considerando posibles espacios extra)
IAM$file_grupos <- trimws(IAM$file_grupos)
primiparos_existentes <- IAM %>% filter(file_grupos == "matriculados nuevos")
if (nrow(primiparos_existentes) == 0) {
  stop("No se encontraron registros de primíparos ('matriculados nuevos') en la base de datos IAM.")
}

# Calcular TDC para cada cohorte y semestre
for (c in años) {
  # Filtrar primíparos de la cohorte c
  primiparos_c <- IAM %>%
    filter(periodo == c, file_grupos == "matriculados nuevos")
  
  # Verificar si existen primíparos en la cohorte c
  if (nrow(primiparos_c) == 0) {
    cat("No se encontraron primíparos para la cohorte:", c, "\n")
    next
  }
  
  # Obtener el año y el semestre de la cohorte
  anio <- as.numeric(substr(c, 1, 4))
  semestre <- as.numeric(substr(c, 5, 5))
  
  # Calcular el periodo hasta el cual se va a acumular la deserción
  for (s in 1:2) {  # Se consideran los primeros dos años (cuatro semestres)
    if (semestre == 1) {
      periodo_s <- paste0(anio + (s - 1), ifelse(s %% 2 == 1, "1", "2"))
    } else {
      periodo_s <- paste0(anio + (s - 1), ifelse(s %% 2 == 1, "2", "1"))
    }
    
    # Filtrar desertores de la cohorte c hasta el semestre s
    desertores_c_s <- desertores %>%
      filter(periodo == periodo_s)
    
    # Verificar si existen desertores para el periodo
    if (nrow(desertores_c_s) == 0) {
      cat("No se encontraron desertores para el periodo:", periodo_s, "\n")
      next
    }
    
    # Unir desertores con primíparos para identificar los desertores de la cohorte c
    if (!"data_identificacion" %in% colnames(desertores)) {
      stop("La columna 'data_identificacion' no existe en la base de datos desertores.")
    }
    
    desertores_en_primiparos <- desertores_c_s %>%
      inner_join(primiparos_c, by = c("data_identificacion" = "identificacion"))
    
    # Calcular la Tasa de Deserción por Cohorte (TDC)
    num_desertores <- nrow(desertores_en_primiparos)
    num_primiparos_c <- nrow(primiparos_c)
    
    TDC <- (num_desertores / num_primiparos_c) * 100
    
    # Imprimir valores para validación
    cat("Cohorte:", c, "\n")
    cat("Semestre acumulado:", s, "\n")
    cat("Número de primíparos en la cohorte:", num_primiparos_c, "\n")
    cat("Número de desertores hasta el semestre", s, ":", num_desertores, "\n")
    cat("TDC calculada:", round(TDC, 2), "%\n\n")
    
    # Agregar el resultado al dataframe
    TDC_results <- rbind(TDC_results, data.frame(cohorte = c, semestre = s, TDC = round(TDC, 2)))
  }
}

# Mostrar resultados
print(TDC_results)

# ..

library(dplyr)

# Definir los periodos disponibles a partir del año 20161
periodos <- unique(IAM$periodo)
periodos <- periodos[periodos >= "20161"]

# Crear un dataframe vacío para almacenar los resultados de TDC
TDC_results <- data.frame(periodo = character(), TDC = numeric(), stringsAsFactors = FALSE)

# Verificación previa de datos
# Verificar si existen registros con file_grupos == "matriculados nuevos"
if (!"file_grupos" %in% colnames(IAM)) {
  stop("La columna 'file_grupos' no existe en la base de datos IAM.")
}

# Verificar si hay registros de primíparos (considerando posibles espacios extra)
IAM$file_grupos <- trimws(IAM$file_grupos)
primiparos_existentes <- IAM %>% filter(file_grupos == "matriculados nuevos")
if (nrow(primiparos_existentes) == 0) {
  stop("No se encontraron registros de primíparos ('matriculados nuevos') en la base de datos IAM.")
}

# Calcular TDC para cada periodo
for (p in periodos) {
  # Filtrar primíparos de la cohorte t - 2
  anio <- as.numeric(substr(p, 1, 4))
  semestre <- as.numeric(substr(p, 5, 5))
  
  if (semestre == 1) {
    periodo_t_menos_2 <- paste0(anio - 1, "2")
  } else {
    periodo_t_menos_2 <- paste0(anio - 1, "1")
  }
  
  primiparos_c <- IAM %>%
    filter(periodo == periodo_t_menos_2, file_grupos == "matriculados nuevos")
  
  # Verificar si existen primíparos en el periodo t - 2
  if (nrow(primiparos_c) == 0) {
    cat("No se encontraron primíparos para el periodo t - 2:", periodo_t_menos_2, "\n")
    next
  }
  
  # Filtrar desertores del periodo actual
  desertores_p <- desertores %>%
    filter(periodo == p)
  
  # Verificar si existen desertores para el periodo
  if (nrow(desertores_p) == 0) {
    cat("No se encontraron desertores para el periodo:", p, "\n")
    next
  }
  
  # Unir desertores con primíparos para identificar los desertores de la cohorte t - 2
  if (!"data_identificacion" %in% colnames(desertores)) {
    stop("La columna 'data_identificacion' no existe en la base de datos desertores.")
  }
  
  desertores_en_primiparos <- desertores_p %>%
    inner_join(primiparos_c, by = c("data_identificacion" = "identificacion"))
  
  # Calcular la Tasa de Deserción Anual (TDC)
  num_desertores <- nrow(desertores_en_primiparos)
  num_primiparos_c <- nrow(primiparos_c)
  
  TDC <- (num_desertores / num_primiparos_c) * 100
  
  # Imprimir valores para validación
  cat("Periodo:", p, "\n")
  cat("Número de primíparos en el periodo t - 2:", num_primiparos_c, "\n")
  cat("Número de desertores en el periodo:", num_desertores, "\n")
  cat("TDC calculada:", round(TDC, 2), "%\n\n")
  
  # Agregar el resultado al dataframe
  TDC_results <- rbind(TDC_results, data.frame(periodo = p, TDC = round(TDC, 2)))
}

# Mostrar resultados
print(TDC_results)

guardar_datos(TDC_results, path_base, "Tasa_Desercion_Cohortes")

# TASA DE DESERCIÓN PROMEDIO ACUMULADA ------

library(dplyr)

# Definir los periodos disponibles a partir del año 20161
periodos <- unique(IAM$periodo)
periodos <- periodos[periodos >= "20161" & periodos <= "20232"]

# Crear un dataframe vacío para almacenar los resultados de TDPA
TDPA_results <- data.frame(periodo = character(), TDPA = numeric(), stringsAsFactors = FALSE)

# Verificación previa de datos
# Verificar si existen registros con file_grupos == "matriculados nuevos"
if (!"file_grupos" %in% colnames(IAM)) {
  stop("La columna 'file_grupos' no existe en la base de datos IAM.")
}

# Verificar si hay registros de primíparos (considerando posibles espacios extra)
IAM$file_grupos <- trimws(IAM$file_grupos)
primiparos_existentes <- IAM %>% filter(file_grupos == "matriculados nuevos")
if (nrow(primiparos_existentes) == 0) {
  stop("No se encontraron registros de primíparos ('matriculados nuevos') en la base de datos IAM.")
}

# Calcular TDPA acumulada para cada periodo desde 20161 hasta 20232
for (p in periodos) {
  num_desertores_acumulado <- 0
  num_primiparos_acumulado <- 0
  
  # Iterar sobre las cohortes hasta el periodo actual
  cohortes <- periodos[periodos <= p]
  for (c in cohortes) {
    # Filtrar primíparos de la cohorte c
    primiparos_c <- IAM %>%
      filter(periodo == c, file_grupos == "matriculados nuevos")
    
    # Verificar si existen primíparos en la cohorte c
    if (nrow(primiparos_c) == 0) {
      next
    }
    
    # Acumular el número de primíparos
    num_primiparos_acumulado <- num_primiparos_acumulado + nrow(primiparos_c)
    
    # Filtrar desertores de la cohorte c hasta el periodo actual
    desertores_c_p <- desertores %>%
      filter(periodo == p)
    
    # Verificar si existen desertores para el periodo
    if (nrow(desertores_c_p) == 0) {
      next
    }
    
    # Unir desertores con primíparos para identificar los desertores de la cohorte c
    desertores_en_primiparos <- desertores_c_p %>%
      inner_join(primiparos_c, by = c("data_identificacion" = "identificacion"))
    
    # Acumular el número de desertores
    num_desertores_acumulado <- num_desertores_acumulado + nrow(desertores_en_primiparos)
  }
  
  # Calcular la Tasa de Deserción Promedio Acumulada (TDPA)
  if (num_primiparos_acumulado > 0) {
    TDPA <- (num_desertores_acumulado / num_primiparos_acumulado) * 100
  } else {
    TDPA <- NA
  }
  
  # Imprimir valores para validación
  cat("Periodo:", p, "\n")
  cat("Número acumulado de primíparos:", num_primiparos_acumulado, "\n")
  cat("Número acumulado de desertores hasta el periodo", p, ":", num_desertores_acumulado, "\n")
  cat("TDPA calculada:", round(TDPA, 2), "%\n\n")
  
  # Agregar el resultado al dataframe
  TDPA_results <- rbind(TDPA_results, data.frame(periodo = p, TDPA = round(TDPA, 2)))
}

# Mostrar resultados
print(TDPA_results)

guardar_datos(TDPA_results, path_base, "Tasa_Desercion_Promedio_Acumulada")

# TASA DE SUPERVIVENCIA -------

library(dplyr)

# Definir los periodos disponibles a partir del año 20161
periodos <- unique(IAM$periodo)
periodos <- periodos[periodos >= "20161" & periodos <= "20232"]

# Crear un dataframe vacío para almacenar los resultados de TS
TS_results <- data.frame(semestre = character(), TS = numeric(), stringsAsFactors = FALSE)

# Verificación previa de datos
# Verificar si existen registros con file_grupos == "matriculados nuevos"
if (!"file_grupos" %in% colnames(IAM)) {
  stop("La columna 'file_grupos' no existe en la base de datos IAM.")
}

# Verificar si hay registros de primíparos (considerando posibles espacios extra)
IAM$file_grupos <- trimws(IAM$file_grupos)
primiparos_existentes <- IAM %>% filter(file_grupos == "matriculados nuevos") %>% distinct(identificacion, .keep_all = TRUE)
if (nrow(primiparos_existentes) == 0) {
  stop("No se encontraron registros de primíparos ('matriculados nuevos') en la base de datos IAM.")
}

# Calcular la Tasa de Supervivencia (TS) para cada semestre desde 20161 hasta 20232
for (p in periodos) {
  # Filtrar matriculados del periodo actual
  matriculados_p <- IAM %>%
    filter(periodo == p, file_grupos == "matriculados") %>%
    distinct(identificacion, .keep_all = TRUE)
  
  # Verificar si existen matriculados para el periodo
  if (nrow(matriculados_p) == 0) {
    cat("No se encontraron matriculados para el periodo:", p, "\n")
    next
  }
  
  # Obtener el número de primíparos del periodo anterior t-1
  if (substr(p, 5, 5) == "1") {
    periodo_anterior <- paste0(as.character(as.numeric(substr(p, 1, 4)) - 1), "2")
  } else {
    periodo_anterior <- paste0(substr(p, 1, 4), "1")
  }
  
  primiparos_anterior <- IAM %>%
    filter(periodo == periodo_anterior, file_grupos == "matriculados nuevos") %>%
    distinct(identificacion, .keep_all = TRUE)
  
  # Verificar si existen primíparos en el periodo anterior
  if (nrow(primiparos_anterior) == 0) {
    cat("No se encontraron primíparos para el periodo anterior:", periodo_anterior, "\n")
    next
  }
  
  # Filtrar primíparos que continúan matriculados en el periodo actual
  primiparos_continuos <- primiparos_anterior %>%
    filter(identificacion %in% matriculados_p$identificacion)
  
  # Calcular la Tasa de Supervivencia (TS)
  num_primiparos_anterior <- nrow(primiparos_anterior)
  num_primiparos_continuos <- nrow(primiparos_continuos)
  
  if (num_primiparos_anterior > 0) {
    TS <- (num_primiparos_continuos / num_primiparos_anterior) * 100
  } else {
    TS <- NA
  }
  
  # Imprimir valores para validación
  cat("Periodo:", p, "\n")
  cat("Número de primíparos del periodo anterior:", num_primiparos_anterior, "\n")
  cat("Número de primíparos que continúan matriculados en el periodo actual:", num_primiparos_continuos, "\n")
  cat("TS calculada:", round(TS, 2), "%\n\n")
  
  # Agregar el resultado al dataframe
  TS_results <- rbind(TS_results, data.frame(semestre = p, TS = round(TS, 2)))
}

# Mostrar resultados
print(TS_results)

guardar_datos(TS_results, path_base, "Tasa_Supervivencia")

# AGRUPAR RESULTADOS

# Agregar una columna de identificación a cada base de datos
TDA_results$tipo <- "Tasa de Deserción Anual"
TDC_results$tipo <- "Tasa de Deserción por Cohortes"
TDPA_results$tipo <- "Tasa de Deserción Promedio Acumulada"

# Cambiar el nombre de la columna de tasa para que sean consistentes
colnames(TDA_results)[2] <- "tasa"
colnames(TDC_results)[2] <- "tasa"
colnames(TDPA_results)[2] <- "tasa"

# Unir las bases de datos en una sola
desercion_por_tipos <- rbind(TDA_results, TDC_results, TDPA_results)

# Crear las columnas año y semestre a partir de la columna periodo
desercion_por_tipos$año <- substr(desercion_por_tipos$periodo, 1, 4)
desercion_por_tipos$semestre <- substr(desercion_por_tipos$periodo, 5, 5)

# Verificar el resultado
head(desercion_por_tipos)

# Verificar el resultado
str(desercion_por_tipos)

guardar_datos(desercion_por_tipos, path_base, "desercion_por_tipos")
