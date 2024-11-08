
# INDICADORES

# TASA DE ABSORCIÓN POR PROGRAMA -------

# Definir los periodos disponibles a partir del año 20161 hasta 20232
periodos <- unique(IAM$periodo)
periodos <- periodos[periodos >= "20161" & periodos <= "20232"]

# Obtener la lista de programas
programas <- unique(IAM$programa)

# Crear un dataframe vacío para almacenar los resultados de la Tasa de Absorción (TA)
TA_results_programa <- data.frame(programa = character(), semestre = character(), TA = numeric(), stringsAsFactors = FALSE)

# Verificación previa de datos
# Verificar si existen registros con file_grupos == "ListadoDeAdmitidos"
if (!"file_grupos" %in% colnames(IAM)) {
  stop("La columna 'file_grupos' no existe en la base de datos IAM.")
}

# Verificar si hay registros de admitidos
IAM$file_grupos <- trimws(IAM$file_grupos)
admitidos_existentes <- IAM %>% filter(file_grupos == "ListadoDeAdmitidos") %>% distinct(identificacion, .keep_all = TRUE)
if (nrow(admitidos_existentes) == 0) {
  stop("No se encontraron registros de admitidos ('ListadoDeAdmitidos') en la base de datos IAM.")
}

# Calcular la Tasa de Absorción (TA) para cada programa y cada semestre
for (prog in programas) {
  for (p in periodos) {
    # Filtrar admitidos del periodo actual para el programa actual
    admitidos_p <- IAM %>%
      filter(periodo == p, file_grupos == "ListadoDeAdmitidos", programa == prog) %>%
      distinct(identificacion, .keep_all = TRUE)
    
    # Verificar si existen admitidos para el periodo y programa
    if (nrow(admitidos_p) == 0) {
      cat("No se encontraron admitidos para el programa:", prog, "y periodo:", p, "\n")
      next
    }
    
    # Filtrar matriculados nuevos del periodo actual para el programa actual
    matriculados_nuevos_p <- IAM %>%
      filter(periodo == p, file_grupos == "matriculados nuevos", programa == prog) %>%
      distinct(identificacion, .keep_all = TRUE)
    
    # Verificar si existen matriculados nuevos en el periodo para el programa
    if (nrow(matriculados_nuevos_p) == 0) {
      cat("No se encontraron matriculados nuevos para el programa:", prog, "y periodo:", p, "\n")
      next
    }
    
    # Calcular la Tasa de Absorción (TA)
    num_admitidos <- nrow(admitidos_p)
    num_matriculados_nuevos <- nrow(matriculados_nuevos_p)
    
    if (num_admitidos > 0) {
      TA <- (num_matriculados_nuevos / num_admitidos) * 100
    } else {
      TA <- NA
    }
    
    # Imprimir valores para validación
    cat("Programa:", prog, "\n")
    cat("Periodo:", p, "\n")
    cat("Número de admitidos:", num_admitidos, "\n")
    cat("Número de matriculados nuevos:", num_matriculados_nuevos, "\n")
    cat("TA calculada:", round(TA, 2), "%\n\n")
    
    # Agregar el resultado al dataframe
    TA_results_programa <- rbind(TA_results_programa, data.frame(programa = prog, semestre = p, TA = round(TA, 2)))
  }
}

# Mostrar resultados
print(TA_results_programa)

# Guardar resultados
guardar_datos(TA_results_programa, path_base, "Tasa_Absorcion_Programas")





# TASA DE ABSORCIÓN ACOTADA POR PROGRAMA -------

# Definir los periodos disponibles a partir del año 20161 hasta 20232
periodos <- unique(IAM$periodo)
periodos <- periodos[periodos >= "20161" & periodos <= "20232"]

# Obtener la lista de programas
programas <- unique(IAM$programa)

# Crear un dataframe vacío para almacenar los resultados de la Tasa de Absorción Acotada (TAA)
TAA_results_programa <- data.frame(programa = character(), semestre = character(), TAA = numeric(), stringsAsFactors = FALSE)

# Verificación previa de datos
# Verificar si existen registros con file_grupos == "ListadoDeAdmitidos"
if (!"file_grupos" %in% colnames(IAM)) {
  stop("La columna 'file_grupos' no existe en la base de datos IAM.")
}

# Verificar si hay registros de admitidos
IAM$file_grupos <- trimws(IAM$file_grupos)
admitidos_existentes <- IAM %>% filter(file_grupos == "ListadoDeAdmitidos") %>% distinct(identificacion, .keep_all = TRUE)
if (nrow(admitidos_existentes) == 0) {
  stop("No se encontraron registros de admitidos ('ListadoDeAdmitidos') en la base de datos IAM.")
}

# Calcular la Tasa de Absorción Acotada (TAA) para cada programa y cada semestre
for (prog in programas) {
  for (p in periodos) {
    # Filtrar admitidos del periodo actual para el programa actual
    admitidos_p <- IAM %>%
      filter(periodo == p, file_grupos == "ListadoDeAdmitidos", programa == prog) %>%
      distinct(identificacion, .keep_all = TRUE)
    
    # Verificar si existen admitidos para el periodo y programa
    if (nrow(admitidos_p) == 0) {
      cat("No se encontraron admitidos para el programa:", prog, "y periodo:", p, "\n")
      next
    }
    
    # Filtrar matriculados nuevos del periodo actual para el programa actual
    matriculados_nuevos_p <- IAM %>%
      filter(periodo == p, file_grupos == "matriculados nuevos", programa == prog) %>%
      distinct(identificacion, .keep_all = TRUE)
    
    # Verificar si existen matriculados nuevos en el periodo para el programa
    if (nrow(matriculados_nuevos_p) == 0) {
      cat("No se encontraron matriculados nuevos para el programa:", prog, "y periodo:", p, "\n")
      next
    }
    
    # Calcular la Tasa de Absorción Acotada (TAA)
    num_admitidos <- nrow(admitidos_p)
    num_matriculados_nuevos <- nrow(matriculados_nuevos_p)
    
    if (num_admitidos > 0) {
      TAA <- (num_matriculados_nuevos / num_admitidos) * 100
    } else {
      TAA <- NA
    }
    
    # Imprimir valores para validación
    cat("Programa:", prog, "\n")
    cat("Periodo:", p, "\n")
    cat("Número de admitidos:", num_admitidos, "\n")
    cat("Número de matriculados nuevos:", num_matriculados_nuevos, "\n")
    cat("TAA calculada:", round(TAA, 2), "%\n\n")
    
    # Agregar el resultado al dataframe
    TAA_results_programa <- rbind(TAA_results_programa, data.frame(programa = prog, semestre = p, TAA = round(TAA, 2)))
  }
}

# Mostrar resultados
print(TAA_results_programa)

# Guardar resultados
guardar_datos(TAA_results_programa, path_base, "Tasa_Absorcion_Acotada_Programas")


# TASA DE CONVERSIÓN POR PROGRAMA -------

# Definir los periodos disponibles a partir del año 20161 hasta 20232
periodos <- unique(IAM$periodo)
periodos <- periodos[periodos >= "20161" & periodos <= "20232"]

# Obtener la lista de programas
programas <- unique(IAM$programa)

# Crear un dataframe vacío para almacenar los resultados de la Tasa de Conversión (TC)
TC_results_programa <- data.frame(programa = character(), semestre = character(), TC = numeric(), stringsAsFactors = FALSE)

# Verificación previa de datos
# Verificar si existen registros con file_grupos == "ListadoDeAspirantes"
if (!"file_grupos" %in% colnames(IAM)) {
  stop("La columna 'file_grupos' no existe en la base de datos IAM.")
}

# Verificar si hay registros de aspirantes
IAM$file_grupos <- trimws(IAM$file_grupos)
aspirantes_existentes <- IAM %>% filter(file_grupos == "ListadoDeAspirantes") %>% distinct(identificacion, .keep_all = TRUE)
if (nrow(aspirantes_existentes) == 0) {
  stop("No se encontraron registros de aspirantes ('ListadoDeAspirantes') en la base de datos IAM.")
}

# Calcular la Tasa de Conversión (TC) para cada programa y cada semestre
for (prog in programas) {
  for (p in periodos) {
    # Filtrar aspirantes del periodo actual para el programa actual
    aspirantes_p <- IAM %>%
      filter(periodo == p, file_grupos == "ListadoDeAspirantes", programa == prog) %>%
      distinct(identificacion, .keep_all = TRUE)
    
    # Verificar si existen aspirantes para el periodo y programa
    if (nrow(aspirantes_p) == 0) {
      cat("No se encontraron aspirantes para el programa:", prog, "y periodo:", p, "\n")
      next
    }
    
    # Filtrar matriculados nuevos del periodo actual para el programa actual
    matriculados_nuevos_p <- IAM %>%
      filter(periodo == p, file_grupos == "matriculados nuevos", programa == prog) %>%
      distinct(identificacion, .keep_all = TRUE)
    
    # Verificar si existen matriculados nuevos en el periodo para el programa
    if (nrow(matriculados_nuevos_p) == 0) {
      cat("No se encontraron matriculados nuevos para el programa:", prog, "y periodo:", p, "\n")
      next
    }
    
    # Calcular la Tasa de Conversión (TC)
    num_aspirantes <- nrow(aspirantes_p)
    num_matriculados_nuevos <- nrow(matriculados_nuevos_p)
    
    if (num_aspirantes > 0) {
      TC <- (num_matriculados_nuevos / num_aspirantes) * 100
    } else {
      TC <- NA
    }
    
    # Imprimir valores para validación
    cat("Programa:", prog, "\n")
    cat("Periodo:", p, "\n")
    cat("Número de aspirantes:", num_aspirantes, "\n")
    cat("Número de matriculados nuevos:", num_matriculados_nuevos, "\n")
    cat("TC calculada:", round(TC, 2), "%\n\n")
    
    # Agregar el resultado al dataframe
    TC_results_programa <- rbind(TC_results_programa, data.frame(programa = prog, semestre = p, TC = round(TC, 2)))
  }
}

# Mostrar resultados
print(TC_results_programa)

# Guardar resultados
guardar_datos(TC_results_programa, path_base, "Tasa_Conversion_Programas")




# TASA DE SELECTIVIDAD POR PROGRAMA -------

# Definir los periodos disponibles a partir del año 20161 hasta 20232
periodos <- unique(IAM$periodo)
periodos <- periodos[periodos >= "20161" & periodos <= "20232"]

# Obtener la lista de programas
programas <- unique(IAM$programa)

# Crear un dataframe vacío para almacenar los resultados de la Tasa de Selectividad (TSel)
TSel_results_programa <- data.frame(programa = character(), semestre = character(), TSel = numeric(), stringsAsFactors = FALSE)

# Verificación previa de datos
# Verificar si existen registros con file_grupos == "ListadoDeAspirantes" o "ListadoDeAdmitidos"
if (!"file_grupos" %in% colnames(IAM)) {
  stop("La columna 'file_grupos' no existe en la base de datos IAM.")
}

# Verificar si hay registros de aspirantes
IAM$file_grupos <- trimws(IAM$file_grupos)
aspirantes_existentes <- IAM %>% filter(file_grupos == "ListadoDeAspirantes") %>% distinct(identificacion, .keep_all = TRUE)
if (nrow(aspirantes_existentes) == 0) {
  stop("No se encontraron registros de aspirantes ('ListadoDeAspirantes') en la base de datos IAM.")
}

# Calcular la Tasa de Selectividad (TSel) para cada programa y cada semestre
for (prog in programas) {
  for (p in periodos) {
    # Filtrar aspirantes del periodo actual para el programa actual
    aspirantes_p <- IAM %>%
      filter(periodo == p, file_grupos == "ListadoDeAspirantes", programa == prog) %>%
      distinct(identificacion, .keep_all = TRUE)
    
    # Verificar si existen aspirantes para el periodo y programa
    if (nrow(aspirantes_p) == 0) {
      cat("No se encontraron aspirantes para el programa:", prog, "y periodo:", p, "\n")
      next
    }
    
    # Filtrar admitidos del periodo actual para el programa actual
    admitidos_p <- IAM %>%
      filter(periodo == p, file_grupos == "ListadoDeAdmitidos", programa == prog) %>%
      distinct(identificacion, .keep_all = TRUE)
    
    # Verificar si existen admitidos para el periodo y programa
    if (nrow(admitidos_p) == 0) {
      cat("No se encontraron admitidos para el programa:", prog, "y periodo:", p, "\n")
      next
    }
    
    # Calcular la Tasa de Selectividad (TSel)
    num_aspirantes <- nrow(aspirantes_p)
    num_admitidos <- nrow(admitidos_p)
    
    if (num_aspirantes > 0) {
      TSel <- (num_admitidos / num_aspirantes) * 100
    } else {
      TSel <- NA
    }
    
    # Imprimir valores para validación
    cat("Programa:", prog, "\n")
    cat("Periodo:", p, "\n")
    cat("Número de aspirantes:", num_aspirantes, "\n")
    cat("Número de admitidos:", num_admitidos, "\n")
    cat("TSel calculada:", round(TSel, 2), "%\n\n")
    
    # Agregar el resultado al dataframe
    TSel_results_programa <- rbind(TSel_results_programa, data.frame(programa = prog, semestre = p, TSel = round(TSel, 2)))
  }
}

# Mostrar resultados
print(TSel_results_programa)

# Guardar resultados
guardar_datos(TSel_results_programa, path_base, "Tasa_Selectividad_Programas")


# TASA DE PERMANENCIA ANUAL POR PROGRAMA -------

# Definir los periodos disponibles a partir del año 20161 hasta 20232 (deben ser consecutivos)
periodos <- unique(IAM$periodo)
periodos <- periodos[periodos >= "20161" & periodos <= "20232"]

# Obtener la lista de programas
programas <- unique(IAM$programa)

# Crear un dataframe vacío para almacenar los resultados de la Tasa de Permanencia Anual (TPA)
TPA_results_programa <- data.frame(programa = character(), año = character(), TPA = numeric(), stringsAsFactors = FALSE)

# Calcular la Tasa de Permanencia Anual (TPA) para cada programa y cada año
for (prog in programas) {
  for (p in periodos) {
    # Extraer el año del periodo actual
    anio_actual <- as.numeric(substr(p, 1, 4))
    semestre_actual <- as.numeric(substr(p, 5, 5))
    
    # Solo considerar periodos del primer semestre (para anual)
    if (semestre_actual != 1) {
      next
    }
    
    # Definir el próximo año para calcular la TPA
    anio_siguiente <- anio_actual + 1
    periodo_siguiente <- paste0(anio_siguiente, "1")
    
    # Filtrar estudiantes matriculados del periodo actual que deberían continuar
    estudiantes_actual <- IAM %>%
      filter(periodo == p, file_grupos == "matriculados", programa == prog) %>%
      distinct(identificacion, .keep_all = TRUE)
    
    # Verificar si existen estudiantes en el periodo actual
    if (nrow(estudiantes_actual) == 0) {
      cat("No se encontraron estudiantes para el programa:", prog, "y periodo:", p, "\n")
      next
    }
    
    # Filtrar estudiantes matriculados en el siguiente periodo
    estudiantes_siguiente <- IAM %>%
      filter(periodo == periodo_siguiente, file_grupos == "matriculados", programa == prog) %>%
      distinct(identificacion, .keep_all = TRUE)
    
    # Filtrar los estudiantes que están en ambos periodos (aquellos que continúan)
    estudiantes_que_continuan <- estudiantes_actual %>%
      filter(identificacion %in% estudiantes_siguiente$identificacion)
    
    # Calcular la Tasa de Permanencia Anual (TPA)
    num_estudiantes_actual <- nrow(estudiantes_actual)
    num_estudiantes_que_continuan <- nrow(estudiantes_que_continuan)
    
    if (num_estudiantes_actual > 0) {
      TPA <- (num_estudiantes_que_continuan / num_estudiantes_actual) * 100
    } else {
      TPA <- NA
    }
    
    # Imprimir valores para validación
    cat("Programa:", prog, "\n")
    cat("Año:", anio_actual, "\n")
    cat("Número de estudiantes en el año actual:", num_estudiantes_actual, "\n")
    cat("Número de estudiantes que continúan al año siguiente:", num_estudiantes_que_continuan, "\n")
    cat("TPA calculada:", round(TPA, 2), "%\n\n")
    
    # Agregar el resultado al dataframe
    TPA_results_programa <- rbind(TPA_results_programa, data.frame(programa = prog, año = anio_actual, TPA = round(TPA, 2)))
  }
}

# Mostrar resultados
print(TPA_results_programa)

# Guardar resultados
guardar_datos(TPA_results_programa, path_base, "Tasa_Permanencia_Anual_Programas")


# TASA DE RE-ABSORCIÓN POR PROGRAMA -------

# Definir los periodos disponibles a partir del año 20161 hasta 20232 (deben ser consecutivos)
periodos <- unique(IAM$periodo)
periodos <- periodos[periodos >= "20161" & periodos <= "20232"]

# Obtener la lista de programas
programas <- unique(IAM$programa)

# Crear un dataframe vacío para almacenar los resultados de la Tasa de Re-absorción (TRA)
TRA_results_programa <- data.frame(programa = character(), periodo = character(), TRA = numeric(), stringsAsFactors = FALSE)

# Calcular la Tasa de Re-absorción (TRA) para cada programa y cada periodo
for (prog in programas) {
  for (p in periodos) {
    # Filtrar estudiantes matriculados del periodo actual para el programa actual
    estudiantes_actual <- IAM %>%
      filter(periodo == p, file_grupos == "matriculados", programa == prog) %>%
      distinct(identificacion, .keep_all = TRUE)
    
    # Verificar si existen estudiantes en el periodo actual
    if (nrow(estudiantes_actual) == 0) {
      cat("No se encontraron estudiantes para el programa:", prog, "y periodo:", p, "\n")
      next
    }
    
    # Definir el siguiente periodo para evaluar si continuaron (si semestre == 1, el siguiente es semestre 2 del mismo año)
    anio_actual <- as.numeric(substr(p, 1, 4))
    semestre_actual <- as.numeric(substr(p, 5, 5))
    
    if (semestre_actual == 1) {
      periodo_siguiente <- paste0(anio_actual, "2")
    } else {
      periodo_siguiente <- paste0(anio_actual + 1, "1")
    }
    
    # Filtrar estudiantes que continuaron en el siguiente periodo
    estudiantes_siguiente <- IAM %>%
      filter(periodo == periodo_siguiente, file_grupos == "matriculados", programa == prog) %>%
      distinct(identificacion, .keep_all = TRUE)
    
    # Identificar estudiantes que abandonaron (no están en el siguiente periodo)
    estudiantes_abandonaron <- estudiantes_actual %>%
      filter(!identificacion %in% estudiantes_siguiente$identificacion)
    
    # Verificar si hay estudiantes que abandonaron
    if (nrow(estudiantes_abandonaron) == 0) {
      cat("No se encontraron estudiantes que abandonaron para el programa:", prog, "y periodo:", p, "\n")
      next
    }
    
    # Verificar si hay estudiantes que se reintegraron en periodos posteriores
    estudiantes_reintegrados <- IAM %>%
      filter(identificacion %in% estudiantes_abandonaron$identificacion, periodo > periodo_siguiente, file_grupos == "matriculados", programa == prog) %>%
      distinct(identificacion, .keep_all = TRUE)
    
    # Calcular la Tasa de Re-absorción (TRA)
    num_abandonaron <- nrow(estudiantes_abandonaron)
    num_reintegrados <- nrow(estudiantes_reintegrados)
    
    if (num_abandonaron > 0) {
      TRA <- (num_reintegrados / num_abandonaron) * 100
    } else {
      TRA <- NA
    }
    
    # Imprimir valores para validación
    cat("Programa:", prog, "\n")
    cat("Periodo:", p, "\n")
    cat("Número de estudiantes que abandonaron:", num_abandonaron, "\n")
    cat("Número de estudiantes que se reintegraron:", num_reintegrados, "\n")
    cat("TRA calculada:", round(TRA, 2), "%\n\n")
    
    # Agregar el resultado al dataframe
    TRA_results_programa <- rbind(TRA_results_programa, data.frame(programa = prog, periodo = p, TRA = round(TRA, 2)))
  }
}

# Mostrar resultados
print(TRA_results_programa)

# Guardar resultados
guardar_datos(TRA_results_programa, path_base, "Tasa_Reabsorcion_Programas")








# TASA DE RETENCIÓN INICIAL POR PROGRAMA -------

# Definir los periodos disponibles a partir del año 20161 hasta 20232 (deben ser consecutivos)
periodos <- unique(IAM$periodo)
periodos <- periodos[periodos >= "20161" & periodos <= "20232"]

# Obtener la lista de programas
programas <- unique(IAM$programa)

# Crear un dataframe vacío para almacenar los resultados de la Tasa de Retención Inicial (TRI)
TRI_results_programa <- data.frame(programa = character(), periodo_inicial = character(), TRI = numeric(), stringsAsFactors = FALSE)

# Calcular la Tasa de Retención Inicial (TRI) para cada programa y cada semestre inicial
for (prog in programas) {
  for (p in periodos) {
    # Extraer el año y semestre del periodo actual
    anio_actual <- as.numeric(substr(p, 1, 4))
    semestre_actual <- as.numeric(substr(p, 5, 5))
    
    # Solo considerar los periodos del primer semestre (semestre 1) para evaluar el primer año
    if (semestre_actual != 1) {
      next
    }
    
    # Definir el siguiente periodo para evaluar si continuaron (semestre 2 del mismo año)
    periodo_siguiente <- paste0(anio_actual, "2")
    
    # Filtrar estudiantes matriculados inicialmente en el periodo actual
    estudiantes_inicial <- IAM %>%
      filter(periodo == p, file_grupos == "matriculados nuevos", programa == prog) %>%
      distinct(identificacion, .keep_all = TRUE)
    
    # Verificar si existen estudiantes matriculados inicialmente para el periodo y programa
    if (nrow(estudiantes_inicial) == 0) {
      cat("No se encontraron estudiantes iniciales para el programa:", prog, "y periodo:", p, "\n")
      next
    }
    
    # Filtrar estudiantes matriculados en el periodo siguiente (semestre 2 del mismo año)
    estudiantes_siguiente <- IAM %>%
      filter(periodo == periodo_siguiente, file_grupos == "matriculados", programa == prog) %>%
      distinct(identificacion, .keep_all = TRUE)
    
    # Definir el primer semestre del año siguiente para evaluar la continuidad después del primer año
    periodo_anio_siguiente <- paste0(anio_actual + 1, "1")
    
    # Filtrar estudiantes matriculados en el primer semestre del año siguiente
    estudiantes_anio_siguiente <- IAM %>%
      filter(periodo == periodo_anio_siguiente, file_grupos == "matriculados", programa == prog) %>%
      distinct(identificacion, .keep_all = TRUE)
    
    # Identificar los estudiantes que continúan después del primer año
    estudiantes_que_continuan <- estudiantes_inicial %>%
      filter(identificacion %in% estudiantes_siguiente$identificacion | identificacion %in% estudiantes_anio_siguiente$identificacion)
    
    # Calcular la Tasa de Retención Inicial (TRI)
    num_estudiantes_inicial <- nrow(estudiantes_inicial)
    num_estudiantes_que_continuan <- nrow(estudiantes_que_continuan)
    
    if (num_estudiantes_inicial > 0) {
      TRI <- (num_estudiantes_que_continuan / num_estudiantes_inicial) * 100
    } else {
      TRI <- NA
    }
    
    # Imprimir valores para validación
    cat("Programa:", prog, "\n")
    cat("Periodo Inicial:", p, "\n")
    cat("Número de estudiantes iniciales:", num_estudiantes_inicial, "\n")
    cat("Número de estudiantes que continúan después del primer año:", num_estudiantes_que_continuan, "\n")
    cat("TRI calculada:", round(TRI, 2), "%\n\n")
    
    # Agregar el resultado al dataframe
    TRI_results_programa <- rbind(TRI_results_programa, data.frame(programa = prog, periodo_inicial = p, TRI = round(TRI, 2)))
  }
}

# Mostrar resultados
print(TRI_results_programa)

# Guardar resultados
guardar_datos(TRI_results_programa, path_base, "Tasa_Retencion_Inicial_Programas")


# TASA DE GRADUACIÓN POR PROGRAMA -------

# Definir la ruta del archivo de graduados
ruta_graduados <- "/Users/cristianespinal/Library/CloudStorage/GoogleDrive-cristian.espinal@pascualbravo.edu.co/Unidades compartidas/UVIC - Unidad de Vigilancia Tecnologica e Inteligencia Competitiva/6. Lago de datos/RAW DATA/Graduados_IUPB/graduados_raw.xlsx"

# Leer la base de datos de graduados
graduados <- read_excel(ruta_graduados)

# Definir los periodos disponibles a partir del año 20161 hasta 20232
periodos <- unique(IAM$periodo)
periodos <- periodos[periodos >= "20161" & periodos <= "20232"]

# Obtener la lista de programas
programas <- unique(IAM$programa)

# Crear un dataframe vacío para almacenar los resultados de la Tasa de Graduación (TG)
TG_results_programa <- data.frame(programa = character(), periodo_inicial = character(), TG = numeric(), stringsAsFactors = FALSE)

# Calcular la Tasa de Graduación (TG) para cada programa y cada periodo inicial
for (prog in programas) {
  for (p in periodos) {
    # Extraer el año y semestre del periodo actual
    anio_actual <- as.numeric(substr(p, 1, 4))
    semestre_actual <- as.numeric(substr(p, 5, 5))
    
    # Solo considerar los periodos del primer semestre (semestre 1) para evaluar la graduación a largo plazo
    if (semestre_actual != 1) {
      next
    }
    
    # Filtrar estudiantes matriculados inicialmente en el periodo actual para el programa actual
    estudiantes_inicial <- IAM %>%
      filter(periodo == p, file_grupos == "matriculados nuevos", programa == prog) %>%
      distinct(identificacion, .keep_all = TRUE)
    
    # Verificar si existen estudiantes matriculados inicialmente para el periodo y programa
    if (nrow(estudiantes_inicial) == 0) {
      cat("No se encontraron estudiantes iniciales para el programa:", prog, "y periodo:", p, "\n")
      next
    }
    
    # Filtrar estudiantes que se graduaron usando la base de datos de graduados
    estudiantes_graduados <- graduados %>%
      filter(DOCUMENTO %in% estudiantes_inicial$identificacion) %>%
      distinct(DOCUMENTO, .keep_all = TRUE)
    
    # Calcular la Tasa de Graduación (TG)
    num_estudiantes_inicial <- nrow(estudiantes_inicial)
    num_estudiantes_graduados <- nrow(estudiantes_graduados)
    
    if (num_estudiantes_inicial > 0) {
      TG <- (num_estudiantes_graduados / num_estudiantes_inicial) * 100
    } else {
      TG <- NA
    }
    
    # Imprimir valores para validación
    cat("Programa:", prog, "\n")
    cat("Periodo Inicial:", p, "\n")
    cat("Número de estudiantes iniciales:", num_estudiantes_inicial, "\n")
    cat("Número de estudiantes que se graduaron:", num_estudiantes_graduados, "\n")
    cat("TG calculada:", round(TG, 2), "%\n\n")
    
    # Agregar el resultado al dataframe
    TG_results_programa <- rbind(TG_results_programa, data.frame(programa = prog, periodo_inicial = p, TG = round(TG, 2)))
  }
}

# Mostrar resultados
print(TG_results_programa)

# Guardar resultados
guardar_datos(TG_results_programa, path_base, "Tasa_Graduacion_Programas")







# TASA DE GRADUACIÓN ACUMULADA POR PROGRAMA -------

# Definir los periodos disponibles a partir del año 20161 hasta 20232
periodos <- unique(IAM$periodo)
periodos <- periodos[periodos >= "20161" & periodos <= "20232"]

# Obtener la lista de programas
programas <- unique(IAM$programa)

# Crear un dataframe vacío para almacenar los resultados de la Tasa de Graduación Acumulada (TGA)
TGA_results_programa <- data.frame(programa = character(), TGA = numeric(), stringsAsFactors = FALSE)

# Calcular la Tasa de Graduación Acumulada (TGA) para cada programa
for (prog in programas) {
  
  # Filtrar todos los estudiantes que se matricularon inicialmente en cualquier periodo para el programa actual
  estudiantes_inicial <- IAM %>%
    filter(file_grupos == "matriculados nuevos", programa == prog) %>%
    distinct(identificacion, .keep_all = TRUE)
  
  # Verificar si existen estudiantes matriculados inicialmente para el programa
  if (nrow(estudiantes_inicial) == 0) {
    cat("No se encontraron estudiantes iniciales para el programa:", prog, "\n")
    next
  }
  
  # Filtrar estudiantes que se graduaron usando la base de datos de graduados
  estudiantes_graduados <- graduados %>%
    filter(DOCUMENTO %in% estudiantes_inicial$identificacion) %>%
    distinct(DOCUMENTO, .keep_all = TRUE)
  
  # Calcular la Tasa de Graduación Acumulada (TGA)
  num_estudiantes_inicial <- nrow(estudiantes_inicial)
  num_estudiantes_graduados <- nrow(estudiantes_graduados)
  
  if (num_estudiantes_inicial > 0) {
    TGA <- (num_estudiantes_graduados / num_estudiantes_inicial) * 100
  } else {
    TGA <- NA
  }
  
  # Imprimir valores para validación
  cat("Programa:", prog, "\n")
  cat("Número total de estudiantes iniciales:", num_estudiantes_inicial, "\n")
  cat("Número total de estudiantes que se graduaron:", num_estudiantes_graduados, "\n")
  cat("TGA calculada:", round(TGA, 2), "%\n\n")
  
  # Agregar el resultado al dataframe
  TGA_results_programa <- rbind(TGA_results_programa, data.frame(programa = prog, TGA = round(TGA, 2)))
}

# Mostrar resultados
print(TGA_results_programa)

# Guardar resultados
guardar_datos(TGA_results_programa, path_base, "Tasa_Graduacion_Acumulada_Programas")









# TASA DE PERMANENCIA POR PROGRAMA ----------

# Definir los periodos disponibles a partir del año 20161 hasta 20232 (deben ser consecutivos)
periodos <- unique(IAM$periodo)
periodos <- periodos[periodos >= "20161" & periodos <= "20232"]

# Obtener la lista de programas
programas <- unique(IAM$programa)

# Crear un dataframe vacío para almacenar los resultados de la Tasa de Permanencia (TP)
TP_results_programa <- data.frame(programa = character(), periodo = character(), TP = numeric(), stringsAsFactors = FALSE)

# Calcular la Tasa de Permanencia (TP) para cada programa y cada periodo
for (prog in programas) {
  for (p in periodos) {
    # Extraer el año y semestre del periodo actual
    anio_actual <- as.numeric(substr(p, 1, 4))
    semestre_actual <- as.numeric(substr(p, 5, 5))
    
    # Definir el siguiente periodo (semestre) para evaluar la permanencia
    if (semestre_actual == 1) {
      periodo_siguiente <- paste0(anio_actual, "2")
    } else {
      periodo_siguiente <- paste0(anio_actual + 1, "1")
    }
    
    # Filtrar estudiantes matriculados en el periodo actual para el programa actual
    estudiantes_actual <- IAM %>%
      filter(periodo == p, file_grupos == "matriculados", programa == prog) %>%
      distinct(identificacion, .keep_all = TRUE)
    
    # Verificar si existen estudiantes matriculados en el periodo actual
    if (nrow(estudiantes_actual) == 0) {
      cat("No se encontraron estudiantes para el programa:", prog, "y periodo:", p, "\n")
      next
    }
    
    # Filtrar estudiantes matriculados en el siguiente periodo
    estudiantes_siguiente <- IAM %>%
      filter(periodo == periodo_siguiente, file_grupos == "matriculados", programa == prog) %>%
      distinct(identificacion, .keep_all = TRUE)
    
    # Filtrar los estudiantes que están en ambos periodos (aquellos que continúan)
    estudiantes_que_continuan <- estudiantes_actual %>%
      filter(identificacion %in% estudiantes_siguiente$identificacion)
    
    # Calcular la Tasa de Permanencia (TP)
    num_estudiantes_actual <- nrow(estudiantes_actual)
    num_estudiantes_que_continuan <- nrow(estudiantes_que_continuan)
    
    if (num_estudiantes_actual > 0) {
      TP <- (num_estudiantes_que_continuan / num_estudiantes_actual) * 100
    } else {
      TP <- NA
    }
    
    # Imprimir valores para validación
    cat("Programa:", prog, "\n")
    cat("Periodo:", p, "\n")
    cat("Número de estudiantes en el periodo actual:", num_estudiantes_actual, "\n")
    cat("Número de estudiantes que continúan al siguiente periodo:", num_estudiantes_que_continuan, "\n")
    cat("TP calculada:", round(TP, 2), "%\n\n")
    
    # Agregar el resultado al dataframe
    TP_results_programa <- rbind(TP_results_programa, data.frame(programa = prog, periodo = p, TP = round(TP, 2)))
  }
}

# Mostrar resultados
print(TP_results_programa)

# Guardar resultados
guardar_datos(TP_results_programa, path_base, "Tasa_Permanencia_Programas")


# PROMEDIO DE DURACIÓN DE ESTUDIOS POR PROGRAMA -----------

# Convertir las columnas 'identificacion' de IAM y 'DOCUMENTO' de graduados a tipo character
IAM$identificacion <- as.character(IAM$identificacion)
graduados$DOCUMENTO <- as.character(graduados$DOCUMENTO)

# Definir los periodos disponibles a partir del año 20161 hasta 20232
periodos <- unique(IAM$periodo)
periodos <- periodos[periodos >= "20161" & periodos <= "20232"]

# Obtener la lista de programas
programas <- unique(IAM$programa)

# Crear un dataframe vacío para almacenar los resultados del Promedio de Duración (PD)
PD_results_programa <- data.frame(programa = character(), PD = numeric(), stringsAsFactors = FALSE)

# Calcular el Promedio de Duración (PD) para cada programa
for (prog in programas) {
  
  # Filtrar todos los estudiantes que se matricularon inicialmente en cualquier periodo para el programa actual
  estudiantes_inicial <- IAM %>%
    filter(file_grupos == "matriculados nuevos", programa == prog) %>%
    distinct(identificacion, .keep_all = TRUE)
  
  # Verificar si existen estudiantes matriculados inicialmente para el programa
  if (nrow(estudiantes_inicial) == 0) {
    cat("No se encontraron estudiantes iniciales para el programa:", prog, "\n")
    next
  }
  
  # Filtrar estudiantes que se graduaron usando la base de datos de graduados
  estudiantes_graduados <- graduados %>%
    filter(DOCUMENTO %in% estudiantes_inicial$identificacion) %>%
    distinct(DOCUMENTO, .keep_all = TRUE)
  
  # Verificar si existen estudiantes graduados
  if (nrow(estudiantes_graduados) == 0) {
    cat("No se encontraron estudiantes graduados para el programa:", prog, "\n")
    next
  }
  
  # Unir los datos de estudiantes matriculados inicialmente con la información de graduados
  estudiantes_duracion <- estudiantes_inicial %>%
    inner_join(estudiantes_graduados, by = c("identificacion" = "DOCUMENTO"))
  
  # Calcular la duración de los estudios para cada estudiante
  # Convertir las fechas de matrícula y graduación en formato Date para calcular la diferencia
  estudiantes_duracion <- estudiantes_duracion %>%
    mutate(fecha_matricula = as.Date(fecha_de_matricula, format = "%Y-%m-%d"),
           fecha_graduacion = as.Date(FECHA, format = "%Y-%m-%d"),
           duracion_en_meses = as.numeric(difftime(fecha_graduacion, fecha_matricula, units = "weeks")) / 4.345)  # Convertir semanas a meses
  
  # Filtrar duraciones negativas
  estudiantes_duracion <- estudiantes_duracion %>%
    filter(duracion_en_meses >= 0)
  
  # Verificar si existen duraciones válidas para el cálculo
  if (nrow(estudiantes_duracion) == 0) {
    cat("No se encontraron duraciones válidas para el programa:", prog, "\n")
    next
  }
  
  # Calcular el promedio de duración en meses para el programa actual
  promedio_duracion <- mean(estudiantes_duracion$duracion_en_meses, na.rm = TRUE)
  
  # Imprimir valores para validación
  cat("Programa:", prog, "\n")
  cat("Número de estudiantes graduados:", nrow(estudiantes_graduados), "\n")
  cat("Promedio de duración de estudios (meses):", round(promedio_duracion, 2), "\n\n")
  
  # Agregar el resultado al dataframe
  PD_results_programa <- rbind(PD_results_programa, data.frame(programa = prog, PD = round(promedio_duracion, 2)))
}

# Mostrar resultados
print(PD_results_programa)

# Guardar resultados
guardar_datos(PD_results_programa, path_base, "Promedio_Duracion_Estudios_Programas")


# PROMEDIO DE DESEMPEÑO ACADEMICO POR PROGRAMA --------------

# Filtrar los registros para evitar filas con NA en 'Periodo' o 'PlanDeEstudio'
notas_general_raw <- notas_general_raw %>%
  filter(!is.na(Periodo) & !is.na(PlanDeEstudio))

# Obtener la lista de periodos y programas únicos después de eliminar valores NA
periodos <- unique(notas_general_raw$Periodo)
programas <- unique(notas_general_raw$PlanDeEstudio)

# Crear un dataframe vacío para almacenar los resultados del Promedio de Desempeño Académico (PDA)
PDA_results_programa <- data.frame(programa = character(), periodo = character(), PDA = numeric(), stringsAsFactors = FALSE)

# Calcular el Promedio de Desempeño Académico (PDA) para cada programa y periodo
for (prog in programas) {
  for (per in periodos) {
    
    # Filtrar los registros para el programa y periodo actuales
    notas_programa_periodo <- notas_general_raw %>%
      filter(PlanDeEstudio == prog, Periodo == per)
    
    # Verificar si existen registros para el programa y periodo actuales
    if (nrow(notas_programa_periodo) == 0) {
      cat("No se encontraron registros para el programa:", prog, "y periodo:", per, "\n")
      next
    }
    
    # Calcular el promedio de desempeño académico (NotaParcialAcumulada) para el programa y periodo actuales
    promedio_desempeño <- mean(notas_programa_periodo$NotaParcialAcumulada, na.rm = TRUE)
    
    # Imprimir valores para validación
    cat("Programa:", prog, "\n")
    cat("Periodo:", per, "\n")
    cat("Número de estudiantes:", nrow(notas_programa_periodo), "\n")
    cat("Promedio de desempeño académico:", round(promedio_desempeño, 2), "\n\n")
    
    # Agregar el resultado al dataframe
    PDA_results_programa <- rbind(PDA_results_programa, data.frame(programa = prog, periodo = per, PDA = round(promedio_desempeño, 2)))
  }
}

# Mostrar resultados
print(PDA_results_programa)

# Guardar resultados
guardar_datos(PDA_results_programa, path_base, "Promedio_Desempeño_Academico_Programas")






# TASA DE EXCESO DE DEMANDA POR PROGRAMA -------

# Definir la ruta del archivo
ruta_cupos <- "/Users/cristianespinal/Library/CloudStorage/GoogleDrive-cristian.espinal@pascualbravo.edu.co/Unidades compartidas/UVIC - Unidad de Vigilancia Tecnologica e Inteligencia Competitiva/6. Lago de datos/RAW DATA/Cupos/cupos_raw.xlsx"

# Leer el archivo Excel
cupos <- read_excel(ruta_cupos)

# Mostrar las primeras filas para verificar la carga correcta
head(cupos)

# Convertir la columna 'PeriodoAcademico' en la base de datos de cupos a tipo character
cupos$PeriodoAcademico <- as.character(cupos$PeriodoAcademico)

# Definir los periodos disponibles a partir del año 20161 hasta 20232
periodos <- unique(IAM$periodo)
periodos <- periodos[periodos >= "20161" & periodos <= "20232"]

# Obtener la lista de programas
programas <- unique(IAM$programa)

# Crear un dataframe vacío para almacenar los resultados de la Tasa de Exceso de Demanda (TED)
TED_results_programa <- data.frame(programa = character(), periodo = character(), TED = numeric(), stringsAsFactors = FALSE)

# Calcular la Tasa de Exceso de Demanda (TED) para cada programa y cada periodo
for (prog in programas) {
  for (p in periodos) {
    
    # Filtrar aspirantes del periodo actual para el programa actual
    aspirantes_p <- IAM %>%
      filter(periodo == p, file_grupos == "ListadoDeAspirantes", programa == prog) %>%
      distinct(identificacion, .keep_all = TRUE)
    
    # Verificar si existen aspirantes para el periodo y programa
    if (nrow(aspirantes_p) == 0) {
      cat("No se encontraron aspirantes para el programa:", prog, "y periodo:", p, "\n")
      next
    }
    
    # Filtrar cupos disponibles para el periodo y programa
    cupos_p <- cupos %>%
      filter(PeriodoAcademico == p, ProgramaAcademico == prog)
    
    # Verificar si existen cupos para el periodo y programa
    if (nrow(cupos_p) == 0) {
      cat("No se encontraron cupos para el programa:", prog, "y periodo:", p, "\n")
      next
    }
    
    # Obtener el número de aspirantes y cupos
    num_aspirantes <- nrow(aspirantes_p)
    num_cupos <- cupos_p$Cantidad[1]  # Se asume un único registro por programa y periodo en la base de cupos
    
    # Calcular la Tasa de Exceso de Demanda (TED)
    TED <- num_aspirantes / num_cupos
    
    # Imprimir valores para validación
    cat("Programa:", prog, "\n")
    cat("Periodo:", p, "\n")
    cat("Número de aspirantes:", num_aspirantes, "\n")
    cat("Número de cupos:", num_cupos, "\n")
    cat("TED calculada:", round(TED, 2), "\n\n")
    
    # Agregar el resultado al dataframe
    TED_results_programa <- rbind(TED_results_programa, data.frame(programa = prog, periodo = p, TED = round(TED, 2)))
  }
}

# Mostrar resultados
print(TED_results_programa)

# Guardar resultados
guardar_datos(TED_results_programa, path_base, "Tasa_Exceso_Demanda_Programas")


# TASA DE DEMANDA EFECTIVA POR PROGRAMA -------

# Convertir la columna 'PeriodoAcademico' en la base de datos de cupos a tipo character
cupos$PeriodoAcademico <- as.character(cupos$PeriodoAcademico)

# Definir los periodos disponibles a partir del año 20161 hasta 20232
periodos <- unique(IAM$periodo)
periodos <- periodos[periodos >= "20161" & periodos <= "20232"]

# Obtener la lista de programas
programas <- unique(IAM$programa)

# Crear un dataframe vacío para almacenar los resultados de la Tasa de Demanda Efectiva (TDE)
TDE_results_programa <- data.frame(programa = character(), periodo = character(), TDE = numeric(), stringsAsFactors = FALSE)

# Calcular la Tasa de Demanda Efectiva (TDE) para cada programa y cada periodo
for (prog in programas) {
  for (p in periodos) {
    
    # Filtrar matriculados nuevos del periodo actual para el programa actual
    matriculados_nuevos_p <- IAM %>%
      filter(periodo == p, file_grupos == "matriculados nuevos", programa == prog) %>%
      distinct(identificacion, .keep_all = TRUE)
    
    # Verificar si existen matriculados nuevos para el periodo y programa
    if (nrow(matriculados_nuevos_p) == 0) {
      cat("No se encontraron matriculados nuevos para el programa:", prog, "y periodo:", p, "\n")
      next
    }
    
    # Filtrar cupos disponibles para el periodo y programa
    cupos_p <- cupos %>%
      filter(PeriodoAcademico == p, ProgramaAcademico == prog)
    
    # Verificar si existen cupos para el periodo y programa
    if (nrow(cupos_p) == 0) {
      cat("No se encontraron cupos para el programa:", prog, "y periodo:", p, "\n")
      next
    }
    
    # Obtener el número de matriculados nuevos y cupos
    num_matriculados_nuevos <- nrow(matriculados_nuevos_p)
    num_cupos <- cupos_p$Cantidad[1]  # Se asume un único registro por programa y periodo en la base de cupos
    
    # Calcular la Tasa de Demanda Efectiva (TDE)
    TDE <- num_matriculados_nuevos / num_cupos
    
    # Imprimir valores para validación
    cat("Programa:", prog, "\n")
    cat("Periodo:", p, "\n")
    cat("Número de matriculados nuevos:", num_matriculados_nuevos, "\n")
    cat("Número de cupos:", num_cupos, "\n")
    cat("TDE calculada:", round(TDE, 2), "\n\n")
    
    # Agregar el resultado al dataframe
    TDE_results_programa <- rbind(TDE_results_programa, data.frame(programa = prog, periodo = p, TDE = round(TDE, 2)))
  }
}

# Mostrar resultados
print(TDE_results_programa)

# Guardar resultados
guardar_datos(TDE_results_programa, path_base, "Tasa_Demanda_Efectiva_Programas")




