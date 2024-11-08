
# GEOREFERENCIACIÓN DE MATRICULADOS -- PASCUAL BRAVO

if (!require(ggmap)) install.packages("ggmap")
if (!require(dplyr)) install.packages("dplyr")
if (!require(readr)) install.packages("readr")
if (!require(tidyr)) install.packages("tidyr")
library(ggmap)     # Para geocodificación y mapas
library(dplyr)     # Para la manipulación de datos
library(readr)     # Para la lectura y escritura de archivos CSV
library(tidyr)     # Para operaciones de unnest

# Registrar la API Key de Google Maps
register_google(key = "XXXXXX")  # Se excluye la API Key del código que se envía por seguridad

# Función para georreferenciar una dirección
georeferenciar_direccion <- function(direccion, pais) {
  # Combina dirección y país para aumentar la precisión
  direccion_completa <- paste(direccion, pais, sep = ", ")
  
  # Llama a la API de Google Maps para obtener las coordenadas
  resultado <- tryCatch({
    geocode(direccion_completa, output = "latlon", source = "google")
  }, error = function(e) {
    # En caso de error, devolver NA en latitud y longitud
    return(data.frame(lon = NA, lat = NA))
  })
  
  # Devuelve el resultado con latitud y longitud
  return(resultado)
}

# Leer el archivo CSV con direcciones y países
direcciones <- read_csv("direcciones.csv")

# Verificar las primeras filas para asegurarse de que los datos son correctos
head(direcciones)

# Aplicar la georreferenciación a todas las filas del conjunto de datos
direcciones_georreferenciadas <- direcciones %>%
  rowwise() %>%
  mutate(coordenadas = list(georeferenciar_direccion(direccion, pais))) %>%
  unnest_wider(coordenadas)

# Ver los resultados para verificar que se obtuvieron las coordenadas
head(direcciones_georreferenciadas)

# Exportar las direcciones georreferenciadas a un archivo CSV
write_csv(direcciones_georreferenciadas, "direcciones_georreferenciadas.csv")

# Obtener un mapa base centrado en la región de interés
mapa <- get_map(location = "Colombia", zoom = 4)
