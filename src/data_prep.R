# ==============================================================================
# SCRIPT: data_prep.R
# OBJETIVO: Funciones para la limpieza y preparación de datos transaccionales
# ==============================================================================

library(data.table)
library(jsonlite)

#' Carga y extrae las métricas de validación T0 desde el JSON
#'
#' @param ruta_json Cadena de texto con la ruta al archivo JSON de métricas.
#' @return Una lista de R (proveniente de fromJSON) lista para procesar.
cargar_datos_t0 <- function(ruta_json) {
  if (!file.exists(ruta_json)) {
    stop("Error: No se encontró el archivo JSON en la ruta especificada.")
  }
  jsonlite::fromJSON(ruta_json)
}

#' Reconstruye los conteos absolutos (TP, FP, FN) a partir de tasas relativas
#'
#' @description 
#' Transforma el dataset temporal calculando los Verdaderos Positivos, 
#' Falsos Negativos y Falsos Positivos a partir del Recall, Precision y 
#' el total de casos de fraude.
#'
#' @param df_temporal Un data.table que contiene las columnas: recall, precision y n_fraude.
#' @return El mismo data.table con tres columnas adicionales: tp, fn, fp (enteros).
preparar_conteos_temporales <- function(df_temporal) {
  # Asegurar que es un data.table
  dt <- as.data.table(df_temporal)
  
  # Calcular TP, FN y FP redondeando al número entero más cercano
  dt[, tp := round(recall * n_fraude)]
  dt[, fn := n_fraude - tp]
  
  # Evitar división por cero en FP si la precisión es 0
  dt[, fp := ifelse(precision > 0, round((tp / precision) - tp), 0)]
  
  return(dt)
}

#' Guarda una tabla de datos (data.table) en la carpeta de datos procesados
#'
#' @description 
#' Exporta un objeto data.table a un archivo CSV utilizando la función eficiente 
#' fwrite de data.table, asegurando que el directorio de destino exista.
#'
#' @param dt data.table. La tabla de datos que se desea exportar.
#' @param ruta_salida Cadena de texto con la ruta completa y nombre del archivo CSV (ej. "data/processed/tabla_t0.csv").
#' @return Invisible NULL. Genera el archivo en el disco.
guardar_tabla_procesada <- function(dt, ruta_salida) {
  # Asegurar que el directorio base exista (crearlo si no existe en la máquina)
  directorio <- dirname(ruta_salida)
  if (!dir.exists(directorio)) {
    dir.create(directorio, recursive = TRUE)
  }
  
  # Guardar con fwrite de data.table (rápido, eficiente y limpio)
  data.table::fwrite(dt, file = ruta_salida, row.names = FALSE)
  
  invisible(NULL)
}