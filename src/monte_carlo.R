# ==============================================================================
# SCRIPT: monte_carlo.R
# OBJETIVO: Funciones de simulación bayesiana (Muestreo Monte Carlo)
# ==============================================================================

library(data.table)

#' Simula la distribución posterior del F1-Score para un modelo en T0
#'
#' @description 
#' Utiliza las distribuciones conjugadas Beta-Binomial para generar muestras 
#' de la posterior de Precisión y Sensibilidad, y luego calcula el F1-Score.
#'
#' @param nombre_modelo Cadena de texto con el nombre del modelo (ej. "M3_gbm_completo").
#' @param datos_json Lista cargada desde el archivo JSON con las métricas T0.
#' @param S Número entero. Cantidad de simulaciones Monte Carlo (default: 5000).
#' @return Un data.table con 3 columnas: Modelo, Conteos (str), y F1_Muestra (numérico).
simular_posterior_f1_t0 <- function(nombre_modelo, datos_json, S = 5000) {
  
  # Extracción de conteos
  TP <- datos_json[[nombre_modelo]]$T0_validacion$TP
  FP <- datos_json[[nombre_modelo]]$T0_validacion$FP
  FN <- datos_json[[nombre_modelo]]$T0_validacion$FN
  
  # Muestreo Monte Carlo
  muestras_precision <- rbeta(S, shape1 = 1 + TP, shape2 = 1 + FP)
  muestras_recall    <- rbeta(S, shape1 = 1 + TP, shape2 = 1 + FN)
  muestras_f1        <- (2 * muestras_precision * muestras_recall) / (muestras_precision + muestras_recall)
  
  # Empaquetar resultados
  data.table(
    Modelo  = nombre_modelo,
    Conteos = paste0("TP:", TP, " | FP:", FP, " | FN:", FN),
    F1_Muestra = muestras_f1
  )
}

#' Calcula el resumen posterior del F1-Score para una celda temporal (Mes/Modelo)
#'
#' @description 
#' Diseñada para ser vectorizada dentro de un data.table. Toma los conteos de una 
#' fila específica, realiza la simulación Monte Carlo y devuelve métricas resumen.
#'
#' @param tp Número entero. Verdaderos Positivos.
#' @param fp Número entero. Falsos Positivos.
#' @param fn Número entero. Falsos Negativos.
#' @param S Número entero. Cantidad de simulaciones (default: 5000).
#' @return Una lista con la mediana del F1 y los cuantiles 0.025 y 0.975 (HPDI 95%).
calcular_posterior_fila <- function(tp, fp, fn, S = 5000) {
  # Control de NAs
  if(is.na(tp) | is.na(fp) | is.na(fn)) {
    return(list(Mediana_F1 = NA_real_, q025 = NA_real_, q975 = NA_real_))
  }
  
  # Muestreo
  muestras_precision <- rbeta(S, shape1 = 1 + tp, shape2 = 1 + fp)
  muestras_recall    <- rbeta(S, shape1 = 1 + tp, shape2 = 1 + fn)
  muestras_f1        <- (2 * muestras_precision * muestras_recall) / (muestras_precision + muestras_recall)
  
  # Devolver lista nombrada (ideal para data.table)
  list(
    Mediana_F1 = median(muestras_f1),
    q025       = quantile(muestras_f1, 0.025),
    q975       = quantile(muestras_f1, 0.975)
  )
}