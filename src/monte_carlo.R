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

#' Simula la posterior del F1-Score agregada para una ventana temporal
#'
#' @description 
#' Toma un data.table temporal, filtra una ventana de meses específica, 
#' agrega (suma) los conteos absolutos de TP, FP y FN por modelo, y realiza 
#' la simulación Monte Carlo Beta-Binomial.
#'
#' @param dt_listo data.table. Contiene las columnas modelo, mes, tp, fp, fn.
#' @param meses_ventana Vector numérico. Meses que componen la ventana (ej: 16:18).
#' @param S Número entero. Cantidad de simulaciones (default: 5000).
#' @return Un data.table en formato ancho (wide), donde cada columna es un modelo 
#'         y cada fila es una muestra de Monte Carlo.
simular_posterior_ventana <- function(dt_listo, meses_ventana, S = 5000) {
  
  # 1. Filtrar y agregar los conteos absolutos sobre los tres meses
  dt_agregado <- dt_listo[mes %in% meses_ventana, .(
    tp_tot = sum(tp),
    fp_tot = sum(fp),
    fn_tot = sum(fn)
  ), by = .(modelo)]
  
  # 2. Simular muestras para cada modelo y guardarlas en una lista
  lista_muestras <- list()
  
  for (mod in dt_agregado$modelo) {
    conteos <- dt_agregado[modelo == mod]
    
    muestras_precision <- rbeta(S, shape1 = 1 + conteos$tp_tot, shape2 = 1 + conteos$fp_tot)
    muestras_recall    <- rbeta(S, shape1 = 1 + conteos$tp_tot, shape2 = 1 + conteos$fn_tot)
    muestras_f1        <- (2 * muestras_precision * muestras_recall) / (muestras_precision + muestras_recall)
    
    lista_muestras[[mod]] <- muestras_f1
  }
  
  # Convertir a un data.table ancho (5 columnas, S filas)
  return(as.data.table(lista_muestras))
}

#' Calcula la matriz de dominancia bayesiana P(F1(i) > F1(j))
#'
#' @description 
#' Compara par a par las columnas de muestras de un data.table, evaluando la 
#' fracción de simulaciones donde el modelo de la fila supera al de la columna.
#'
#' @param dt_muestras_wide data.table. Formato ancho generado por simular_posterior_ventana.
#' @return Una matriz de 5x5 con los nombres de los modelos y las probabilidades de dominancia.
calcular_matriz_dominancia <- function(dt_muestras_wide) {
  nombres_modelos <- colnames(dt_muestras_wide)
  n_modelos <- length(nombres_modelos)
  
  # Inicializar matriz vacía
  matriz_dom <- matrix(NA_real_, nrow = n_modelos, ncol = n_modelos, 
                       dimnames = list(nombres_modelos, nombres_modelos))
  
  # Llenar la matriz celda por celda aplicando tu lógica conceptual (mean(i > j))
  for (i in nombres_modelos) {
    for (j in nombres_modelos) {
      matriz_dom[i, j] <- mean(dt_muestras_wide[[i]] > dt_muestras_wide[[j]])
    }
  }
  
  return(matriz_dom)
}