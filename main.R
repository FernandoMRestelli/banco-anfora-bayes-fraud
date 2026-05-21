# ==============================================================================
# MAIN.R - Orquestador del Pipeline de Detección de Fraude
# ==============================================================================

# ------------------------------------------------------------------------------
# LOGS AUTOMÁTICOS - crear una carpeta logs
# ------------------------------------------------------------------------------
if (!dir.exists("logs")) {
  dir.create("logs", recursive = TRUE)
}

timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
ruta_log  <- paste0("logs/ejecucion_", timestamp, ".log")

log_con <- file(ruta_log, open = "wt")
sink(log_con, type = "output")  # Desvía prints, tablas y cats
sink(log_con, type = "message") # Desvía warnings y mensajes de error

# Cargar librerías core
library(data.table)
library(ggplot2)

# Importar los módulos del proyecto
source("src/data_prep.R")
source("src/monte_carlo.R")
source("src/visualizacion.R")

# ==============================================================================
# VALIDACIÓN HISTÓRICA (T0)
# ==============================================================================
cat("Procesando Fase 1 (T0 Validación)...\n")
# Carga de datos
json_data <- cargar_datos_t0("data/raw/metricas_T0_validacion.json")
modelos_lista <- c("M1_logistica", "M2_random_forest", "M3_gbm_completo", "M4_gbm_moderado", "M5_naive_bayes")

# Simulación (Extrae los datos crudos para el gráfico)
set.seed(20260424)
dt_f1_crudo_t0 <- rbindlist(lapply(modelos_lista, simular_posterior_f1_t0, datos_json = json_data))

# GENERAR LA TABLA RESUMEN DE T0
tabla_resumen_t0 <- dt_f1_crudo_t0[, .(
  Conteos    = first(Conteos), 
  Mediana_F1 = round(median(F1_Muestra), 3),
  HPDI_95    = paste0("[", round(quantile(F1_Muestra, 0.025), 3), ", ", 
                          round(quantile(F1_Muestra, 0.975), 3), "]")
), by = .(Modelo)]

# Guardar Tabla Procesada
guardar_tabla_procesada(tabla_resumen_t0, "reports/tables/01_resumen_t0_validacion.csv")

# Visualización y guardado de gráfico
grafico_t0 <- graficar_posterior_t0(dt_f1_crudo_t0)
ggsave(filename = "reports/figures/01_densidades_t0.png", plot = grafico_t0, width = 10, height = 6, dpi = 300)

# ==============================================================================
# ANÁLISIS DE TRAYECTORIA Y CRISIS (T1)
# ==============================================================================
cat("Procesando Fase 2 (Trayectorias Temporales)...\n")
# Carga y preparación
df_temporales <- fread("data/raw/metricas_temporales.csv")
df_1_18 <- df_temporales[mes >= 1 & mes <= 18]
dt_listo  <- preparar_conteos_temporales(df_1_18)

# Simulación fila por fila
dt_trayectorias <- dt_listo[, {
  res <- calcular_posterior_fila(tp, fp, fn)
  .(Mediana_F1 = round(res$Mediana_F1, 4), 
    q025       = round(res$q025, 4), 
    q975       = round(res$q975, 4))
}, by = .(modelo, mes)]

# Guardar Tabla Procesada de Trayectorias
guardar_tabla_procesada(dt_trayectorias, "reports/tables/02_trayectorias_mes_a_mes.csv")

# Visualización y guardado de gráfico
grafico_crisis <- graficar_trayectorias_temporales(dt_trayectorias)
ggsave(filename = "reports/figures/02_trayectorias_crisis.png", plot = grafico_crisis, width = 12, height = 7, dpi = 300)

cat("========================================================\n")
cat("Pipeline ejecutado con éxito.\n")
cat("-> Gráficos guardados en: reports/figures/\n")
cat("-> Tablas CSV guardadas en: data/processed/\n")
cat("========================================================\n")

# Volvemos a conectar la salida a la terminal normal y cerramos el archivo log
sink(type = "message")
sink(type = "output")
close(log_con)