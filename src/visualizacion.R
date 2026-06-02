# ==============================================================================
# SCRIPT: visualizacion.R
# OBJETIVO: Funciones para generar gráficos con ggplot2 a partir de simulaciones
# ==============================================================================

library(ggplot2)
library(data.table)

#' Gráfico de densidades superpuestas para la validación T0
#'
#' @description 
#' Genera un gráfico de densidad para las 5000 muestras de la posterior del F1-Score,
#' destacando visualmente al modelo M3 frente al resto.
#'
#' @param dt_f1_crudo data.table. Contiene las columnas Modelo y F1_Muestra.
#' @return Un objeto ggplot listo para ser impreso o guardado.
graficar_posterior_t0 <- function(dt_f1_crudo) {
  # Trabajamos sobre una copia para no modificar el dt original por referencia
  dt_plot <- copy(dt_f1_crudo)
  
  # Lógica de colores y destaque (Hardcodeada para el caso de negocio)
  dt_plot[, Destacado := ifelse(Modelo == "M3_gbm_completo", "M3 (Elegido)", "Otros Modelos")]
  colores_paleta <- c("M3 (Elegido)" = "#0072B2", "Otros Modelos" = "#999999")
  
  # Construcción del gráfico
  g <- ggplot(dt_plot, aes(x = F1_Muestra, group = Modelo, fill = Destacado, color = Destacado)) +
    geom_density(alpha = 0.4, linewidth = 0.8) +
    scale_fill_manual(values = colores_paleta) +
    scale_color_manual(values = colores_paleta) +
    coord_cartesian(xlim = c(0, 1)) + 
    labs(
      title = "Comparación de Distribuciones Posteriores del F1-Score",
      subtitle = "Validación Histórica T0: M3 demuestra una dominancia absoluta",
      x = "Valor del F1-Score (Muestras de la Posterior)",
      y = "Densidad Posterior", 
      fill = "Condición", 
      color = "Condición"
    ) +
    theme_minimal(base_size = 13) +
    theme(panel.grid.minor = element_blank(), legend.position = "bottom")
  
  return(g)
}

#' Gráfico de trayectoria temporal con bandas de incertidumbre (HPDI)
#'
#' @description 
#' Genera un gráfico de líneas para la evolución del F1-Score de los 5 modelos,
#' incluyendo las cintas de incertidumbre Bayesiana y marcadores de hitos temporales.
#'
#' @param dt_trayectorias data.table. Contiene mes, modelo, Mediana_F1, q025 y q975.
#' @return Un objeto ggplot listo para ser impreso o guardado.
graficar_trayectorias_temporales <- function(dt_trayectorias) {
  
  g <- ggplot(dt_trayectorias, aes(x = mes, y = Mediana_F1, color = modelo, fill = modelo)) +
    geom_line(linewidth = 1) +
    geom_ribbon(aes(ymin = q025, ymax = q975), alpha = 0.15, color = NA) +
    
    # Hitos del caso de negocio
    geom_vline(xintercept = 12, linetype = "dashed", color = "darkgreen", linewidth = 0.7) +
    geom_vline(xintercept = 13, linetype = "dashed", color = "firebrick", linewidth = 0.7) +
    geom_vline(xintercept = 16, linetype = "dashed", color = "purple", linewidth = 0.7) +
    
    annotate("text", x = 11.5, y = 0.7, label = "Fin de T0", angle = 90, color = "darkgreen", size = 3.5) +
    annotate("text", x = 13.5, y = 0.7, label = "Inicio Phishing", angle = 90, color = "firebrick", size = 3.5) +
    annotate("text", x = 16.5, y = 0.7, label = "Estabilización", angle = 90, color = "purple", size = 3.5) +
    
    labs(
      title = "Evolución Mensual del F1-Score con Incertidumbre Bayesiana",
      subtitle = "Meses 1 a 18: Evidencia del colapso de M3 ante la campaña de phishing",
      x = "Mes de Operación en Producción",
      y = "F1-Score (Mediana e Intervalo HPDI 95%)",
      color = "Modelo", 
      fill = "Modelo"
    ) +
    scale_x_continuous(breaks = 1:18) +
    theme_minimal(base_size = 12) +
    theme(legend.position = "bottom")
  
  return(g)
}

#' Gráfico de densidades solapadas para la ventana operativa
#'
#' @param dt_muestras_wide data.table. Muestras en formato ancho.
#' @return Un objeto ggplot con las 5 densidades en el mismo eje numérico.
graficar_solapamiento_ventana <- function(dt_muestras_wide) {
  # Pasamos a formato largo (long) solo para facilitarle la vida a ggplot
  dt_long <- melt(dt_muestras_wide, measure.vars = colnames(dt_muestras_wide),
                  variable.name = "Modelo", value.name = "F1_Muestra")
  
  g <- ggplot(dt_long, aes(x = F1_Muestra, fill = Modelo, color = Modelo)) +
    geom_density(alpha = 0.25, linewidth = 0.9) +
    scale_fill_brewer(palette = "Set1") +
    scale_color_brewer(palette = "Set1") +
    coord_cartesian(xlim = c(0, 1)) +
    labs(
      title = "Solapamiento de Densidades Posteriores del F1-Score",
      subtitle = "Ventana Operativa Crítica: Meses 16 a 18 (Post-Crisis de Phishing)",
      x = "Valor del F1-Score",
      y = "Densidad Posterior"
    ) +
    theme_minimal(base_size = 13) +
    theme(panel.grid.minor = element_blank(), legend.position = "bottom")
  
  return(g)
}