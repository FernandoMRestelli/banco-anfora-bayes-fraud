# ==============================================================================
# SCRIPT: visualizaciones_EDA.R
# OBJETIVO: Funciones para generar gráficos comparativos entre T0 y T1 para las variables y para clases (fraude vs no fraude)
# ==============================================================================
library(dplyr)
library(ggplot2)
library(patchwork)


# =====================================================================
# VARIABLES NUMÉRICAS: Comparativo de distribuciones por clase entre T0 y T1
# =====================================================================


#' Gráficos comparativos de tasa de fraude entre T0 y T1
#'
#' @description 
#' Generar histogramas por clase por clase por variable.

calcular_hist_por_clase <- function(data, col, bins) {
  data <- data %>%
    select(
      valor = all_of(col),
      es_fraude
    ) %>%
    filter(!is.na(valor), !is.na(es_fraude)) %>%
    mutate(es_fraude = as.character(es_fraude))
  
  clases <- unique(data$es_fraude)
  
  hist_data <- lapply(clases, function(clase) {
    
    valores_clase <- data %>%
      filter(es_fraude == clase) %>%
      pull(valor)
    
    h <- hist(
      valores_clase,
      breaks = bins,
      plot = FALSE,
      include.lowest = TRUE
    )
    
    tibble(
      bin_mid = h$mids,
      count = h$counts,
      porcentaje = h$counts / sum(h$counts) * 100,
      es_fraude = clase
    )
  }) %>%
    bind_rows()
  
  hist_data
}

#' Gráfica las distribuciones numéricas de las variables comunes entre T0 y T1, comparando clases (fraude vs no fraude)
#'
#' @description 
#' Genera un gráfico de de barras para cada variable numérica común entre T0 y T1, mostrando la distribución por clase (fraude vs no fraude) y comparando visualmente entre ambos períodos. 
#' @return Un objeto ggplot listo para ser impreso o guardado.

graficar_distribuciones_numericas <- function(
    operaciones_T0,
    operaciones_T1,
    excluir = c("op_id", "secuencia_24h", "mes"),
    n_bins = 30
) {
  
  numericas_T0 <- operaciones_T0 %>%
    select(where(is.numeric)) %>%
    select(-any_of(excluir))
  
  numericas_T1 <- operaciones_T1 %>%
    select(where(is.numeric)) %>%
    select(-any_of(excluir))
  
  cols_comunes <- intersect(names(numericas_T0), names(numericas_T1))
  
  palette_fraude <- c(
    "FALSE" = "#9BFF99",
    "TRUE"  = "#ED4123"
  )
  
  plots <- list()
  
  for (col in cols_comunes) {
    
    min_val <- min(
      operaciones_T0[[col]],
      operaciones_T1[[col]],
      na.rm = TRUE
    )
    
    max_val <- max(
      operaciones_T0[[col]],
      operaciones_T1[[col]],
      na.rm = TRUE
    )
    
    bins <- seq(min_val, max_val, length.out = n_bins)
    bin_width <- diff(bins)[1]
    
    hist_T0 <- calcular_hist_por_clase(
      data = operaciones_T0,
      col = col,
      bins = bins
    )
    
    hist_T1 <- calcular_hist_por_clase(
      data = operaciones_T1,
      col = col,
      bins = bins
    )
    
    p_T0 <- ggplot(
      hist_T0,
      aes(
        x = bin_mid,
        y = porcentaje,
        fill = es_fraude
      )
    ) +
      geom_col(
        position = position_dodge(width = bin_width),
        width = bin_width * 0.8
      ) +
      scale_fill_manual(
        values = palette_fraude,
        labels = c("FALSE" = "No Fraude", "TRUE" = "Fraude"),
        name = "Clase"
      ) +
      labs(
        title = paste("T0 -", col),
        x = col,
        y = "Porcentaje (%) por clase"
      ) +
      theme_minimal()
    
    p_T1 <- ggplot(
      hist_T1,
      aes(
        x = bin_mid,
        y = porcentaje,
        fill = es_fraude
      )
    ) +
      geom_col(
        position = position_dodge(width = bin_width),
        width = bin_width * 0.8
      ) +
      scale_fill_manual(
        values = palette_fraude,
        labels = c("FALSE" = "No Fraude", "TRUE" = "Fraude"),
        name = "Clase"
      ) +
      labs(
        title = paste("T1 -", col),
        x = col,
        y = NULL
      ) +
      theme_minimal()
    
    plot_final <- p_T0 + p_T1 +
      plot_annotation(
        title = paste("Comparación T0 vs T1 -", col)
      )
    
    plots[[col]] <- plot_final
    
    print(plot_final)
  }
  
  return(plots)

}


# =====================================================================
# VARIABLES CATEGÓRICAS: Comparativo de distribuciones por clase entre T0 y T1
# =====================================================================


## Funcion para calcular la tasa de fraude por categoría:

calcular_tasa_fraude_cat <- function(data, col) {
  
  data %>%
    filter(!is.na(.data[[col]])) %>%
    group_by(categoria = .data[[col]]) %>%
    summarise(
      total = n(),
      fraudes = sum(es_fraude, na.rm = TRUE),
      tasa_fraude = mean(es_fraude, na.rm = TRUE),
      tasa_fraude_pct = tasa_fraude * 100,
      .groups = "drop"
    ) %>%
    arrange(desc(tasa_fraude_pct))
}


## Función principal para graficar categorias: 

graficar_tasa_fraude_categoricas <- function(
    operaciones_T0,
    operaciones_T1,
    excluir = c(),
    guardar = FALSE,
    carpeta_salida = "reports/figures"
) {
  
  categoricas_T0 <- operaciones_T0 %>%
    select(where(~ is.character(.x) || is.factor(.x))) %>%
    select(-any_of(excluir))
  
  categoricas_T1 <- operaciones_T1 %>%
    select(where(~ is.character(.x) || is.factor(.x))) %>%
    select(-any_of(excluir))
  
  cols_comunes_cat <- intersect(
    names(categoricas_T0),
    names(categoricas_T1)
  )
  
  plots <- list()
  tablas_T0 <- list()
  tablas_T1 <- list()
  
  if (guardar) {
    dir.create(carpeta_salida, recursive = TRUE, showWarnings = FALSE)
  }
  
  for (col in cols_comunes_cat) {
    
    tabla_T0 <- calcular_tasa_fraude_cat(operaciones_T0, col) %>%
      mutate(variable = col)
    
    tabla_T1 <- calcular_tasa_fraude_cat(operaciones_T1, col) %>%
      mutate(variable = col)
    
    tablas_T0[[col]] <- tabla_T0
    tablas_T1[[col]] <- tabla_T1
    
    ymax <- max(
      tabla_T0$tasa_fraude_pct,
      tabla_T1$tasa_fraude_pct,
      na.rm = TRUE
    )
    
    p_T0 <- ggplot(
      tabla_T0,
      aes(
        x = reorder(as.character(categoria), -tasa_fraude_pct),
        y = tasa_fraude_pct,
        fill = as.character(categoria)
      )
    ) +
      geom_col(show.legend = FALSE) +
      geom_text(
        aes(label = paste0(round(tasa_fraude_pct, 2), "%")),
        vjust = 1.2,
        size = 3
      ) +
      coord_cartesian(ylim = c(0, ymax * 1.10)) +
      labs(
        title = paste("T0 - tasa de fraude por", col),
        x = col,
        y = "Tasa de fraude (%)"
      ) +
      theme_minimal() +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1)
      )
    
    p_T1 <- ggplot(
      tabla_T1,
      aes(
        x = reorder(as.character(categoria), -tasa_fraude_pct),
        y = tasa_fraude_pct,
        fill = as.character(categoria)
      )
    ) +
      geom_col(show.legend = FALSE) +
      geom_text(
        aes(label = paste0(round(tasa_fraude_pct, 2), "%")),
        vjust = 1.2,
        size = 3
      ) +
      coord_cartesian(ylim = c(0, ymax * 1.10)) +
      labs(
        title = paste("T1 - tasa de fraude por", col),
        x = col,
        y = NULL
      ) +
      theme_minimal() +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1)
      )
    
    plot_final <- p_T0 + p_T1 +
      plot_annotation(
        title = paste("Tasa de fraude por categoría -", col)
      )
    
    plots[[col]] <- plot_final
    
    print(plot_final)
    
    if (guardar) {
      
      archivo_salida <- file.path(
        carpeta_salida,
        paste0("04_tasa_fraude_categorica_", col, ".png")
      )
      
      ggsave(
        filename = archivo_salida,
        plot = plot_final,
        width = 10,
        height = 5,
        dpi = 300
      )
      
      cat("Guardado:", archivo_salida, "\n")
    }
  }
  
  tabla_T0_final <- bind_rows(tablas_T0)
  tabla_T1_final <- bind_rows(tablas_T1)
  
  return(
    list(
      plots = plots,
      tabla_T0 = tabla_T0_final,
      tabla_T1 = tabla_T1_final
    )
  )
}