# Tema visual único del proyecto (paleta de marca + escala semáforo de favorabilidad).
# Se usa en todos los .qmd para mantener consistencia visual (ggplot2, highcharter, reactable).

library(ggplot2)
library(highcharter)
library(reactable)

# --- Paleta corporativa (ver _brand.yml) ------------------------------------
tgo_colors <- c(
  navy        = "#252036",
  dark_teal   = "#324f57",
  teal        = "#00acac",
  gold        = "#fcbf2f",
  gray        = "#c6c6c6",
  dark_gray   = "#424242"
)

# Paleta categórica (para series/grupos demográficos), en orden de uso preferente
tgo_paleta_categorica <- unname(tgo_colors[c("teal", "navy", "gold", "dark_teal", "gray", "dark_gray")])

# --- Escala semáforo de favorabilidad (separada de la paleta de marca) ------
# Uso exclusivo para % de favorabilidad "alto" (4-5), anclada a media_global/sd_global
# de las 50 preguntas estándar del modelo. Ver R/utils_metrics.R para los puntos de corte.
tgo_semaforo_colores <- c(
  rojo     = "#d73027",
  naranja  = "#F49643",
  amarillo = "#C1C41B",
  verde    = "#1a9850"
)

#' Color hex para una banda de semáforo ("rojo"/"naranja"/"amarillo"/"verde")
tgo_color_semaforo <- function(banda) {
  unname(tgo_semaforo_colores[as.character(banda)])
}

# --- Tema ggplot2 ------------------------------------------------------------
theme_tgo <- function(base_size = 12) {
  theme_minimal(base_size = base_size) +
    theme(
      plot.title = element_text(color = tgo_colors[["navy"]], face = "bold"),
      axis.title = element_text(color = tgo_colors[["dark_gray"]]),
      axis.text = element_text(color = tgo_colors[["dark_gray"]]),
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(color = tgo_colors[["gray"]], linewidth = 0.3),
      legend.position = "bottom",
      legend.title = element_blank(),
      strip.text = element_text(color = tgo_colors[["navy"]], face = "bold")
    )
}

# --- Tema highcharter ---------------------------------------------------------
hc_theme_tgo <- function() {
  hc_theme(
    colors = tgo_paleta_categorica,
    chart = list(style = list(fontFamily = "Segoe UI")),
    title = list(style = list(color = tgo_colors[["navy"]], fontWeight = "bold")),
    subtitle = list(style = list(color = tgo_colors[["dark_gray"]])),
    xAxis = list(
      gridLineColor = tgo_colors[["gray"]],
      labels = list(style = list(color = tgo_colors[["dark_gray"]])),
      lineColor = tgo_colors[["gray"]],
      tickColor = tgo_colors[["gray"]]
    ),
    yAxis = list(
      gridLineColor = tgo_colors[["gray"]],
      labels = list(style = list(color = tgo_colors[["dark_gray"]])),
      title = list(style = list(color = tgo_colors[["dark_gray"]]))
    ),
    legend = list(itemStyle = list(color = tgo_colors[["dark_gray"]])),
    tooltip = list(backgroundColor = tgo_colors[["navy"]], style = list(color = "#FFFFFF")),
    credits = list(enabled = FALSE)
  )
}

# --- Tema reactable ------------------------------------------------------------
reactable_theme_tgo <- function() {
  reactableTheme(
    color = tgo_colors[["dark_gray"]],
    backgroundColor = "#FFFFFF",
    borderColor = tgo_colors[["gray"]],
    stripedColor = "#F7F7F8",
    highlightColor = "#EFF8F8",
    headerStyle = list(
      backgroundColor = tgo_colors[["navy"]],
      color = "#FFFFFF",
      fontWeight = 600
    ),
    searchInputStyle = list(width = "100%")
  )
}

options(reactable.theme = reactable_theme_tgo())
