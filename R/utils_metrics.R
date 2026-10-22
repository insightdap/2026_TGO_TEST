# Cálculo del semáforo de favorabilidad y del margen de error muestral.
# Regla de negocio confirmada por el usuario.

compute_semaforo_breaks <- function(pct_alto_preguntas_estandar) {
  media_global <- mean(pct_alto_preguntas_estandar, na.rm = TRUE)
  sd_global <- sd(pct_alto_preguntas_estandar, na.rm = TRUE)

  list(
    media_global = media_global,
    sd_global = sd_global,
    rojo = 0,
    naranja = round(media_global - sd_global, digits = 0) - 0.5,
    amarillo = round(media_global, digits = 0) - 0.5,
    verde = round(media_global + sd_global, digits = 0) - 0.5
  )
}

#' Clasifica un vector de % alto en bandas "rojo"/"naranja"/"amarillo"/"verde"
#' usando los puntos de corte de `compute_semaforo_breaks()`.
clasificar_semaforo <- function(pct, breaks) {
  cut(
    pct,
    breaks = c(-Inf, breaks$naranja, breaks$amarillo, breaks$verde, Inf),
    labels = c("rojo", "naranja", "amarillo", "verde"),
    right = FALSE
  ) |> as.character()
}

#' Margen de error muestral con corrección por población finita.
#' z = 1.96 (95%), p = 0.5 por defecto — confirmado por el usuario.
margen_error <- function(n, N, confianza = 0.95, p = 0.5) {
  if (is.na(n) || is.na(N) || n <= 0 || N <= 1) return(NA_real_)
  z <- stats::qnorm(1 - (1 - confianza) / 2)
  moe <- z * sqrt(p * (1 - p) / n)
  fpc <- sqrt((N - n) / (N - 1))
  moe * fpc
}

#' Oculta (NA) un valor si el grupo no alcanza el tamaño mínimo de reporte.
#' Aplica tanto a los dashboards como a la matriz exportable de Excel.
suprimir_si_n_insuficiente <- function(valor, n, min_n) {
  dplyr::if_else(n < min_n, NA_real_, valor)
}

#' Extrae el orden oficial de categorías de una tabla "key_*" (2 columnas: orden, etiqueta)
ordenar_categorias <- function(key_df) key_df[order(key_df[[1]]), 
][[2]]
