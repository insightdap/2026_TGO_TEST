# Exportable Excel: matriz de preguntas (filas) x grupos demográficos (columnas),
# solo % de favorabilidad "alto" (4-5), con relleno de color según el semáforo
# (misma clasificación y paleta que los dashboards).

library(dplyr)
library(tidyr)
library(openxlsx2)

#' Construye y colorea una hoja-matriz dentro de un workbook openxlsx2 existente.
#'
#' @param wb workbook openxlsx2 (wb_workbook())
#' @param hoja nombre de la pestaña
#' @param datos data.frame largo con columnas: fila_var, col_var, valor_var, n_var
#' @param breaks lista devuelta por compute_semaforo_breaks()
escribir_hoja_matriz <- function(wb, hoja, datos, fila_var, col_var,
                                  valor_var = "pct_alto", n_var = "n_validas",
                                  breaks, min_n) {
  datos_sup <- datos |>
    mutate(
      !!valor_var := suprimir_si_n_insuficiente(.data[[valor_var]], .data[[n_var]], min_n)
    )

  matriz <- datos_sup |>
    select(all_of(c(fila_var, col_var, valor_var))) |>
    pivot_wider(names_from = all_of(col_var), values_from = all_of(valor_var))
  
  # matriz_n <- datos_sup |>
  #   select(all_of(c(col_var, n_var))) |>
  #   pivot_wider(names_from = all_of(col_var), values_from = all_of(n_var))

  wb <- wb |>
    wb_add_worksheet(sheet = hoja) |>
    # wb_add_data(sheet = hoja, x = matriz_n, start_row = 1, start_col = 1, na = NULL) |>
    wb_add_data(sheet = hoja, x = matriz, start_row = 5, start_col = 1, na = NULL) |>
    wb_freeze_pane(sheet = hoja, first_row = TRUE, first_col = TRUE) |>
    wb_add_font(sheet = hoja, dims = wb_dims(rows = 1, cols = seq_len(ncol(matriz))), bold = TRUE)

  n_filas <- nrow(matriz)
  n_cols <- ncol(matriz)

  if (n_filas > 0 && n_cols > 1) {
    for (cl in 2:n_cols) {
      for (f in seq_len(n_filas)) {
        valor <- matriz[[cl]][f]
        if (is.na(valor)) next
        banda <- clasificar_semaforo(valor, breaks)
        color <- tgo_color_semaforo(banda)
        wb <- wb_add_fill(
          wb, sheet = hoja,
          dims = wb_dims(rows = f + 1, cols = cl),
          color = wb_color(hex = color)
        )
      }
    }
  }

  wb
}

#' Arma el libro completo: una hoja por variable demográfica + una hoja "Global por factor".
#' `metricas_pregunta_segmento` y `metricas_factor_global` ya deben venir con pct_alto = % alto (4-5).
construir_excel_resultados <- function(metricas_pregunta_segmento, metricas_factor_global,
                                        breaks, min_n) {
  wb <- wb_workbook()

  wb <- escribir_hoja_matriz(
    wb, "Global por factor",
    metricas_factor_global |> mutate(grupo = "Favorabilidad (%)"),
    fila_var = "factor", col_var = "grupo",
    breaks = breaks, min_n = 0
  )

  for (seg in unique(metricas_pregunta_segmento$segmento)) {
    datos_seg <- metricas_pregunta_segmento |> filter(segmento == seg)
    wb <- escribir_hoja_matriz(
      wb, substr(seg, 1, 31), datos_seg,
      fila_var = "pregunta", col_var = "grupo",
      breaks = breaks, min_n = min_n
    )
  }

  wb
}
