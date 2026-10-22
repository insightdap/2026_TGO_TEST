# Funciones de limpieza, joins y cálculo de métricas de favorabilidad reutilizables
# en todos los scripts/ y .qmd.

library(dplyr)
library(tidyr)
library(stringr)

#' Arma la tabla de respuestas en formato largo (1 fila = 1 respuesta a 1 pregunta),
#' con demográficos propios de la respuesta + área/país/líder (tomados de base_personas
#' via "rf") + catálogo de factor/dimensión (tomado de "modelo").
build_respuestas_long <- function(respuestas, base_personas, modelo) {
  ids_preguntas <- modelo$id_cuestionario

  # Todas las columnas area_* con al menos un valor no vacío
  cols_area_con_datos <- base_personas |>
    select(starts_with("area_")) |>
    select(where(~ any(!is.na(.x)))) |>
    names()

  respuestas_demo <- respuestas |>
    left_join(
      base_personas |>
        select(rf, company, pais, all_of(cols_area_con_datos), lider = lider_1),
      by = "rf"
    )
  
  respuestas_demo |>
    pivot_longer(
      cols = all_of(ids_preguntas),
      names_to = "id_cuestionario",
      values_to = "valor_raw"
    ) |>
    left_join(modelo, by = "id_cuestionario") |>
    mutate(
      # Regla de negocio: los vacíos se recodifican a 0 y se excluyen del denominador
      nps_question = replace_na(nps_question,0),
      valor = if_else(is.na(valor_raw), 0, valor_raw),
      respondida = valor > 0,
      banda = case_when(
        !respondida ~ NA_character_,
        valor <= 2   ~ "bajo",
        valor == 3   ~ "medio",
        valor >= 4   ~ "alto"
      )
    )
}

#' % de respuestas válidas (>0) que caen en la banda "alto" (4-5)
pct_alto <- function(valor) {
  validas <- valor[valor > 0]
  if (length(validas) == 0) return(NA_real_)
  100 * mean(validas >= 4)
}

#' % de respuestas válidas que caen en la banda "medio" (3)
pct_medio <- function(valor) {
  validas <- valor[valor > 0]
  if (length(validas) == 0) return(NA_real_)
  100 * mean(validas == 3)
}

#' % de respuestas válidas que caen en la banda "bajo" (1-2)
pct_bajo <- function(valor) {
  validas <- valor[valor > 0]
  if (length(validas) == 0) return(NA_real_)
  100 * mean(validas <= 2)
}

#' Cantidad de respuestas válidas (excluye vacíos recodificados a 0)
n_validas <- function(rf) n_distinct(rf, na.rm = TRUE)


#' Niveles ordenados (segun catalogo oficial) para una variable demo_*,
#' presentes en `valores`. El catalogo se lee de data/processed/niveles_demo.parquet,
#' subido por scripts/actualizar_metricas.R (reemplaza la lectura directa de los
#' key_* crudos, que ya no se persisten en disco).

# niveles_demo <- function(segmento, valores, ruta_processed = "data/processed") {
#   archivo <- file.path(ruta_processed, "niveles_demo.parquet")
#   if (!file.exists(archivo)) return(sort(unique(valores)))
# 
#   catalogo <- arrow::read_parquet(archivo) |>
#     dplyr::filter(segmento == !!segmento) |>
#     dplyr::arrange(orden)
# 
#   if (nrow(catalogo) == 0) return(sort(unique(valores)))
#   intersect(catalogo$nivel, unique(valores))
# }

#' Resumen de favorabilidad (n válidas, promedio 1-5, %bajo/medio/alto), agrupando
#' por las variables indicadas. Si `group_vars` está vacío, resume el total global.
#' El promedio/porcentaje siempre se calcula a partir de las respuestas individuales
#' que componen el grupo (no como promedio de promedios).
resumir_favorabilidad <- function(data, group_vars = character(0)) {
  if (length(group_vars) > 0) {
    data <- data |> group_by(across(all_of(group_vars)))
  }
  data |>
    summarise(
      n_validas = n_validas(rf),
      promedio = mean(valor[valor > 0]),
      pct_bajo = pct_bajo(valor),
      pct_medio = pct_medio(valor),
      pct_alto = pct_alto(valor),
      .groups = "drop"
      )
}

#' % de respuestas válidas (>0) que caen en la banda "alto" (4-5)
pct_promotores <- function(nps_question) {
  validas <- nps_question[nps_question > 0]
  if (length(validas) == 0) return(NA_real_)
  100 * mean(validas >= 9)
}

#' % de respuestas válidas que caen en la banda "medio" (3)
pct_pasivos <- function(nps_question) {
  validas <- nps_question[nps_question > 0]
  if (length(validas) == 0) return(NA_real_)
  100 * (mean(validas == 7) + mean(validas == 8))
}

#' % de respuestas válidas que caen en la banda "bajo" (1-2)
pct_detractores <- function(nps_question) {
  validas <- nps_question[nps_question > 0]
  if (length(validas) == 0) return(NA_real_)
  100 * mean(validas <= 6)
}

resumir_nps <- function(data, group_vars = character(0)) {
  if (length(group_vars) > 0) {
    data <- data |> group_by(across(all_of(group_vars)))
  }
  data |>
    summarise(
      n_validas = n_validas(rf),
      promedio = mean(nps_question[nps_question > 0]),
      pct_detractores = pct_detractores(nps_question),
      pct_pasivos = pct_pasivos(nps_question),
      pct_promotores = pct_promotores(nps_question),
      .groups = "drop"
    )
}
