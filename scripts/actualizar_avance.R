

library(dplyr)
library(tidyr)
library(purrr)
library(stringr)
library(lubridate)
library(googlesheets4)

source("R/auth.R")
source("R/utils_metrics.R")

`%||%` <- function(x, y) if (length(x) == 0 || is.na(x)) y else x

config <- tgo_config()
cliente <- config$cliente$nombre
offset_horas <- config$encuesta$zona_horaria_offset_horas
min_n <- config$reglas_negocio$min_n_grupo
confianza <- config$reglas_negocio$moe_confianza
p_moe <- config$reglas_negocio$moe_p

# =============================================================================
# 1. Extracción -------------------------------
# =============================================================================

tgo_auth(config)

sheets <- config$google_sheets

respuestas <- read_sheet(sheets$respuestas_id) |>
  mutate(
    `Submitted at` = as.POSIXct(`Submitted at`),
    fecha = `Submitted at` - hours(offset_horas)
  ) |> 
  relocate(fecha,.after = `Submitted at`) |> 
  filter(!is.na(rf)) #Eliminando capturas de datos sin referencia de colaborador

base_personas <- read_sheet(sheets$base_personas_id) |>
  # `user_login` llega como columna de lista (artefacto de la hoja origen); se aplana.
  mutate(user_login = purrr::map_chr(user_login, ~ as.character(.x[1])))

#Demográficos de la encuesta
key_genero <- read_sheet(sheets$catalogos_id, sheet = "genero")
key_jerarquia <- read_sheet(sheets$catalogos_id, sheet = "jerarquia")
key_tiempo_laboral <- read_sheet(sheets$catalogos_id, sheet = "tiempo_laboral")
key_edad <- read_sheet(sheets$catalogos_id, sheet = "edad")
# key_nps <- read_sheet(sheets$catalogos_id, sheet = "nps")


niveles_demo <- bind_rows(
  tibble(segmento = "dem_genero", nivel = ordenar_categorias(key_genero)),
  tibble(segmento = "dem_jerarquia", nivel = ordenar_categorias(key_jerarquia)),
  tibble(segmento = "dem_tiempo_laboral", nivel = ordenar_categorias(key_tiempo_laboral)),
  tibble(segmento = "dem_edad", nivel = ordenar_categorias(key_edad))
) |>
  group_by(segmento) |>
  mutate(orden = row_number()) |>
  ungroup()

segmentos_demo <- c("dem_genero", "dem_jerarquia", "dem_tiempo_laboral", "dem_edad")

# =============================================================================
# 2. Contador: total, diario, margen de error global y por país/área ------------
# =============================================================================
N_total <- nrow(base_personas)
n_respuestas <- nrow(respuestas)

avance_global <- tibble(
  invitados = N_total,
  respuestas = n_respuestas,
  pct_avance = 100 * n_respuestas / N_total,
  margen_error = 100 * margen_error(n_respuestas, N_total, confianza, p_moe)
)

avance_diario <- respuestas |>
  mutate(dia = as.Date(fecha)) |>
  count(dia, name = "respuestas_dia") |>
  arrange(dia) |>
  mutate(respuestas_acum = cumsum(respuestas_dia))

avance_conteo_demo <- map_dfr(segmentos_demo, function(seg) {
  respuestas |>
    filter(!is.na(.data[[seg]])) |>
    count(grupo = .data[[seg]], name = "respuestas") |>
    mutate(segmento = seg)
})

avance_diario_demo <- map_dfr(segmentos_demo, function(seg) {
  respuestas |>
    filter(!is.na(.data[[seg]])) |>
    mutate(dia = as.Date(fecha), grupo = .data[[seg]]) |>
    count(segmento = seg, grupo, dia, name = "respuestas_dia") |>
    arrange(grupo, dia) |>
    group_by(grupo) |>
    mutate(respuestas_acum = cumsum(respuestas_dia)) |>
    ungroup()
})

base_personas <- base_personas |>
  mutate(respondio = rf %in% respuestas$rf)

cols_area_con_datos <- base_personas |>
  select(starts_with("area_")) |>
  select(where(~ any(!is.na(.x)))) |>
  names()

segmentos_base <- c("pais", cols_area_con_datos)

avance_base_personas <- map_dfr(segmentos_base, function(seg) {
  base_personas |>
    filter(!is.na(.data[[seg]])) |>
    group_by(grupo = .data[[seg]]) |>
    summarise(invitados = n(), respuestas = sum(respondio), .groups = "drop") |>
    mutate(
      segmento = seg,
      pct_avance = 100 * respuestas / invitados,
      margen_error = 100 * map2_dbl(respuestas, invitados, ~ margen_error(.x, .y, confianza, p_moe))
    )
})

