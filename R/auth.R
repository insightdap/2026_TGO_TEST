# Autenticación y configuración centralizada.
# Reutiliza el patrón ya validado en carga_datos.R: clave de servicio CIFRADA
# (segura de versionar) + llave de descifrado en la variable de entorno
# GS4_ENCUESTAS_KEY (definida en .Renviron, NO versionada).

library(yaml)
library(googlesheets4)
library(gargle)

#' Lee config/pipeline_config.yml
tgo_config <- function(path = "config/pipeline_config.yml") {
  yaml::read_yaml(path)
}

#' Autentica googlesheets4 (y opcionalmente googledrive) con la cuenta de servicio del proyecto
# tgo_auth <- function(config = tgo_config(), con_drive = FALSE) {
#   googlesheets4::gs4_auth(
#     path = gargle::secret_decrypt_json(
#       config$auth$service_account_file,
#       key = config$auth$key_env_var
#     )
#   )
#   if (con_drive) {
#     library(googledrive)
#     googledrive::drive_auth(token = googlesheets4::gs4_token())
#   }
#   invisible(TRUE)
# }

tgo_auth <- function(config = tgo_config()) {
  googlesheets4::gs4_auth(
    path = gargle::secret_decrypt_json(
      config$auth$service_account_file,
      key = config$auth$key_env_var
    )
  )
}
