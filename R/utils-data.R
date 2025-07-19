# R/utils-data.R

#' Obtiene el board de cach\u00e9 local de GeoDOM
#' @noRd
get_geodom_cache <- function() {
  .pkg_env$geodom_cache_board
}

#' Obtiene la URL base para la descarga de datos
#' @noRd
get_base_data_url <- function() {
  .pkg_env$base_data_url
}

#' Descarga, procesa y cachea un archivo de datos espaciales
#'
#' Esta funci\u00f3n es el motor de datos del paquete.
#' 1. Comprueba si una versi\u00f3n procesada (.json) del dato existe en el cach\u00e9 local.
#' 2. Si existe, la lee y la devuelve (muy r\u00e1pido).
#' 3. Si no, construye la URL completa, descarga el archivo fuente,
#'    lo lee como un objeto sf, y lo guarda como un pin .json en el caché para el futuro.
#'
#' @param id El nombre del archivo en el servidor remoto (ej. "RD_MREG").
#' @param ... Argumentos adicionales para sf::st_read().
#' @return Un objeto sf.
#' @noRd
fetch_and_cache <- function(id, ...) {
  # browser()
  local_board <- get_geodom_cache()

  # 1. VERIFICAR CACH\u00c9
  if (pins::pin_exists(local_board, id)) {
    message("Cargando '", id, "' desde cach\u00e9 local (r\u00e1pido).")
    return(sf::st_as_sf(pins::pin_read(local_board, id)))
  }

  # 2. CONSTRUIR URL Y DESCARGAR SI NO EST\u00c1 EN CACH\u00c9
  base_url <- get_base_data_url()
  full_url <- paste0(base_url, 'TopoJSON/', id, ".json")

  message(
    "Pin '",
    id,
    "' no encontrado localmente. Descargando..."
  )

  data_sf <- tryCatch(
    {
      sf::st_read(full_url, quiet = TRUE, ...)
    },
    error = function(e) {
      stop(
        "La descarga y lectura del archivo fall\u00f3.\n  URL: ",
        full_url,
        "\n  Error: ",
        e$message
      )
    }
  )

  # 3. GUARDAR EL OBJETO PROCESADO EN CACH\u00c9
  pins::pin_write(
    board = local_board,
    x = data_sf,
    name = id,
    type = "rds",
    title = paste("Datos para", id)
  )
  message("Pin '", id, "' guardado en cach\u00e9 para uso futuro.")

  return(data_sf)
}

gd_get_dataset <- function(id, ...) {
  # browser()
  local_board <- get_geodom_cache()

  # 1. VERIFICAR CACH\u00c9
  if (pins::pin_exists(local_board, id)) {
    message("Cargando '", id, "' desde cach\u00e9 local (r\u00e1pido).")
    return(pins::pin_read(local_board, id))
  }

  # 2. CONSTRUIR URL Y DESCARGAR SI NO EST\u00c1 EN CACH\u00c9
  base_url <- get_base_data_url()
  full_url <- paste0(base_url, 'datasets/', id, ".json")

  message(
    "Pin '",
    id,
    "' no encontrado localmente. Descargando..."
  )

  data_sf <- tryCatch(
    {
      jsonlite::fromJSON(full_url, simplifyVector = TRUE)
    },
    error = function(e) {
      stop(
        "La descarga y lectura del archivo fall\u00f3.\n  URL: ",
        full_url,
        "\n  Error: ",
        e$message
      )
    }
  )

  # 3. GUARDAR EL OBJETO PROCESADO EN CACH\u00c9
  pins::pin_write(
    board = local_board,
    x = data_sf,
    name = id,
    type = "rds",
    title = paste("Datos para", id)
  )
  message("Pin '", id, "' guardado en cach\u00e9 para uso futuro.")

  return(data_sf)
}
  