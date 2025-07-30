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

#' Verifica si un archivo remoto ha cambiado comparando metadatos
#' 
#' Esta función hace una petición HEAD al servidor para obtener ETag o Last-Modified
#' y los compara con los metadatos almacenados en el pin local.
#' 
#' @param url URL del archivo remoto
#' @param pin_name Nombre del pin en el caché local
#' @param board Board de pins donde está almacenado el caché
#' @param verbose Logical. Si TRUE, muestra mensajes informativos. Default TRUE.
#' @return TRUE si el archivo ha cambiado o no se puede determinar, FALSE si no ha cambiado
#' @noRd
check_remote_file_changed <- function(url, pin_name, board, verbose = TRUE) {
  tryCatch({
    # Verificar si tenemos metadatos del pin local
    if (!pins::pin_exists(board, pin_name)) {
      return(TRUE)  # No existe localmente, necesita descarga
    }

    # Obtener metadatos del pin local
    local_meta <- pins::pin_meta(board, pin_name)
    local_etag <- tryCatch(local_meta$user$etag, error = function(e) NULL)
    local_last_modified <- tryCatch(local_meta$user$last_modified, error = function(e) NULL)
    local_created <- tryCatch(local_meta$created, error = function(e) NULL)

    # Verificar disponibilidad de httr2
    if (!requireNamespace("httr2", quietly = TRUE)) {
      if (verbose) message("httr2 no disponible, usando caché local")
      return(FALSE)  # Sin httr2, asumir que no cambió (usar caché)
    }

    # Hacer petición HEAD para obtener metadatos actuales
    resp <- tryCatch({
      httr2::request(url) |> 
        httr2::req_method("HEAD") |> 
        httr2::req_perform()
    }, error = function(e) {
      # Muchos servidores no soportan HEAD, usar caché en ese caso
      return(NULL)
    })
    
    if (is.null(resp)) {
      # Si falla la petición HEAD (común en CDNs), usar caché por defecto
      # Solo forzar descarga si el pin es muy antiguo (> 24 horas)
      if (!is.null(local_created)) {
        hours_old <- as.numeric(difftime(Sys.time(), local_created, units = "hours"))
        if (hours_old > 24) {
          if (verbose) message("Pin muy antiguo (", round(hours_old, 1), " horas), forzando verificación")
          return(TRUE)
        }
      }
      return(FALSE)  # Usar caché si es reciente
    }

    remote_etag <- httr2::resp_header(resp, "etag")
    remote_last_modified <- httr2::resp_header(resp, "last-modified")

    # Comparar ETag si está disponible (más confiable)
    if (!is.null(local_etag) && !is.null(remote_etag)) {
      return(local_etag != remote_etag)
    }

    # Comparar Last-Modified si ambos existen
    if (!is.null(local_last_modified) && !is.null(remote_last_modified)) {
      return(local_last_modified != remote_last_modified)
    }

    # Si no hay etag ni last_modified, comparar con fecha de creación local
    if (!is.null(local_created) && !is.null(remote_last_modified)) {
      # Convertir fecha remota a POSIXct
      remote_time <- tryCatch({
        as.POSIXct(remote_last_modified, tz = "UTC", tryFormats = c(
          "%a, %d %b %Y %H:%M:%S %Z", 
          "%a, %d %b %Y %H:%M:%S GMT",
          "%Y-%m-%d %H:%M:%S"
        ))
      }, error = function(e) NA)
      
      if (!is.na(remote_time)) {
        # Dar margen de error de 2 minutos para diferencias menores
        time_diff <- abs(as.numeric(difftime(local_created, remote_time, units = "mins")))
        return(time_diff > 2)  # Solo considerar cambio si hay más de 2 minutos de diferencia
      }
    }

    # Si no podemos verificar, usar caché (ser conservador)
    return(FALSE)

  }, error = function(e) {
    # En caso de error, usar caché (ser conservador)
    if (verbose) message("Error verificando cambios remotos, usando caché local")
    return(FALSE)
  })
}

#' Descarga, procesa y cachea un archivo de datos espaciales
#'
#' Esta funci\u00f3n es el motor de datos del paquete.
#' 1. Comprueba si una versi\u00f3n procesada (.json) del dato existe en el cach\u00e9 local.
#' 2. Verifica si el archivo remoto ha cambiado desde la última descarga.
#' 3. Si existe y no ha cambiado, la lee y la devuelve (muy r\u00e1pido).
#' 4. Si no existe o ha cambiado, construye la URL completa, descarga el archivo fuente,
#'    lo lee como un objeto sf, y lo guarda como un pin .json en el caché para el futuro.
#'
#' @param id El nombre del archivo en el servidor remoto (ej. "RD_MREG").
#' @param force_download Logical. Si TRUE, fuerza la descarga ignorando el caché. Default FALSE.
#' @param verbose Logical. Si TRUE, muestra mensajes informativos. Default TRUE.
#' @param ... Argumentos adicionales para sf::st_read().
#' @return Un objeto sf.
#' @noRd
fetch_and_cache <- function(id, force_download = FALSE, verbose = TRUE, ...) {
  # browser()
  local_board <- get_geodom_cache()
  base_url <- get_base_data_url()
  full_url <- paste0(base_url, 'TopoJSON/', id, ".json")

  # 1. VERIFICAR CACHÉ Y SI EL ARCHIVO REMOTO HA CAMBIADO
  if (!force_download && 
      pins::pin_exists(local_board, id) && 
      !check_remote_file_changed(full_url, id, local_board, verbose)) {
    if (verbose) message("Cargando '", id, "' desde cach\u00e9 local (sin cambios remotos).")
    return(sf::st_as_sf(pins::pin_read(local_board, id)))
  }

  # 2. DESCARGAR (NUEVO O ACTUALIZADO)
  if (verbose) {
    if (pins::pin_exists(local_board, id)) {
      message("Pin '", id, "' encontrado pero el archivo remoto ha cambiado. Actualizando...")
    } else {
      message("Pin '", id, "' no encontrado localmente. Descargando...")
    }
  }

  # Obtener metadatos del archivo remoto antes de descargar
  remote_meta <- tryCatch({
    if (requireNamespace("httr2", quietly = TRUE)) {
      resp <- httr2::request(full_url) |> 
        httr2::req_method("HEAD") |> 
        httr2::req_perform()
      
      list(
        etag = httr2::resp_header(resp, "etag"),
        last_modified = httr2::resp_header(resp, "last-modified")
      )
    } else {
      list(etag = NULL, last_modified = NULL)
    }
  }, error = function(e) {
    list(etag = NULL, last_modified = NULL)
  })

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

  # 3. GUARDAR EL OBJETO PROCESADO EN CACHÉ CON METADATOS
  pins::pin_write(
    board = local_board,
    x = data_sf,
    name = id,
    type = "rds",
    title = paste("Datos para", id),
    metadata = list(
      etag = remote_meta$etag,
      last_modified = remote_meta$last_modified,
      download_time = Sys.time()
    )
  )
  if (verbose) message("Pin '", id, "' guardado en cach\u00e9 para uso futuro.")

  return(data_sf)
}

gd_get_dataset <- function(id, force_download = FALSE, verbose = TRUE, ...) {
  # browser()
  local_board <- get_geodom_cache()
  base_url <- get_base_data_url()
  full_url <- paste0(base_url, 'datasets/', id, ".json")

  # 1. VERIFICAR CACHÉ Y SI EL ARCHIVO REMOTO HA CAMBIADO
  if (!force_download && 
      pins::pin_exists(local_board, id) && 
      !check_remote_file_changed(full_url, id, local_board, verbose)) {
    if (verbose) message("Cargando '", id, "' desde cach\u00e9 local (sin cambios remotos).")
    return(pins::pin_read(local_board, id))
  }

  # 2. DESCARGAR (NUEVO O ACTUALIZADO)
  if (verbose) {
    if (pins::pin_exists(local_board, id)) {
      message("Pin '", id, "' encontrado pero el archivo remoto ha cambiado. Actualizando...")
    } else {
      message("Pin '", id, "' no encontrado localmente. Descargando...")
    }
  }

  # Obtener metadatos del archivo remoto antes de descargar
  remote_meta <- tryCatch({
    if (requireNamespace("httr2", quietly = TRUE)) {
      resp <- httr2::request(full_url) |> 
        httr2::req_method("HEAD") |> 
        httr2::req_perform()
      
      list(
        etag = httr2::resp_header(resp, "etag"),
        last_modified = httr2::resp_header(resp, "last-modified")
      )
    } else {
      list(etag = NULL, last_modified = NULL)
    }
  }, error = function(e) {
    list(etag = NULL, last_modified = NULL)
  })

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

  # 3. GUARDAR EL OBJETO PROCESADO EN CACHÉ CON METADATOS
  pins::pin_write(
    board = local_board,
    x = data_sf,
    name = id,
    type = "rds",
    title = paste("Datos para", id),
    metadata = list(
      etag = remote_meta$etag,
      last_modified = remote_meta$last_modified,
      download_time = Sys.time()
    )
  )
  if (verbose) message("Pin '", id, "' guardado en cach\u00e9 para uso futuro.")

  return(data_sf)
}
  