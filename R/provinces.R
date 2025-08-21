# R/provinces.R

#' Obtener Límites de las Provincias de la República Dominicana
#'
#' Descarga (si es necesario) y carga los límites de las provincias de la
#' República Dominicana. Permite obtener los datos como objeto `sf` o como
#' `data.frame` sin geometría. Además, puede agregar la columna de región
#' administrativa según la Ley 345-22 si se especifica el argumento `.reg = "rup"`.
#'
#' @param id Nombre del archivo de datos en el servidor remoto. Por defecto es `"RD_PROV"`.
#' @param sf Lógico. Si es `FALSE`, devuelve un `data.frame` regular sin la
#'   columna de geometría. Por defecto es `TRUE`.
#' @param .reg Caracter. Si es `"rup"`, agrega la columna de región administrativa
#'   según la Ley 345-22. Por defecto es `NULL`.
#' @param verbose Lógico. Si es `TRUE`, muestra mensajes de progreso durante la 
#' descarga y procesamiento de datos. Por defecto es `FALSE`.
#'
#' @return Un objeto de la clase `sf` o un `data.frame`, dependiendo del valor de `sf`.
#'         Si `.reg = "rup"`, incluye la columna de región administrativa.
#' @export
#' @importFrom sf st_drop_geometry
#' @examples
#' \dontrun{
#'   # Cargar el objeto sf completo
#'   provincias_sf <- gd_provinces()
#'
#'   # Cargar solo la tabla de atributos (sin geometría)
#'   provincias_df <- gd_provinces(sf = FALSE)
#'
#'   # Cargar provincias con columna de región administrativa
#'   provincias_reg <- gd_provinces(.reg = "rup")
#' }
gd_provinces <- function(id = "RD_PROV", sf = TRUE, .reg = NULL, verbose = FALSE) {
  PROV_CODE <- NULL  # Evitar el warning de no utilizado
  REG_CODE <- NULL   # Evitar el warning de no utilizado

  if (verbose) message("Descargando y cargando límites de provincias...")

  data_sf <- fetch_and_cache(id = id, verbose = verbose)

  if (!sf) {
    if (verbose) message("Eliminando geometría, devolviendo data.frame...")
    data_sf <- sf::st_drop_geometry(data_sf)
  }

  if (!is.null(.reg)) {
    if (.reg == tolower('rup')) {
      if (verbose) message("Agregando columna de región administrativa (Ley 345-22)...")
      .datos <- gd_get_dataset(id = "division_territorial_rd_ley_345_22", verbose = verbose)
      .datos[['data']] |>
        dplyr::select(PROV = PROV_CODE, REG_CODE) |>
        dplyr::distinct() |>
        dplyr::left_join(data_sf, by = c("PROV")) -> data_sf
    }
  }

  if (verbose) message("Finalizado.")
  return(data_sf)
}

# Función auxiliar para obtener el dataset de provincias_alias
.get_provincias_alias <- function() {
  tryCatch({
    # Usar la función estándar de geodomR para obtener datasets
    datos <- gd_get_dataset(id = "provincias_alias", verbose = FALSE)
    return(datos$data)
  }, error = function(e) {
    # Si falla, usar los datos básicos de gd_provinces
    prov_data <- gd_provinces(sf = FALSE)
    # Convertir a formato compatible con el dataset de alias
    prov_alias <- data.frame(
      PROV_ID = sprintf("%02d", as.numeric(rownames(prov_data))),
      PROV_CODE = prov_data$PROV_CODE,
      PROV_NAME = prov_data$PROV_NAME,
      stringsAsFactors = FALSE
    )
    return(prov_alias)
  })
}

#' Limpiar nombres de provincias de República Dominicana
#' 
#' Esta función limpia y estandariza los nombres de las provincias de la 
#' República Dominicana, utilizando un dataset de alias para mayor robustez
#' en el emparejamiento de nombres.
#'
#' @param prov Vector de caracteres con nombres de provincias a limpiar
#' @param .tol Nivel de tolerancia numérico para similitud de cadenas. Por defecto 0.25.
#' @param .on_error Método de manejo de errores. Por defecto "fail". 
#'   Puede ser "fail" para detener la ejecución, "omit" para ignorar nombres 
#'   no emparejados, o "na" para devolver NA en nombres no emparejados.
#'
#' @return Vector de caracteres con nombres de provincias limpiados
#' @export
#'
#' @examples
#' \dontrun{
#'   # Uso básico
#'   provincias_limpias <- gd_clean_prov_name(c("azua", "barahona", "stgo"))
#'   
#'   # Con mayor tolerancia
#'   provincias_limpias <- gd_clean_prov_name(c("azua", "barahona"), .tol = 0.5)
#' }
gd_clean_prov_name <- function(prov, .tol = 0.25, .on_error = "fail") {
  # Obtener dataset de alias
  alias_data <- .get_provincias_alias()
  
  # Usar la función de limpieza de geodomR adaptada para provincias
  .do_prov_names_cleaning_robust(prov, alias_data, .tol, .on_error)
}





# Helper function for robust province name cleaning
.do_prov_names_cleaning_robust <- function(names, alias_data, .tol = 0.25, .on_error = "fail") {
  # Validación de entrada
  if (length(names) == 0) {
    return(character(0))
  }
  
  # Limpiar los nombres de entrada
  names_clean <- .text_cleaning(names)
  
  # Crear un dataframe con todas las variantes de alias
  alias_clean <- alias_data %>%
    dplyr::mutate(
      PROV_NAME_CLEAN = .text_cleaning(PROV_NAME)
    ) %>%
    # Remover duplicados manteniendo el nombre oficial (el primero en orden alfabético)
    dplyr::arrange(PROV_ID, PROV_NAME) %>%
    dplyr::group_by(PROV_ID) %>%
    dplyr::mutate(
      PROV_NAME_OFFICIAL = dplyr::first(PROV_NAME)
    ) %>%
    dplyr::ungroup()
  
  # Inicializar vector de resultados con la misma longitud que la entrada
  result <- character(length(names))
  
  # Procesar cada nombre individualmente para preservar la longitud del vector
  for (i in seq_along(names)) {
    current_name <- names[i]
    current_clean <- names_clean[i]
    
    # Buscar coincidencia exacta primero
    exact_match <- alias_clean %>%
      dplyr::filter(PROV_NAME_CLEAN == current_clean) %>%
      dplyr::slice(1)
    
    if (nrow(exact_match) > 0) {
      result[i] <- exact_match$PROV_NAME_OFFICIAL
      next
    }
    
    # Si no hay coincidencia exacta, buscar por prefijo
    prefix_candidates <- alias_clean %>%
      dplyr::filter(PROV_NAME_CLEAN != "_na_") %>%
      dplyr::filter(startsWith(PROV_NAME_CLEAN, current_clean)) %>%
      dplyr::arrange(nchar(PROV_NAME_CLEAN))
    
    if (nrow(prefix_candidates) > 0) {
      result[i] <- prefix_candidates$PROV_NAME_OFFICIAL[1]
      next
    }
    
    # Si no hay prefix match, usar fuzzy matching
    distances <- alias_clean %>%
      dplyr::filter(PROV_NAME_CLEAN != "_na_") %>%
      dplyr::mutate(
        distance = stringdist::stringdist(current_clean, PROV_NAME_CLEAN, method = "lv"),
        distance_norm = pmax(distance / nchar(current_clean), distance / nchar(PROV_NAME_CLEAN)),
        starts_with_input = ifelse(startsWith(PROV_NAME_CLEAN, current_clean), 0.2, 0),
        input_starts_with_name = ifelse(startsWith(current_clean, PROV_NAME_CLEAN), 0.1, 0),
        abbreviation_bonus = 0,
        length_penalty = ifelse(nchar(PROV_NAME_CLEAN) > nchar(current_clean) * 2, 0.05, 0),
        total_score = distance_norm - starts_with_input - input_starts_with_name - abbreviation_bonus + length_penalty
      ) %>%
      dplyr::arrange(total_score, nchar(PROV_NAME_CLEAN))
    
    if (nrow(distances) > 0) {
      best_match <- distances[1, ]
      
      if (best_match$total_score <= .tol) {
        result[i] <- best_match$PROV_NAME_OFFICIAL
      } else {
        # Manejar casos que exceden tolerancia
        if (.on_error == "fail") {
          cli::cli_abort(
            c(
              "x" = "Nombre de provincia no pudo emparejarse con la tolerancia especificada:",
              " " = paste0("'", current_name, "' -> '", best_match$PROV_NAME_OFFICIAL, "' (tolerancia: ", round(best_match$total_score, 3), ")"),
              "i" = "Considera aumentar .tol o usar .on_error = 'na' o 'omit'"
            )
          )
        } else if (.on_error == "na") {
          result[i] <- NA_character_
        } else if (.on_error == "omit") {
          result[i] <- current_name
        } else {
          result[i] <- best_match$PROV_NAME_OFFICIAL
        }
      }
    } else {
      # Sin candidatos
      if (.on_error == "fail") {
        cli::cli_abort(
          c(
            "x" = "No se encontraron candidatos para el nombre de provincia:",
            " " = paste0("'", current_name, "'"),
            "i" = "Verifica que el nombre esté bien escrito"
          )
        )
      } else if (.on_error == "na") {
        result[i] <- NA_character_
      } else if (.on_error == "omit") {
        result[i] <- current_name
      }
    }
  }
  
  return(result)
}
