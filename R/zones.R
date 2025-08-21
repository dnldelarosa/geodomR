# Evitar NOTEs de variables globales en dplyr
utils::globalVariables(c(
  'ZONE_ID', 'ZONE_CODE', 'ZONE_NAME', 'TOPONIMIA'
))

#' Obtener Datos de Zonas de Residencia
#'
#' Proporciona datos sobre zonas de residencia (urbana/rural) en la República Dominicana.
#' Esta función crea un dataset estándar con las zonas de residencia para uso en análisis
#' y estandarización de datos.
#'
#' @param sf Lógico. Si es `FALSE`, devuelve un data.frame regular.
#'   Por defecto es `TRUE` (aunque no hay geometría para zones).
#'
#' @return Un data.frame con información de zonas de residencia.
#' @export
#' @examples
#' \dontrun{
#'   # Cargar datos de zonas
#'   zones_df <- gd_zones()
#'
#'   # Lo mismo (sf no aplica para zones)
#'   zones_df <- gd_zones(sf = FALSE)
#' }
gd_zones <- function(sf = TRUE) {
  # Dataset base con zonas de residencia estándar
  zones_data <- data.frame(
    ZONE_ID = c("01", "02"),
    ZONE_CODE = c("URB", "RUR"),
    ZONE_NAME = c("Urbana", "Rural"),
    TOPONIMIA = c("Urbana", "Rural"),
    stringsAsFactors = FALSE
  )
  
  return(zones_data)
}

# Helper function for zones aliases
.get_zones_alias <- function() {
  # Use standard geodomR function to get datasets
  datos <- gd_get_dataset(id = "zones_alias")
  return(datos$data)
}

#' Limpiar y estandarizar nombres de zonas de residencia
#'
#' @param names Vector de nombres de zonas a limpiar
#' @param .tol Tolerancia para coincidencias fuzzy (0 a 1, por defecto 0.25)
#' @param .on_error Acción en caso de error: "fail", "na" o "omit" (por defecto "fail")
#' @return Vector de nombres de zonas estandarizados
#' @export 
#' @examples
#' \dontrun{
#'   # Limpiar nombres de zonas
#'   clean_zones <- gd_clean_zone_name(c("zona urbana", "campo", "ciudad"))
#'   print(clean_zones)  # "Urbana" "Rural" "Urbana"
#' }
gd_clean_zone_name <- function(names, .tol = 0.25, .on_error = "fail") {
  # Parameter validation
  if (!is.numeric(.tol) || length(.tol) != 1 || .tol < 0 || .tol > 1) {
    cli::cli_abort(".tol debe ser un número entre 0 y 1")
  }
  
  if (!.on_error %in% c("fail", "na", "omit")) {
    cli::cli_abort(".on_error debe ser uno de: 'fail', 'na', 'omit'")
  }
  
  # Obtener datos de zonas y alias
  zones_data <- gd_zones(sf = FALSE)
  alias_data <- .get_zones_alias()
  
  # Usar función similar a la de provincias y regiones
  .do_zone_names_cleaning_robust(names, alias_data, zones_data, .tol, .on_error)
}

# Helper function for robust zone name cleaning
.do_zone_names_cleaning_robust <- function(names, alias_data, zones_data, .tol = 0.25, .on_error = "fail") {
  # Validación de entrada
  if (length(names) == 0) {
    return(character(0))
  }
  
  # Limpiar los nombres de entrada
  names_clean <- .text_cleaning(names)
  
  # Inicializar vector de resultados con la misma longitud que la entrada
  result <- character(length(names))
  
  # Crear lista de todos los nombres de referencia posibles
  reference_names <- c()
  if (!is.null(alias_data) && nrow(alias_data) > 0) {
    reference_names <- c(reference_names, alias_data$ZONE_NAME)
  }
  reference_names <- c(reference_names, zones_data$TOPONIMIA)
  reference_names <- unique(reference_names[!is.na(reference_names)])
  
  # Procesar cada nombre individualmente para preservar la longitud del vector
  for (i in seq_along(names)) {
    current_name <- names[i]
    current_clean <- names_clean[i]
    
    if (current_clean == "_na_") {
      result[i] <- NA_character_
      next
    }
    
    # Buscar coincidencia exacta en alias primero
    found_exact <- FALSE
    if (!is.null(alias_data) && nrow(alias_data) > 0) {
      for (j in seq_len(nrow(alias_data))) {
        if (.text_cleaning(alias_data$ZONE_NAME[j]) == current_clean) {
          result[i] <- alias_data$TOPONIMIA[j]
          found_exact <- TRUE
          break
        }
      }
    }
    
    if (found_exact) next
    
    # Buscar coincidencia exacta en nombres oficiales
    for (j in seq_len(nrow(zones_data))) {
      if (.text_cleaning(zones_data$TOPONIMIA[j]) == current_clean) {
        result[i] <- zones_data$TOPONIMIA[j]
        found_exact <- TRUE
        break
      }
    }
    
    if (found_exact) next
    
    # Si no hay coincidencia exacta, usar fuzzy matching
    if (length(reference_names) > 0) {
      # Calcular distancias con todos los nombres de referencia
      best_distance <- Inf
      best_match <- NA_character_
      
      for (ref_name in reference_names) {
        ref_clean <- .text_cleaning(ref_name)
        distance <- stringdist::stringdist(current_clean, ref_clean, method = "lv")
        distance_norm <- distance / max(nchar(current_clean), nchar(ref_clean))
        
        if (distance_norm < best_distance) {
          best_distance <- distance_norm
          best_match <- ref_name
        }
      }
      
      # Mapear el nombre de referencia al nombre oficial
      if (!is.na(best_match)) {
        official_name <- best_match
        if (!is.null(alias_data) && best_match %in% alias_data$ZONE_NAME) {
          alias_idx <- which(alias_data$ZONE_NAME == best_match)[1]
          official_name <- alias_data$TOPONIMIA[alias_idx]
        }
        
        if (best_distance <= .tol) {
          result[i] <- official_name
        } else {
          # Manejar casos que exceden tolerancia
          if (.on_error == "fail") {
            cli::cli_abort(
              c(
                "x" = "Nombre de zona no pudo emparejarse con la tolerancia especificada:",
                " " = paste0("'", current_name, "' -> '", official_name, "' (tolerancia: ", round(best_distance, 3), ")"),
                "i" = "Considera aumentar .tol o usar .on_error = 'na' o 'omit'"
              )
            )
          } else if (.on_error == "na") {
            result[i] <- NA_character_
          } else if (.on_error == "omit") {
            result[i] <- current_name
          } else {
            result[i] <- official_name
          }
        }
      } else {
        # Sin candidatos
        if (.on_error == "fail") {
          cli::cli_abort(
            c(
              "x" = "No se encontraron candidatos para el nombre de zona:",
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
    } else {
      # Sin nombres de referencia
      if (.on_error == "fail") {
        cli::cli_abort("No hay nombres de referencia disponibles para zonas")
      } else if (.on_error == "na") {
        result[i] <- NA_character_
      } else if (.on_error == "omit") {
        result[i] <- current_name
      }
    }
  }
  
  return(result)
}
