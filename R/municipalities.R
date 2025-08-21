# Evitar NOTEs de variables globales en dplyr
utils::globalVariables(c(
  "MUN_ID", "MUN_NAME", "MUN_NAME_CLEAN", "MUN_NAME_OFFICIAL", "distance"
))

# R/municipalities.R

#' Obtener Límites de los Municipios de la República Dominicana
#'
#' Descarga (si es necesario) y carga los límites de los municipios de la
#' República Dominicana, según la division correspondiente al id suministrado.
#'
#' Las divisiones disponibles son:
#'
#' - `"RD_MUN158"` (por defecto): 158 Municipios de la República Dominicana.
#' - `"RD_MUN155"`: TODO
#'
#' @param id Nombre del archivo de datos en el servidor remoto.
#' @param sf Lógico. Si es `FALSE`, devuelve un data.frame regular sin la
#'   columna de geometría. Por defecto es `TRUE`.
#'
#' @return Un objeto de la clase `sf` o un `data.frame`.
#' @export
#' @importFrom sf st_drop_geometry
#' @examples
#' \dontrun{
#' # Cargar el objeto sf completo
#' municipios_sf <- gd_municipalities()
#'
#' # Cargar solo la tabla de atributos (sin geometría)
#' municipios_df <- gd_municipalities(sf = FALSE)
#' }
gd_municipalities <- function(id = "RD_MUN158", sf = TRUE) {
  data_sf <- fetch_and_cache(id = id)

  if (!sf) {
    data_sf <- sf::st_drop_geometry(data_sf)
  }

  return(data_sf)
}

# Helper function for municipality aliases
.get_municipios_alias <- function() {
  raw_data <- gd_get_dataset(id = "municipios_alias", verbose = FALSE)
  
  # Extract the data portion from the JSON structure
  if (is.list(raw_data) && "data" %in% names(raw_data)) {
    return(raw_data$data)
  } else {
    # If it's already in the expected format, return as is
    return(raw_data)
  }
}

# Helper function for robust municipality name cleaning
.do_municipality_names_cleaning <- function(names, alias_data, .tol = 0.25, .on_error = "fail") {
  # Handle special cases
  names <- ifelse(is.na(names), "_NA_", as.character(names))

  # Clean input names
  names_clean <- .text_cleaning(names)

  # Create a unique lookup table by getting the official name for each municipality
  # Official names should be the first occurrence for each MUN_ID
  official_names <- alias_data %>%
    dplyr::arrange(MUN_ID, MUN_NAME) %>%
    dplyr::group_by(MUN_ID) %>%
    dplyr::slice(1) %>%
    dplyr::ungroup() %>%
    dplyr::select(MUN_ID, MUN_NAME_OFFICIAL = MUN_NAME)

  # Create lookup table with all aliases pointing to official names
  alias_lookup <- alias_data %>%
    dplyr::left_join(official_names, by = "MUN_ID") %>%
    dplyr::mutate(
      MUN_NAME_CLEAN = .text_cleaning(MUN_NAME)
    ) %>%
    dplyr::select(MUN_NAME_CLEAN, MUN_NAME_OFFICIAL) %>%
    dplyr::distinct()

  # Process each input name individually
  results <- character(length(names))

  for (i in seq_along(names)) {
    current_name <- names[i]
    current_clean <- names_clean[i]

    # Handle NA case
    if (current_clean == "_na_") {
      results[i] <- "_NA_"
      next
    }

    # Try exact match first
    exact_match <- alias_lookup %>%
      dplyr::filter(MUN_NAME_CLEAN == current_clean)

    if (nrow(exact_match) > 0) {
      results[i] <- exact_match$MUN_NAME_OFFICIAL[1]
      next
    }

    # Remove common prefixes for better matching
    current_clean_no_prefix <- gsub("^(municipio|mun)\\s+", "", current_clean, ignore.case = TRUE)
    if (current_clean_no_prefix != current_clean) {
      exact_match_no_prefix <- alias_lookup %>%
        dplyr::filter(MUN_NAME_CLEAN == current_clean_no_prefix)

      if (nrow(exact_match_no_prefix) > 0) {
        results[i] <- exact_match_no_prefix$MUN_NAME_OFFICIAL[1]
        next
      }
    }

    # Try partial/prefix matching (input is part of alias)
    prefix_matches <- alias_lookup %>%
      dplyr::filter(startsWith(MUN_NAME_CLEAN, current_clean_no_prefix)) %>%
      dplyr::arrange(nchar(MUN_NAME_CLEAN))

    if (nrow(prefix_matches) > 0) {
      results[i] <- prefix_matches$MUN_NAME_OFFICIAL[1]
      next
    }

    # Try reverse prefix matching (alias is part of input)
    reverse_prefix_matches <- alias_lookup %>%
      dplyr::filter(startsWith(current_clean_no_prefix, MUN_NAME_CLEAN)) %>%
      dplyr::arrange(dplyr::desc(nchar(MUN_NAME_CLEAN)))

    if (nrow(reverse_prefix_matches) > 0) {
      results[i] <- reverse_prefix_matches$MUN_NAME_OFFICIAL[1]
      next
    }

    # Fuzzy matching as last resort
    alias_with_distances <- alias_lookup %>%
      dplyr::filter(MUN_NAME_CLEAN != "_na_") %>%
      dplyr::mutate(
        distance = stringdist::stringdist(current_clean_no_prefix, MUN_NAME_CLEAN, method = "jw")
      ) %>%
      dplyr::arrange(distance, nchar(MUN_NAME_CLEAN))

    if (nrow(alias_with_distances) > 0) {
      best_match <- alias_with_distances[1, ]

      # Apply tolerance check BEFORE assigning result
      # Note: Jaro-Winkler distance is already normalized (0-1), lower is better
      if (best_match$distance <= .tol) {
        results[i] <- best_match$MUN_NAME_OFFICIAL
      } else {
        # Handle error cases - name doesn't match within tolerance
        if (.on_error == "na") {
          results[i] <- NA_character_
        } else if (.on_error == "omit") {
          results[i] <- current_name
        } else if (.on_error == "fail") {
          cli::cli_abort(
            c(
              "x" = paste0("Municipality name '", current_name, "' could not be matched with tolerance ", .tol),
              "i" = paste0("Best match was '", best_match$MUN_NAME_OFFICIAL, "' with distance ", round(best_match$distance, 3)),
              "i" = "Consider increasing .tol or using .on_error = 'na' or 'omit'"
            )
          )
        }
      }
    } else {
      # No matches at all - this should rarely happen
      if (.on_error == "na") {
        results[i] <- NA_character_
      } else if (.on_error == "omit") {
        results[i] <- current_name
      } else if (.on_error == "fail") {
        cli::cli_abort("Municipality name '{current_name}' could not be matched to any known municipality")
      }
    }
  }

  return(results)
}

#' Limpia y estandariza los nombres de municipios de la República Dominicana
#'
#' Esta función limpia y estandariza los nombres de los municipios en la República Dominicana,
#' con tolerancia para la similitud de cadenas y opciones para el manejo de errores. Utiliza
#' el sistema oficial de códigos municipales de la República Dominicana.
#'
#' @param mun Vector de caracteres con los nombres de municipios a limpiar.
#' @param .tol Nivel de tolerancia numérica para la similitud de cadenas. Por defecto es 0.25.
#' Este parámetro controla cuán similares deben ser dos cadenas para considerarse una coincidencia.
#' Un valor más bajo significa una coincidencia más estricta.
#' @param .on_error Cadena de caracteres que especifica el método de manejo de errores. Por defecto es "fail".
#' Puede ser uno de los siguientes: "fail" para detener la ejecución en caso de error,
#' "omit" para ignorar los nombres no coincidentes, o "na" para devolver NA en los nombres no coincidentes.
#'
#' @return Un vector de caracteres con los nombres de municipios limpiados.
#' @export
#'
#' @examples
#' \dontrun{
#' # Uso básico con nombres de municipios
#' nombres_mun_limpios <- gd_clean_municipality_name(c("santiago", "moca", "bonao"))
#'
#' # Con variantes de prefijos
#' gd_clean_municipality_name(c("Municipio de Santiago", "Mun. Moca"))
#'
#' # Con tolerancia y manejo de errores
#' gd_clean_municipality_name("santiagooo", .tol = 0.8, .on_error = "na")
#' }
gd_clean_municipality_name <- function(mun, .tol = 0.25, .on_error = "fail") {
  # Parameter validation
  if (!is.numeric(.tol) || length(.tol) != 1 || .tol < 0 || .tol > 1) {
    cli::cli_abort(".tol debe ser un número entre 0 y 1")
  }
  
  if (!.on_error %in% c("fail", "na", "omit")) {
    cli::cli_abort(".on_error debe ser uno de: 'fail', 'na', 'omit'")
  }
  
  # Get alias data
  alias_data <- .get_municipios_alias()
  
  # Use the robust cleaning function
  .do_municipality_names_cleaning(mun, alias_data, .tol, .on_error)
}
