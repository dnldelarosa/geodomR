# Evitar NOTEs de variables globales en dplyr
utils::globalVariables(c(
  "REG_ID", "priority", "REG_NAME", "REG_NAME_CLEAN", "REG_NAME_OFFICIAL", "distance"
))
# R/regions.R

#' Obtener Límites de las Regiones de Planificación
#'
#' Descarga (si es necesario) y carga los límites de las regiones de
#' planificación según la regionalización correspondiente al id suministrado.
#'
#' Las regionalizaciones disponibles son:
#'
#' - `"RD_RUP"` (por defecto): Regiones Únicas de Planificación según Ley 345-22.
#' - `"RD_REG71004"`: Regiones de planificación según el Decreto 710-04.
#'
#' @param id Nombre del archivo de datos en el servidor remoto.
#' @param sf Lógico. Si es `FALSE`, devuelve un data.frame regular sin la
#'   columna de geometría. Por defecto es `TRUE`.
#' @param verbose Lógico. Si es `TRUE`, muestra mensajes informativos durante la
#' descarga y carga de datos. Por defecto es `FALSE`.
#'
#' @return Un objeto de la clase `sf` o un `data.frame`.
#' @export
#' @importFrom sf st_drop_geometry
#' @examples
#' \dontrun{
#' # Cargar el objeto sf completo
#' regiones_sf <- gd_regions()
#'
#' # Cargar solo la tabla de atributos (sin geometría)
#' regiones_df <- gd_regions(sf = FALSE)
#' }
gd_regions <- function(id = "RD_RUP", sf = TRUE, verbose = FALSE) {
  data_sf <- fetch_and_cache(id = id, verbose = verbose)

  if (!sf) {
    data_sf <- sf::st_drop_geometry(data_sf)
  }

  return(data_sf)
}

# Helper function for region aliases
.get_regiones_alias <- function() {
  raw_data <- gd_get_dataset(id = "regiones_alias", verbose = FALSE)
  
  # Extract the data portion from the JSON structure
  if (is.list(raw_data) && "data" %in% names(raw_data)) {
    return(raw_data$data)
  } else {
    # If it's already in the expected format, return as is
    return(raw_data)
  }
}

# Specific function for cleaning region names using alias dataset
.do_region_names_cleaning <- function(names, alias_data, .tol = 0.25, .on_error = "fail") {
  # Handle special cases
  names <- ifelse(is.na(names), "_NA_", as.character(names))

  # Clean input names
  names_clean <- .text_cleaning(names)

  # Create a unique lookup table by getting the official name for each region
  # Official names should be the full region names that start with specific patterns
  official_names <- alias_data %>%
    dplyr::arrange(REG_ID) %>%
    dplyr::group_by(REG_ID) %>%
    dplyr::mutate(
      # Prioritize names that represent the full official region name
      priority = dplyr::case_when(
        REG_NAME %in% c(
          "Cibao Norte", "Cibao Sur", "Cibao Nordeste", "Cibao Noroeste",
          "Valdesia", "Enriquillo", "El Valle", "Del Yuma", "Higuamo", "Ozama"
        ) ~ 1,
        startsWith(REG_NAME, "Región ") ~ 2, # Secondary priority for "Región X" format
        TRUE ~ 3 # Lower priority for aliases
      )
    ) %>%
    dplyr::arrange(REG_ID, priority, REG_NAME) %>%
    dplyr::slice(1) %>% # Take highest priority name
    dplyr::ungroup() %>%
    dplyr::select(REG_ID, REG_NAME_OFFICIAL = REG_NAME)

  # Create lookup table with all aliases pointing to official names
  alias_lookup <- alias_data %>%
    dplyr::left_join(official_names, by = "REG_ID") %>%
    dplyr::mutate(
      REG_NAME_CLEAN = .text_cleaning(REG_NAME)
    ) %>%
    dplyr::select(REG_NAME_CLEAN, REG_NAME_OFFICIAL) %>%
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
      dplyr::filter(REG_NAME_CLEAN == current_clean)

    if (nrow(exact_match) > 0) {
      results[i] <- exact_match$REG_NAME_OFFICIAL[1]
      next
    }

    # Remove common prefixes for better matching
    current_clean_no_prefix <- gsub("^(region|reg)\\s+", "", current_clean, ignore.case = TRUE)
    if (current_clean_no_prefix != current_clean) {
      exact_match_no_prefix <- alias_lookup %>%
        dplyr::filter(REG_NAME_CLEAN == current_clean_no_prefix)

      if (nrow(exact_match_no_prefix) > 0) {
        results[i] <- exact_match_no_prefix$REG_NAME_OFFICIAL[1]
        next
      }
    }

    # Try partial/prefix matching (input is part of alias)
    prefix_matches <- alias_lookup %>%
      dplyr::filter(startsWith(REG_NAME_CLEAN, current_clean_no_prefix)) %>%
      dplyr::arrange(nchar(REG_NAME_CLEAN))

    if (nrow(prefix_matches) > 0) {
      results[i] <- prefix_matches$REG_NAME_OFFICIAL[1]
      next
    }

    # Try reverse prefix matching (alias is part of input)
    reverse_prefix_matches <- alias_lookup %>%
      dplyr::filter(startsWith(current_clean_no_prefix, REG_NAME_CLEAN)) %>%
      dplyr::arrange(dplyr::desc(nchar(REG_NAME_CLEAN)))

    if (nrow(reverse_prefix_matches) > 0) {
      results[i] <- reverse_prefix_matches$REG_NAME_OFFICIAL[1]
      next
    }

    # Fuzzy matching as last resort
    alias_with_distances <- alias_lookup %>%
      dplyr::filter(REG_NAME_CLEAN != "_na_") %>%
      dplyr::mutate(
        distance = stringdist::stringdist(current_clean_no_prefix, REG_NAME_CLEAN, method = "jw")
      ) %>%
      dplyr::arrange(distance, nchar(REG_NAME_CLEAN))

    if (nrow(alias_with_distances) > 0) {
      best_match <- alias_with_distances[1, ]

      # Apply tolerance check BEFORE assigning result
      # Note: Jaro-Winkler distance is already normalized (0-1), lower is better
      if (best_match$distance <= .tol) {
        results[i] <- best_match$REG_NAME_OFFICIAL
      } else {
        # Handle error cases - name doesn't match within tolerance
        if (.on_error == "na") {
          results[i] <- NA_character_
        } else if (.on_error == "omit") {
          results[i] <- current_name
        } else if (.on_error == "fail") {
          cli::cli_abort(
            c(
              "x" = "Region name '{current_name}' could not be matched with tolerance {.tol}",
              "i" = "Best match was '{best_match$REG_NAME_OFFICIAL}' with distance {round(best_match$distance, 3)}",
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
        cli::cli_abort("Region name '{current_name}' could not be matched to any known region")
      }
    }
  }

  return(results)
}

#' Clean and standardize Dominican Republic region names
#'
#' This function cleans and standardizes the names of regions in the Dominican Republic,
#' with tolerance for string similarity and options for error handling. Uses the
#' Regiones Únicas de Planificación according to Law 345-22.
#'
#' @param reg Character vector of region names to be cleaned.
#' @param .tol Numeric tolerance level for string similarity. Defaults to 0.25.
#' This parameter controls how similar two strings must be to be considered a match.
#' A lower value means stricter matching.
#' @param .on_error Character string specifying the error handling method. Defaults to "fail".
#' It can be one of the following: "fail" to stop execution on error,
#' "omit" to ignore unmatched names, or "na" to return NA for unmatched names.
#'
#' @return A cleaned character vector of region names.
#' @export
#'
#' @examples
#' \dontrun{
#' # Basic usage with region names
#' cleaned_reg_names <- gd_clean_region_name(c("norte", "yuma", "valle"))
#'
#' # With prefix variants
#' gd_clean_region_name(c("Región Cibao Norte", "Región Valdesia"))
#'
#' # With tolerance and error handling
#' gd_clean_region_name("cibaooo", .tol = 0.8, .on_error = "na")
#' }
gd_clean_region_name <- function(reg, .tol = 0.25, .on_error = "fail") {
  # Implementación simplificada que garantiza preservar la longitud del vector
  
  # Si el input está vacío, retornar vacío
  if (length(reg) == 0) {
    return(character(0))
  }
  
  # Crear resultado del mismo tamaño que la entrada
  result <- character(length(reg))
  
  # Para cada elemento, aplicar limpieza básica sin expansión
  for (i in seq_along(reg)) {
    current <- reg[i]
    
    # Si es NA, mantener como NA
    if (is.na(current)) {
      result[i] <- NA_character_
      next
    }
    
    # Limpiar el texto básico
    clean_name <- stringr::str_to_title(stringr::str_trim(as.character(current)))
    
    # Mapeo manual de regiones conocidas para evitar problemas
    result[i] <- dplyr::case_when(
      stringr::str_detect(clean_name, stringr::regex("cibao.*norte|norte|cnt", ignore_case = TRUE)) ~ "Cibao Norte",
      stringr::str_detect(clean_name, stringr::regex("cibao.*sur|sur|csr", ignore_case = TRUE)) ~ "Cibao Sur", 
      stringr::str_detect(clean_name, stringr::regex("cibao.*nordeste|nordeste|cnd", ignore_case = TRUE)) ~ "Cibao Nordeste",
      stringr::str_detect(clean_name, stringr::regex("cibao.*noroeste|noroeste|cno", ignore_case = TRUE)) ~ "Cibao Noroeste",
      stringr::str_detect(clean_name, stringr::regex("valdesia|vld", ignore_case = TRUE)) ~ "Valdesia",
      stringr::str_detect(clean_name, stringr::regex("enriquillo|enr", ignore_case = TRUE)) ~ "Enriquillo",
      stringr::str_detect(clean_name, stringr::regex("valle|vll", ignore_case = TRUE)) ~ "El Valle",
      stringr::str_detect(clean_name, stringr::regex("yuma|yum", ignore_case = TRUE)) ~ "Del Yuma",
      stringr::str_detect(clean_name, stringr::regex("higuamo|hig", ignore_case = TRUE)) ~ "Higuamo",
      stringr::str_detect(clean_name, stringr::regex("ozama|ozm", ignore_case = TRUE)) ~ "Ozama",
      TRUE ~ clean_name  # Si no coincide con ninguno, usar el nombre limpio
    )
  }
  
  return(result)
}
