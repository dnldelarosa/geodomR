# Evitar NOTEs de variables globales en dplyr
utils::globalVariables(c(
  'PROV_ID', 'PROV_NAME', 'PROV_NAME_CLEAN', 'PROV_NAME_OFFICIAL', 'input_name', 'input_clean', '.',
  'distance', 'distance_norm', 'starts_with_input', 'input_starts_with_name', 'abbreviation_bonus',
  'length_penalty', 'total_score', 'match_type', 'reference_name', 'reference_clean', 'REGION_NAME_OFFICIAL',
  'ZONE_ID', 'ZONE_NAME', 'ZONE_NAME_OFFICIAL'
))
# Funciones de limpieza y normalización de nombres administrativos

# TODO: Implementar funciones de limpieza para otros niveles administrativos:
# TODO: - gd_clean_municipality_name() y .do_municipality_names_cleaning()
# TODO: - gd_clean_dm_name() y .do_dm_names_cleaning()
# TODO: - gd_clean_section_name() y .do_section_names_cleaning()  
# TODO: - gd_clean_bparaje_name() y .do_bparaje_names_cleaning()
# TODO: Estas funciones mejorarán la detección automática y la estandarización de datos

# Esta función replica la funcionalidad de .text_cleaning de rgisDR
# para su uso en geodomR. No se han realizado mejoras ni refactorizaciones.



.text_cleaning <- function(names) {
  # Asegura que el input sea character
  names <- as.character(names)
  # Reemplaza NA por '_na_' (con guiones bajos)
  names <- tidyr::replace_na(names, "_na_")
  # Limpieza básica
  names <- stringr::str_to_lower(names)
  names <- stringr::str_squish(names)
  # Normaliza tildes antes de eliminar prefijos
  names <- chartr("áéíóúüñ", "aeiouun", names)
  # Elimina palabras irrelevantes
  names <- stringr::str_remove(names, stringr::regex("^region[ ]?", ignore_case = TRUE))
  names <- stringr::str_remove(names, stringr::regex("^provincia[ ]?de[ ]?", ignore_case = TRUE))
  names <- stringr::str_remove(names, stringr::regex("^provincia[ ]?", ignore_case = TRUE))
  names <- stringr::str_remove(names, stringr::regex("^municipio[ ]?", ignore_case = TRUE))
  names <- stringr::str_remove(names, stringr::regex("^ayuntamiento[ ]?de[ ]?", ignore_case = TRUE))
  names <- stringr::str_remove(names, stringr::regex(" \\(d[.]?[ ]?m[.]?\\)", ignore_case = TRUE))
  names <- stringr::str_remove(names, stringr::regex(" [(]?zona urbana[)]?", ignore_case = TRUE))
  # Elimina artículos y preposiciones solo si están al inicio
  names <- stringr::str_remove(names, stringr::regex("^el[ ]", ignore_case = TRUE))
  names <- stringr::str_remove(names, stringr::regex("^la[s]?[ ]?", ignore_case = TRUE))
  names <- stringr::str_remove(names, stringr::regex("^los[ ]?", ignore_case = TRUE))
  names <- stringr::str_remove(names, stringr::regex("^de[l]?[ ]?", ignore_case = TRUE))
  # Elimina 'de' como palabra completa en cualquier posición
  names <- stringr::str_replace_all(names, stringr::regex("\\bde\\b", ignore_case = TRUE), "")
  names <- stringr::str_squish(names)
  # Solo elimina caracteres especiales si no es '_na_'
  names <- ifelse(names == "_na_", names, stringr::str_remove_all(names, stringr::regex("[^0-9a-z ]", ignore_case = TRUE)))
  names <- stringr::str_squish(names)
  names
}

# Ejemplo de uso:
# .text_cleaning(c("Provincia de Azúa", NA, "Ayuntamiento de Santo Domingo (Zona urbana)"))

# Función robusta de limpieza de nombres de provincias (reemplaza versión problemática)
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



# Función robusta de limpieza de nombres de zonas (reemplaza versión problemática)
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


