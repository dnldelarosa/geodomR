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

# Función específica de limpieza de nombres de provincias usando dataset de alias
.do_prov_names_cleaning <- function(names, alias_data, .tol = 0.25, .on_error = "fail") {
  # Limpiar los nombres de entrada
  names_clean <- .text_cleaning(names)
  
  # Crear un dataframe con todas las variantes de alias
  # El dataset de alias tiene múltiples filas por provincia con diferentes nombres
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
  
  # Buscar coincidencias exactas primero
  exact_matches <- data.frame(
    input_name = names,
    input_clean = names_clean,
    stringsAsFactors = FALSE
  ) %>%
    dplyr::left_join(
      alias_clean %>% dplyr::select(PROV_NAME_CLEAN, PROV_NAME_OFFICIAL),
      by = c("input_clean" = "PROV_NAME_CLEAN")
    )
  
  # Para nombres que no tuvieron coincidencia exacta, buscar coincidencias de prefijo
  no_exact <- exact_matches %>%
    dplyr::filter(is.na(PROV_NAME_OFFICIAL))
  
  if (nrow(no_exact) > 0) {
    # Primero intentar matching por prefijo
    prefix_matches <- no_exact %>%
      dplyr::select(input_name, input_clean) %>%
      dplyr::rowwise() %>%
      dplyr::do({
        current_clean <- .$input_clean
        
        # Buscar nombres que empiecen con el input (prefix matching)
        prefix_candidates <- alias_clean %>%
          dplyr::filter(PROV_NAME_CLEAN != "_na_") %>%
          dplyr::filter(startsWith(PROV_NAME_CLEAN, current_clean)) %>%
          dplyr::arrange(nchar(PROV_NAME_CLEAN))  # Preferir nombres más cortos
        
        if (nrow(prefix_candidates) > 0) {
          best_prefix <- prefix_candidates %>% dplyr::slice(1)
          data.frame(
            input_name = .$input_name,
            input_clean = current_clean,
            PROV_NAME_OFFICIAL = best_prefix$PROV_NAME_OFFICIAL,
            distance_norm = 0,  # Prefijos exactos tienen distancia 0
            match_type = "prefix",
            stringsAsFactors = FALSE
          )
        } else {
          # Si no hay prefix match, usar fuzzy matching mejorado
          distances <- alias_clean %>%
            dplyr::filter(PROV_NAME_CLEAN != "_na_") %>%
            dplyr::mutate(
              # Usar Levenshtein para ser consistente con rgisDR
              distance = stringdist::stringdist(current_clean, PROV_NAME_CLEAN, method = "lv"),
              distance_norm = pmax(distance / nchar(current_clean), distance / nchar(PROV_NAME_CLEAN)),
              # Calcular bonus por coincidencias de inicio/final de palabras
              starts_with_input = ifelse(startsWith(PROV_NAME_CLEAN, current_clean), 0.2, 0),
              input_starts_with_name = ifelse(startsWith(current_clean, PROV_NAME_CLEAN), 0.1, 0),
              # Bonus especial para abreviaciones conocidas
              abbreviation_bonus = ifelse(
                (current_clean == "stgo" & grepl("^santiago", PROV_NAME_CLEAN)) |
                (current_clean == "srodriguez" & grepl("santiago.*rodriguez", PROV_NAME_CLEAN)) |
                (current_clean == "rod" & grepl("rodriguez", PROV_NAME_CLEAN)) |
                (current_clean == "rodriguez" & grepl("santiago.*rodriguez", PROV_NAME_CLEAN)),
                0.3, 0
              ),
              # Penalización ligera por longitud excesiva
              length_penalty = ifelse(nchar(PROV_NAME_CLEAN) > nchar(current_clean) * 2, 0.05, 0),
              # Score final: menor es mejor
              total_score = distance_norm - starts_with_input - input_starts_with_name - abbreviation_bonus + length_penalty
            ) %>%
            dplyr::arrange(total_score, nchar(PROV_NAME_CLEAN))
          
          best_match <- distances %>%
            dplyr::slice(1)
          
          data.frame(
            input_name = .$input_name,
            input_clean = current_clean,
            PROV_NAME_OFFICIAL = best_match$PROV_NAME_OFFICIAL,
            distance_norm = best_match$distance_norm,
            match_type = "fuzzy",
            stringsAsFactors = FALSE
          )
        }
      }) %>%
      dplyr::ungroup()
    
    # Aplicar criterios de tolerancia y manejo de errores
    final_matches <- prefix_matches %>%
      dplyr::mutate(
        PROV_NAME_OFFICIAL = dplyr::case_when(
          match_type == "prefix" ~ PROV_NAME_OFFICIAL,  # Prefijos siempre se aceptan
          distance_norm <= .tol ~ PROV_NAME_OFFICIAL,
          .on_error == "na" ~ NA_character_,
          .on_error == "omit" ~ input_name,
          TRUE ~ PROV_NAME_OFFICIAL
        )
      )
    
    # Verificar si hay errores que requieren abortar
    if (.on_error == "fail" && any(final_matches$distance_norm > .tol & final_matches$match_type != "prefix", na.rm = TRUE)) {
      problem_cases <- final_matches %>%
        dplyr::filter(distance_norm > .tol, match_type != "prefix") %>%
        dplyr::mutate(
          message = paste0("'", input_name, "' -> '", PROV_NAME_OFFICIAL, "' (tolerancia: ", round(distance_norm, 3), ")")
        )
      
      cli::cli_abort(
        c(
          "x" = "Algunos nombres de provincias no pudieron emparejarse con la tolerancia especificada ({(.tol)}):",
          " " = paste(problem_cases$message, collapse = "\n  "),
          "i" = "Considera aumentar .tol o usar .on_error = 'na' o 'omit'"
        )
      )
    }
    
    # Actualizar las coincidencias exactas con los resultados fuzzy/prefix
    exact_matches <- exact_matches %>%
      dplyr::filter(!is.na(PROV_NAME_OFFICIAL)) %>%
      dplyr::bind_rows(
        final_matches %>% dplyr::select(input_name, input_clean, PROV_NAME_OFFICIAL)
      )
  }
  
  # Devolver resultado final manteniendo el orden original
  result <- data.frame(input_name = names, stringsAsFactors = FALSE) %>%
    dplyr::left_join(
      exact_matches %>% dplyr::select(input_name, PROV_NAME_OFFICIAL),
      by = "input_name"
    ) %>%
    dplyr::pull(PROV_NAME_OFFICIAL)
  
  return(result)
}

#' Limpiar y estandarizar nombres de regiones
#'
#' @param names Vector de nombres de regiones a limpiar
#' @param .tol Tolerancia para coincidencias fuzzy (0 a 1, por defecto 0.25)
#' @param .on_error Acción en caso de error: "fail", "na" o "omit" (por defecto "fail")
#' @return Vector de nombres de regiones estandarizados
#' @export
gd_clean_region_name <- function(names, .tol = 0.25, .on_error = "fail") {
  # Obtener datos de regiones y alias
  regions_data <- gd_regions(sf = FALSE)
  alias_data <- .get_regiones_alias()
  
  # Usar función similar a la de provincias
  .do_region_names_cleaning(names, alias_data, regions_data, .tol, .on_error)
}

# Función específica de limpieza de nombres de regiones
.do_region_names_cleaning <- function(names, alias_data, regions_data, .tol = 0.25, .on_error = "fail") {
  # Limpiar los nombres de entrada
  names_clean <- .text_cleaning(names)
  
  # Buscar coincidencias exactas en alias
  exact_matches <- data.frame(
    input_name = names,
    input_clean = names_clean,
    REGION_NAME_OFFICIAL = NA_character_,
    stringsAsFactors = FALSE
  )
  
  if (!is.null(alias_data) && nrow(alias_data) > 0) {
    # Buscar en alias
    for (i in seq_along(names_clean)) {
      clean_name <- names_clean[i]
      if (clean_name == "_na_") next
      
      # Buscar coincidencia exacta en los alias
      alias_match <- alias_data[.text_cleaning(alias_data$alias) == clean_name, ]
      if (nrow(alias_match) > 0) {
        exact_matches$REGION_NAME_OFFICIAL[i] <- alias_match$TOPONIMIA[1]
      } else {
        # Buscar coincidencia exacta en nombres oficiales
        official_match <- regions_data[.text_cleaning(regions_data$TOPONIMIA) == clean_name, ]
        if (nrow(official_match) > 0) {
          exact_matches$REGION_NAME_OFFICIAL[i] <- official_match$TOPONIMIA[1]
        }
      }
    }
  }
  
  # Para nombres sin coincidencia exacta, intentar fuzzy matching
  unmatched_names <- exact_matches[is.na(exact_matches$REGION_NAME_OFFICIAL), ]
  
  if (nrow(unmatched_names) > 0) {
    # Crear lista de todos los nombres de referencia posibles
    reference_names <- c()
    if (!is.null(alias_data) && nrow(alias_data) > 0) {
      reference_names <- c(reference_names, alias_data$alias)
    }
    reference_names <- c(reference_names, regions_data$TOPONIMIA)
    reference_names <- unique(reference_names[!is.na(reference_names)])
    
    # Aplicar fuzzy matching para cada nombre no coincidente
    prefix_matches <- unmatched_names %>%
      dplyr::rowwise() %>%
      dplyr::do({
        current_clean <- .$input_clean
        
        if (current_clean == "_na_") {
          data.frame(
            input_name = .$input_name,
            input_clean = current_clean,
            REGION_NAME_OFFICIAL = NA_character_,
            distance_norm = 1.0,
            match_type = "na",
            stringsAsFactors = FALSE
          )
        } else {
          # Calcular distancias con todos los nombres de referencia
          distances <- data.frame(
            reference_name = reference_names,
            reference_clean = .text_cleaning(reference_names),
            stringsAsFactors = FALSE
          ) %>%
            dplyr::rowwise() %>%
            dplyr::mutate(
              distance = stringdist::stringdist(current_clean, reference_clean, method = "lv"),
              distance_norm = distance / max(nchar(current_clean), nchar(reference_clean))
            ) %>%
            dplyr::ungroup()
          
          # Buscar mejor coincidencia
          best_match <- distances %>%
            dplyr::arrange(distance_norm) %>%
            dplyr::slice(1)
          
          # Mapear el nombre de referencia al nombre oficial
          official_name <- NA_character_
          if (!is.null(alias_data) && best_match$reference_name %in% alias_data$alias) {
            official_name <- alias_data$TOPONIMIA[alias_data$alias == best_match$reference_name][1]
          } else if (best_match$reference_name %in% regions_data$TOPONIMIA) {
            official_name <- best_match$reference_name
          }
          
          data.frame(
            input_name = .$input_name,
            input_clean = current_clean,
            REGION_NAME_OFFICIAL = official_name,
            distance_norm = best_match$distance_norm,
            match_type = "fuzzy",
            stringsAsFactors = FALSE
          )
        }
      }) %>%
      dplyr::ungroup()
    
    # Aplicar criterios de tolerancia y manejo de errores
    final_matches <- prefix_matches %>%
      dplyr::mutate(
        REGION_NAME_OFFICIAL = dplyr::case_when(
          distance_norm <= .tol ~ REGION_NAME_OFFICIAL,
          .on_error == "na" ~ NA_character_,
          .on_error == "omit" ~ input_name,
          TRUE ~ REGION_NAME_OFFICIAL
        )
      )
    
    # Verificar si hay errores que requieren abortar
    if (.on_error == "fail" && any(final_matches$distance_norm > .tol, na.rm = TRUE)) {
      problem_cases <- final_matches %>%
        dplyr::filter(distance_norm > .tol) %>%
        dplyr::mutate(
          message = paste0("'", input_name, "' -> '", REGION_NAME_OFFICIAL, "' (tolerancia: ", round(distance_norm, 3), ")")
        )
      
      cli::cli_abort(
        c(
          "x" = "Algunos nombres de regiones no pudieron emparejarse con la tolerancia especificada ({(.tol)}):",
          " " = paste(problem_cases$message, collapse = "\n  "),
          "i" = "Considera aumentar .tol o usar .on_error = 'na' o 'omit'"
        )
      )
    }
    
    # Actualizar las coincidencias exactas con los resultados fuzzy
    exact_matches <- exact_matches %>%
      dplyr::filter(!is.na(REGION_NAME_OFFICIAL)) %>%
      dplyr::bind_rows(
        final_matches %>% dplyr::select(input_name, input_clean, REGION_NAME_OFFICIAL)
      )
  }
  
  # Devolver resultado final manteniendo el orden original
  result <- data.frame(input_name = names, stringsAsFactors = FALSE) %>%
    dplyr::left_join(
      exact_matches %>% dplyr::select(input_name, REGION_NAME_OFFICIAL),
      by = "input_name"
    ) %>%
    dplyr::pull(REGION_NAME_OFFICIAL)
  
  return(result)
}

# Función específica de limpieza de nombres de zonas de residencia
.do_zone_names_cleaning <- function(names, alias_data, zones_data, .tol = 0.25, .on_error = "fail") {
  # Limpiar los nombres de entrada
  names_clean <- .text_cleaning(names)
  
  # Buscar coincidencias exactas en alias
  exact_matches <- data.frame(
    input_name = names,
    input_clean = names_clean,
    ZONE_NAME_OFFICIAL = NA_character_,
    stringsAsFactors = FALSE
  )
  
  if (!is.null(alias_data) && nrow(alias_data) > 0) {
    # Buscar en alias
    for (i in seq_along(names_clean)) {
      clean_name <- names_clean[i]
      if (clean_name == "_na_") next
      
      # Buscar coincidencia exacta en los alias (ZONE_NAME)
      alias_match <- alias_data[.text_cleaning(alias_data$ZONE_NAME) == clean_name, ]
      if (nrow(alias_match) > 0) {
        exact_matches$ZONE_NAME_OFFICIAL[i] <- alias_match$TOPONIMIA[1]
      } else {
        # Buscar coincidencia exacta en nombres oficiales (TOPONIMIA)
        official_match <- zones_data[.text_cleaning(zones_data$TOPONIMIA) == clean_name, ]
        if (nrow(official_match) > 0) {
          exact_matches$ZONE_NAME_OFFICIAL[i] <- official_match$TOPONIMIA[1]
        }
      }
    }
  }
  
  # Para nombres sin coincidencia exacta, intentar fuzzy matching
  unmatched_names <- exact_matches[is.na(exact_matches$ZONE_NAME_OFFICIAL), ]
  
  if (nrow(unmatched_names) > 0) {
    # Crear lista de todos los nombres de referencia posibles
    reference_names <- c()
    if (!is.null(alias_data) && nrow(alias_data) > 0) {
      reference_names <- c(reference_names, alias_data$ZONE_NAME)
    }
    reference_names <- c(reference_names, zones_data$TOPONIMIA)
    reference_names <- unique(reference_names[!is.na(reference_names)])
    
    # Aplicar fuzzy matching para cada nombre no coincidente
    prefix_matches <- unmatched_names %>%
      dplyr::rowwise() %>%
      dplyr::do({
        current_clean <- .$input_clean
        
        if (current_clean == "_na_") {
          data.frame(
            input_name = .$input_name,
            input_clean = current_clean,
            ZONE_NAME_OFFICIAL = NA_character_,
            distance_norm = 1.0,
            match_type = "na",
            stringsAsFactors = FALSE
          )
        } else {
          # Calcular distancias con todos los nombres de referencia
          distances <- data.frame(
            reference_name = reference_names,
            reference_clean = .text_cleaning(reference_names),
            stringsAsFactors = FALSE
          ) %>%
            dplyr::rowwise() %>%
            dplyr::mutate(
              distance = stringdist::stringdist(current_clean, reference_clean, method = "lv"),
              distance_norm = distance / max(nchar(current_clean), nchar(reference_clean))
            ) %>%
            dplyr::ungroup()
          
          # Buscar mejor coincidencia
          best_match <- distances %>%
            dplyr::arrange(distance_norm) %>%
            dplyr::slice(1)
          
          # Mapear el nombre de referencia al nombre oficial
          official_name <- NA_character_
          if (!is.null(alias_data) && best_match$reference_name %in% alias_data$ZONE_NAME) {
            official_name <- alias_data$TOPONIMIA[alias_data$ZONE_NAME == best_match$reference_name][1]
          } else if (best_match$reference_name %in% zones_data$TOPONIMIA) {
            official_name <- best_match$reference_name
          }
          
          data.frame(
            input_name = .$input_name,
            input_clean = current_clean,
            ZONE_NAME_OFFICIAL = official_name,
            distance_norm = best_match$distance_norm,
            match_type = "fuzzy",
            stringsAsFactors = FALSE
          )
        }
      }) %>%
      dplyr::ungroup()
    
    # Aplicar criterios de tolerancia y manejo de errores
    final_matches <- prefix_matches %>%
      dplyr::mutate(
        ZONE_NAME_OFFICIAL = dplyr::case_when(
          distance_norm <= .tol ~ ZONE_NAME_OFFICIAL,
          .on_error == "na" ~ NA_character_,
          .on_error == "omit" ~ input_name,
          TRUE ~ ZONE_NAME_OFFICIAL
        )
      )
    
    # Verificar si hay errores que requieren abortar
    if (.on_error == "fail" && any(final_matches$distance_norm > .tol, na.rm = TRUE)) {
      problem_cases <- final_matches %>%
        dplyr::filter(distance_norm > .tol) %>%
        dplyr::mutate(
          message = paste0("'", input_name, "' -> '", ZONE_NAME_OFFICIAL, "' (tolerancia: ", round(distance_norm, 3), ")")
        )
      
      cli::cli_abort(
        c(
          "x" = "Algunos nombres de zonas no pudieron emparejarse con la tolerancia especificada ({(.tol)}):",
          " " = paste(problem_cases$message, collapse = "\n  "),
          "i" = "Considera aumentar .tol o usar .on_error = 'na' o 'omit'"
        )
      )
    }
    
    # Actualizar las coincidencias exactas con los resultados fuzzy
    exact_matches <- exact_matches %>%
      dplyr::filter(!is.na(ZONE_NAME_OFFICIAL)) %>%
      dplyr::bind_rows(
        final_matches %>% dplyr::select(input_name, input_clean, ZONE_NAME_OFFICIAL)
      )
  }
  
  # Devolver resultado final manteniendo el orden original
  result <- data.frame(input_name = names, stringsAsFactors = FALSE) %>%
    dplyr::left_join(
      exact_matches %>% dplyr::select(input_name, ZONE_NAME_OFFICIAL),
      by = "input_name"
    ) %>%
    dplyr::pull(ZONE_NAME_OFFICIAL)
  
  return(result)
}
