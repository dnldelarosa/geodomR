# Funciones de limpieza y normalización de nombres administrativos

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
              distance = stringdist::stringdist(current_clean, PROV_NAME_CLEAN, method = "jw"),
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
