# Evitar NOTEs de variables globales en dplyr
utils::globalVariables(c(
  'PROV_ID', 'PROV_NAME', 'PROV_NAME_CLEAN', 'PROV_NAME_OFFICIAL', 'input_name', 'input_clean', '.',
  'distance', 'distance_norm', 'starts_with_input', 'input_starts_with_name', 'abbreviation_bonus',
  'length_penalty', 'total_score', 'match_type', 'reference_name', 'reference_clean', 'REGION_NAME_OFFICIAL',
  'ZONE_ID', 'ZONE_NAME', 'ZONE_NAME_OFFICIAL', 'MUN_NAME', 'MUN_NAME_CLEAN', 'MUN_NAME_OFFICIAL',
  'PROV_NAME_ALIAS', 'REGION_NAME_ALIAS', 'ZONE_NAME_ALIAS', 'MUN_NAME_ALIAS',
  'DM_NAME_ALIAS',
  'SEC_NAME_ALIAS',
  'BP_NAME_ALIAS',
  '.name_clean', '.dist',
  '.data', '_ID'
))

# ══════════════════════════════════════════════════════════════════════════════
# Funciones genéricas de limpieza y normalización de nombres administrativos
# ══════════════════════════════════════════════════════════════════════════════

# ── .text_cleaning ──────────────────────────────────────────────────────────
# Función compartida de limpieza de texto para uso en todos los niveles
# administrativos. Replica la funcionalidad de .text_cleaning de rgisDR.

.text_cleaning <- function(names) {
  names <- as.character(names)
  names <- tidyr::replace_na(names, "_na_")
  names <- stringr::str_to_lower(names)
  names <- stringr::str_squish(names)
  names <- chartr("\u00e1\u00e9\u00ed\u00f3\u00fa\u00fc\u00f1", "aeiouun", names)
  names <- stringr::str_remove(names, stringr::regex("^region[ ]?", ignore_case = TRUE))
  names <- stringr::str_remove(names, stringr::regex("^provincia[ ]?de[ ]?", ignore_case = TRUE))
  names <- stringr::str_remove(names, stringr::regex("^provincia[ ]?", ignore_case = TRUE))
  names <- stringr::str_remove(names, stringr::regex("^municipio[ ]?", ignore_case = TRUE))
  names <- stringr::str_remove(names, stringr::regex("^ayuntamiento[ ]?de[ ]?", ignore_case = TRUE))
  names <- stringr::str_remove(names, stringr::regex(" \\(d[.]?[ ]?m[.]?\\)", ignore_case = TRUE))
  names <- stringr::str_remove(names, stringr::regex(" [(]?zona urbana[)]?", ignore_case = TRUE))
  names <- stringr::str_remove(names, stringr::regex("^el[ ]", ignore_case = TRUE))
  names <- stringr::str_remove(names, stringr::regex("^la[s]?[ ]?", ignore_case = TRUE))
  names <- stringr::str_remove(names, stringr::regex("^los[ ]?", ignore_case = TRUE))
  names <- stringr::str_remove(names, stringr::regex("^de[l]?[ ]?", ignore_case = TRUE))
  names <- stringr::str_replace_all(names, stringr::regex("\\bde\\b", ignore_case = TRUE), "")
  names <- stringr::str_squish(names)
  names <- ifelse(names == "_na_", names, stringr::str_remove_all(names, stringr::regex("[^0-9a-z ]", ignore_case = TRUE)))
  names <- stringr::str_squish(names)
  names
}

# ── .validate_clean_params ──────────────────────────────────────────────────
# Validación compartida de parámetros .tol y .on_error

.validate_clean_params <- function(.tol, .on_error) {
  if (!is.numeric(.tol) || length(.tol) != 1 || .tol < 0 || .tol > 1) {
    cli::cli_abort(".tol debe ser un n\u00famero entre 0 y 1")
  }
  if (!.on_error %in% c("fail", "na", "omit")) {
    cli::cli_abort(".on_error debe ser uno de: 'fail', 'na', 'omit'")
  }
  invisible(TRUE)
}

# ── .resolve_parent_ids ─────────────────────────────────────────────────────
# Resolver IDs padre a partir de un nombre padre y su dataset de alias.
# Retorna un vector de IDs que coinciden con el nombre dado, o NULL.

.resolve_parent_ids <- function(parent_name, parent_alias_data, id_col, name_col) {
  if (is.null(parent_name)) return(NULL)

  parent_clean <- .text_cleaning(parent_name)

  # Coincidencia exacta

  parent_lookup <- parent_alias_data %>%
    dplyr::mutate(.name_clean = .text_cleaning(.data[[name_col]])) %>%
    dplyr::filter(.name_clean == parent_clean)

  if (nrow(parent_lookup) > 0) {
    return(unique(parent_lookup[[id_col]]))
  }

  # Fuzzy fallback
  parent_lookup_fuzzy <- parent_alias_data %>%
    dplyr::mutate(
      .name_clean = .text_cleaning(.data[[name_col]]),
      .dist = stringdist::stringdist(parent_clean, .name_clean, method = "jw")
    ) %>%
    dplyr::filter(.dist <= 0.25) %>%
    dplyr::arrange(.dist)

  if (nrow(parent_lookup_fuzzy) > 0) {
    return(unique(parent_lookup_fuzzy[[id_col]]))
  }

  return(NULL)
}

# ── .do_generic_names_cleaning ──────────────────────────────────────────────
# Función genérica de limpieza de nombres administrativos.
# Reemplaza las 7 funciones .do_X_names_cleaning() específicas por nivel.
#
# @param names Vector de nombres a limpiar
# @param alias_data Data frame con columnas id_col y name_col (y aliases)
# @param id_col Nombre de la columna de ID (ej: "DM_ID")
# @param name_col Nombre de la columna de nombre (ej: "DM_NAME")
# @param level_label Etiqueta para mensajes de error (ej: "DM", "section")
# @param prefix_regex Regex de prefijos específicos del nivel a remover
# @param code_regex Regex para detectar códigos numéricos (ej: "^\\d{6}$")
# @param parent_filter_ids Vector de IDs padre para filtrar (NULL = sin filtro)
# @param parent_prefix_len Longitud de prefijo del ID a comparar con padre
# @param parent_hint Texto para sugerir desambiguación (ej: "Use .municipality")
# @param .tol Tolerancia para fuzzy matching (Jaro-Winkler, 0-1)
# @param .on_error "fail", "na", "omit"

.do_generic_names_cleaning <- function(
    names, alias_data,
    id_col, name_col,
    level_label = "name",
    prefix_regex = NULL,
    code_regex = NULL,
    parent_filter_ids = NULL,
    parent_prefix_len = NULL,
    parent_hint = NULL,
    .tol = 0.25,
    .on_error = "fail"
) {
  # ── Preparar input ──
  if (length(names) == 0) return(character(0))
  names <- ifelse(is.na(names), "_NA_", as.character(names))
  names_clean <- .text_cleaning(names)

  # ── Tabla de nombres oficiales (primer registro por ID = orden curado del JSON) ──
  official_names <- alias_data %>%
    dplyr::group_by(.data[[id_col]]) %>%
    dplyr::slice(1) %>%
    dplyr::ungroup()

  # Renombrar columna de nombre a _OFFICIAL
  off_col <- paste0(gsub("_NAME$", "", name_col), "_NAME_OFFICIAL")
  official_names[[off_col]] <- official_names[[name_col]]
  official_names <- official_names[, c(id_col, off_col)]

  # ── Lookup: todas las variantes → nombre oficial ──
  alias_lookup <- alias_data %>%
    dplyr::left_join(official_names, by = id_col)

  alias_lookup[["_CLEAN"]] <- .text_cleaning(alias_lookup[[name_col]])
  alias_lookup[["_RAW_LOWER"]] <- tolower(alias_lookup[[name_col]])
  alias_lookup[["_OFFICIAL"]] <- alias_lookup[[off_col]]
  alias_lookup[["_ID"]] <- alias_lookup[[id_col]]
  alias_lookup <- alias_lookup[, c("_ID", "_CLEAN", "_RAW_LOWER", "_OFFICIAL")]
  alias_lookup <- dplyr::distinct(alias_lookup)

  # ── Filtro de padre ──
  if (!is.null(parent_filter_ids) && !is.null(parent_prefix_len)) {
    alias_lookup <- alias_lookup %>%
      dplyr::filter(substr(`_ID`, 1, parent_prefix_len) %in% parent_filter_ids)
    official_names <- official_names[
      substr(official_names[[id_col]], 1, parent_prefix_len) %in% parent_filter_ids, ]
  }

  # ── Procesar cada nombre ──
  results <- character(length(names))

  for (i in seq_along(names)) {
    current_name <- names[i]
    current_clean <- names_clean[i]

    # NA
    if (current_clean == "_na_") {
      results[i] <- "_NA_"
      next
    }

    # Cadena vacía → tratar como no-match
    if (current_clean == "") {
      results[i] <- .handle_no_match(
        current_name, level_label, .on_error,
        msg = paste0(level_label, " name is empty")
      )
      next
    }

    # ── Código directo ──
    if (!is.null(code_regex) && grepl(code_regex, current_name)) {
      code_match <- official_names[official_names[[id_col]] == current_name, ]
      if (nrow(code_match) > 0) {
        results[i] <- code_match[[off_col]][1]
        next
      }
      results[i] <- .handle_no_match(current_name, level_label, .on_error,
                                      msg = paste0(level_label, " code '", current_name, "' not found"))
      next
    }

    # ── Coincidencia exacta ──
    exact_matches <- alias_lookup[alias_lookup$`_CLEAN` == current_clean, ]

    if (nrow(exact_matches) > 0) {
      officials <- unique(exact_matches$`_OFFICIAL`)
      if (length(officials) == 1) {
        results[i] <- officials[1]
        next
      }
      # Desambiguar vía raw name: si exactamente una variante coincide con el
      # input sin text_cleaning (solo tolower), preferirla. Esto resuelve
      # colisiones artificiales causadas por .text_cleaning (ej: "Sur" y
      # "Región Sur" ambos se limpian a "sur").
      input_lower <- tolower(current_name)
      raw_exact <- exact_matches[exact_matches$`_RAW_LOWER` == input_lower, ]
      if (nrow(raw_exact) > 0) {
        raw_officials <- unique(raw_exact$`_OFFICIAL`)
        if (length(raw_officials) == 1) {
          results[i] <- raw_officials[1]
          next
        }
      }
      # Aún ambiguo
      results[i] <- .handle_ambiguous(current_name, officials, level_label, parent_hint, .on_error)
      next
    }

    # ── Remover prefijo del nivel ──
    current_no_prefix <- current_clean
    if (!is.null(prefix_regex)) {
      current_no_prefix <- gsub(prefix_regex, "", current_clean, ignore.case = TRUE)
    }

    if (current_no_prefix != current_clean) {
      prefix_exact <- alias_lookup[alias_lookup$`_CLEAN` == current_no_prefix, ]
      if (nrow(prefix_exact) > 0) {
        officials <- unique(prefix_exact$`_OFFICIAL`)
        if (length(officials) == 1) {
          results[i] <- officials[1]
          next
        }
        results[i] <- .handle_ambiguous(current_name, officials, level_label, parent_hint, .on_error)
        next
      }
    }

    # ── Prefix matching (input es prefijo de alias) ──
    prefix_matches <- alias_lookup[startsWith(alias_lookup$`_CLEAN`, current_no_prefix), ]
    prefix_matches <- prefix_matches[order(nchar(prefix_matches$`_CLEAN`)), ]

    if (nrow(prefix_matches) > 0) {
      officials <- unique(prefix_matches$`_OFFICIAL`)
      if (length(officials) == 1) {
        results[i] <- officials[1]
        next
      }
      # Múltiples: tomar el más corto (mejor match de prefijo)
      results[i] <- prefix_matches$`_OFFICIAL`[1]
      next
    }

    # ── Reverse prefix matching (alias es prefijo de input) ──
    reverse_matches <- alias_lookup[startsWith(current_no_prefix, alias_lookup$`_CLEAN`), ]
    reverse_matches <- reverse_matches[order(-nchar(reverse_matches$`_CLEAN`)), ]

    if (nrow(reverse_matches) > 0) {
      officials <- unique(reverse_matches$`_OFFICIAL`)
      if (length(officials) == 1) {
        results[i] <- officials[1]
        next
      }
      results[i] <- reverse_matches$`_OFFICIAL`[1]
      next
    }

    # ── Fuzzy matching (Jaro-Winkler) ──
    fuzzy_pool <- alias_lookup[alias_lookup$`_CLEAN` != "_na_", ]
    if (nrow(fuzzy_pool) > 0) {
      fuzzy_pool$`_DIST` <- stringdist::stringdist(
        current_no_prefix, fuzzy_pool$`_CLEAN`, method = "jw"
      )
      fuzzy_pool <- fuzzy_pool[order(fuzzy_pool$`_DIST`, nchar(fuzzy_pool$`_CLEAN`)), ]
      best <- fuzzy_pool[1, ]

      if (best$`_DIST` <= .tol) {
        results[i] <- best$`_OFFICIAL`
      } else {
        results[i] <- .handle_no_match(
          current_name, level_label, .on_error,
          msg = paste0(level_label, " name '", current_name,
                       "' could not be matched with tolerance ", .tol),
          hint = paste0("Best match was '", best$`_OFFICIAL`,
                        "' with distance ", round(best$`_DIST`, 3))
        )
      }
    } else {
      results[i] <- .handle_no_match(
        current_name, level_label, .on_error,
        msg = paste0(level_label, " name '", current_name, "' could not be matched")
      )
    }
  }

  return(results)
}

# ── .handle_no_match ────────────────────────────────────────────────────────
# Manejo estandarizado de errores cuando no hay coincidencia

.handle_no_match <- function(current_name, level_label, .on_error, msg = NULL, hint = NULL) {
  if (.on_error == "na") return(NA_character_)
  if (.on_error == "omit") return(current_name)
  # fail
  bullets <- c("x" = msg %||% paste0(level_label, " name '", current_name, "' not matched"))
  if (!is.null(hint)) bullets <- c(bullets, "i" = hint)
  bullets <- c(bullets, "i" = "Consider increasing .tol or using .on_error = 'na' or 'omit'")
  cli::cli_abort(bullets)
}

# ── .handle_ambiguous ───────────────────────────────────────────────────────
# Manejo estandarizado cuando se detecta ambiguedad (múltiples oficiales)

.handle_ambiguous <- function(current_name, officials, level_label, parent_hint, .on_error) {
  if (.on_error == "na") return(NA_character_)
  if (.on_error == "omit") return(current_name)
  n <- length(officials)
  sample_names <- paste(utils::head(officials, 3), collapse = "', '")
  bullets <- c(
    "x" = paste0(level_label, " '", current_name, "' is ambiguous: ", n, " matches found"),
    "i" = paste0("Matches include: '", sample_names, "'")
  )
  if (!is.null(parent_hint)) {
    bullets <- c(bullets, "i" = parent_hint)
  }
  cli::cli_abort(bullets)
}
