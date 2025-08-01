# R/detect.R

#' Detectar el nivel administrativo de datos geográficos
#'
#' Esta función intenta determinar automáticamente el nivel administrativo
#' apropiado, el nombre de la variable de enlace, y la variable clave para
#' datos relacionados con la República Dominicana. Examina los datos
#' proporcionados y los compara con los límites administrativos disponibles
#' para encontrar la mejor coincidencia.
#'
#' @param data Un data frame que contiene los datos a analizar.
#' @param .level Cadena de caracteres opcional especificando el nivel administrativo.
#'   Si se proporciona, la función solo considerará este nivel. Opciones válidas son:
#'   "sections", "dm", "municipalities", "provinces", "regions", "bparajes".
#' @param .name Cadena de caracteres opcional especificando el nombre de la variable
#'   en `data` que contiene los nombres de las unidades administrativas. Si se
#'   proporciona, la función solo considerará esta variable.
#' @param .key Cadena de caracteres opcional especificando el nombre de la variable
#'   clave en los datos de límites administrativos. Si se proporciona, la función
#'   solo considerará esta clave.
#'
#' @return Una lista que contiene los siguientes elementos:
#'   * `level`: El nivel administrativo inferido.
#'   * `name`: El nombre de la variable en `data` que contiene los nombres
#'     de las unidades administrativas.
#'   * `key`: El nombre de la variable clave en los datos de límites administrativos.
#'   * `match_count`: El número de valores únicos que coincidieron.
#'   * `total_count`: El número total de valores únicos en la columna.
#'
#' @examples
#' \dontrun{
#'   # Detectar información para datos de municipios
#'   mis_datos <- data.frame(municipio = c("Santo Domingo Este", "Santiago"), valor = 1:2)
#'   info <- gd_detect_level(mis_datos)
#'
#'   # Especificar el nivel administrativo
#'   info <- gd_detect_level(mis_datos, .level = "municipalities")
#' }
#' @export
gd_detect_level <- function(data, .level = NULL, .name = NULL, .key = NULL) {
  if (is.null(.level) || is.null(.name) || is.null(.key)) {
    
    # Obtener las opciones de referencia para cada nivel administrativo
    .options <- .get_admin_levels_data()
    
    if (!is.null(.level)) {
      .options <- .options[.level]
    }
    
    if (!is.null(.name)) {
      data <- data[.name]
    }
    
    if (!is.null(.key)) {
      for (.o in names(.options)) {
        if (.key %in% names(.options[[.o]])) {
          .options[[.o]] <- .options[[.o]][.key]
        }
      }
    }
    
    found <- FALSE
    best_match <- list(level = NULL, name = NULL, key = NULL, match_count = 0, total_count = 0)
    
    # Buscar coincidencias en cada nivel administrativo
    for (.opt_name in names(.options)) {
      .opt <- .options[[.opt_name]]
      for (key_name in names(.opt)) {
        for (variable in names(data)) {
          # Solo considerar columnas de texto (character o factor)
          if (!is.character(data[[variable]]) && !is.factor(data[[variable]])) {
            next
          }
          
          # Obtener valores únicos no nulos
          unique_values <- unique(data[[variable]])
          unique_values <- unique_values[!is.na(unique_values) & unique_values != ""]
          
          if (length(unique_values) > 0) {
            # Obtener valores de referencia únicos no nulos
            reference_values <- unique(.opt[[key_name]])
            reference_values <- reference_values[!is.na(reference_values) & reference_values != ""]
            
            # Evitar coincidencias con columnas numéricas o IDs secuenciales
            if (key_name %in% c("fid", "OBJECTID", "OBJECTID_1", "id") && 
                all(grepl("^\\d+$", as.character(unique_values)))) {
              next
            }
            
            # Evitar falsos positivos con columnas que tienen muy pocos valores únicos
            # A menos que sean coincidencias de limpieza exitosas
            if (length(unique_values) < 3 && !(.opt_name %in% c("provinces", "regions"))) {
              next
            }
            
            # Aplicar limpieza de nombres como método principal de detección
            matches <- 0
            used_cleaning_function <- FALSE
            
            # Primero intentar con funciones de limpieza específicas
            if (.opt_name == "provinces" && key_name == "TOPONIMIA") {
              tryCatch({
                # Usar tolerancia estricta para coincidencias exactas/casi exactas
                cleaned_values <- gd_clean_prov_name(unique_values, .tol = 0.25, .on_error = "fail")
                # Si llegamos aquí sin error, todos los valores fueron exitosamente limpiados
                matches <- length(unique_values)
                used_cleaning_function <- TRUE
              }, error = function(e) {
                # Si falla con tolerancia estricta, intentar con tolerancia más permisiva
                tryCatch({
                  cleaned_values <- gd_clean_prov_name(unique_values, .tol = 0.5, .on_error = "na")
                  cleaned_values <- cleaned_values[!is.na(cleaned_values)]
                  # Solo considerar exitoso si al menos 80% de los valores se limpiaron
                  if (length(cleaned_values) >= length(unique_values) * 0.8) {
                    matches <- length(cleaned_values)
                    used_cleaning_function <- TRUE
                  } else {
                    matches <- sum(unique_values %in% reference_values)
                    used_cleaning_function <- FALSE
                  }
                }, error = function(e2) {
                  # Como último recurso, usar comparación directa
                  matches <- sum(unique_values %in% reference_values)
                  used_cleaning_function <- FALSE
                })
              })
            } else if (.opt_name == "regions" && key_name == "TOPONIMIA") {
              tryCatch({
                # Usar tolerancia estricta para coincidencias exactas/casi exactas
                cleaned_values <- gd_clean_region_name(unique_values, .tol = 0.25, .on_error = "fail")
                # Si llegamos aquí sin error, todos los valores fueron exitosamente limpiados
                matches <- length(unique_values)
                used_cleaning_function <- TRUE
              }, error = function(e) {
                # Si falla con tolerancia estricta, intentar con tolerancia más permisiva
                tryCatch({
                  cleaned_values <- gd_clean_region_name(unique_values, .tol = 0.5, .on_error = "na")
                  cleaned_values <- cleaned_values[!is.na(cleaned_values)]
                  # Solo considerar exitoso si al menos 80% de los valores se limpiaron
                  if (length(cleaned_values) >= length(unique_values) * 0.8) {
                    matches <- length(cleaned_values)
                    used_cleaning_function <- TRUE
                  } else {
                    matches <- sum(unique_values %in% reference_values)
                    used_cleaning_function <- FALSE
                  }
                }, error = function(e2) {
                  # Como último recurso, usar comparación directa
                  matches <- sum(unique_values %in% reference_values)
                  used_cleaning_function <- FALSE
                })
              })
            } else {
              # Para otros niveles, usar comparación directa
              matches <- sum(unique_values %in% reference_values)
              used_cleaning_function <- FALSE
            }
            
            # Aplicar filtros adicionales para evitar falsos positivos
            # Si no usamos función de limpieza Y tenemos muy pocos valores únicos, ser más estricto
            if (!used_cleaning_function && length(unique_values) < 3 && matches < length(unique_values)) {
              next
            }
            
            total <- length(unique_values)
            
            # Si todas las coincidencias son exactas o limpias, es una coincidencia perfecta
            if (matches == total && matches > 0) {
              .level <- .opt_name
              .name <- variable
              .key <- key_name
              best_match <- list(
                level = .level,
                name = .name,
                key = key_name,
                match_count = matches,
                total_count = total
              )
              found <- TRUE
              break
            }
            # Si es la mejor coincidencia parcial hasta ahora
            else if (matches > best_match$match_count && matches > 0) {
              best_match <- list(
                level = .opt_name,
                name = variable,
                key = key_name,
                match_count = matches,
                total_count = total
              )
            }
          }
        }
        if (found) break
      }
      if (found) break
    }
    
    # Si no se encontró una coincidencia perfecta, usar la mejor coincidencia parcial
    if (!found && best_match$match_count > 0) {
      .level <- best_match$level
      .name <- best_match$name
      .key <- best_match$key
      
      # Advertir sobre coincidencia parcial
      warning(paste0("Coincidencia parcial encontrada: ", best_match$match_count, 
                    " de ", best_match$total_count, " valores coinciden con ", 
                    .level, " usando la variable '", .key, "'"))
    }
  }
  
  return(list(
    level = .level,
    name = .name,
    key = .key,
    match_count = ifelse(exists("best_match"), best_match$match_count, NA),
    total_count = ifelse(exists("best_match"), best_match$total_count, NA)
  ))
}

#' Obtener datos de todos los niveles administrativos para detección
#'
#' Función auxiliar que obtiene los datos de referencia de todos los niveles
#' administrativos disponibles en geodomR.
#'
#' @return Una lista con los datos de cada nivel administrativo.
#' @keywords internal
.get_admin_levels_data <- function() {
  tryCatch({
    .options <- list()
    
    # Secciones
    tryCatch({
      sections_data <- gd_sections(sf = FALSE)
      if (!is.null(sections_data)) {
        .options[["sections"]] <- sections_data
      }
    }, error = function(e) {
      message("No se pudieron cargar los datos de secciones: ", e$message)
    })
    
    # Distritos Municipales
    tryCatch({
      dm_data <- gd_dm(sf = FALSE)
      if (!is.null(dm_data)) {
        .options[["dm"]] <- dm_data
      }
    }, error = function(e) {
      message("No se pudieron cargar los datos de distritos municipales: ", e$message)
    })
    
    # Municipios
    tryCatch({
      municipalities_data <- gd_municipalities(sf = FALSE)
      if (!is.null(municipalities_data)) {
        .options[["municipalities"]] <- municipalities_data
      }
    }, error = function(e) {
      message("No se pudieron cargar los datos de municipios: ", e$message)
    })
    
    # Provincias
    tryCatch({
      provinces_data <- gd_provinces(sf = FALSE)
      if (!is.null(provinces_data)) {
        .options[["provinces"]] <- provinces_data
      }
    }, error = function(e) {
      message("No se pudieron cargar los datos de provincias: ", e$message)
    })
    
    # Regiones
    tryCatch({
      regions_data <- gd_regions(sf = FALSE)
      if (!is.null(regions_data)) {
        .options[["regions"]] <- regions_data
      }
    }, error = function(e) {
      message("No se pudieron cargar los datos de regiones: ", e$message)
    })
    
    # Barrios/Parajes (si existe la función)
    tryCatch({
      if (exists("gd_bparajes")) {
        bparajes_data <- gd_bparajes(sf = FALSE)
        if (!is.null(bparajes_data)) {
          .options[["bparajes"]] <- bparajes_data
        }
      }
    }, error = function(e) {
      message("No se pudieron cargar los datos de barrios/parajes: ", e$message)
    })
    
    return(.options)
    
  }, error = function(e) {
    stop("Error al cargar los datos de niveles administrativos: ", e$message)
  })
}

#' Detectar el tipo de columna geográfica específico
#'
#' Esta función analiza una columna específica de un data frame para determinar
#' qué tipo de información geográfica contiene.
#'
#' @param column Un vector con los valores a analizar.
#' @param column_name Nombre de la columna (opcional, para mensajes informativos).
#'
#' @return Una lista con información sobre el tipo detectado.
#' @examples
#' \dontrun{
#'   # Detectar tipo de una columna específica
#'   provincias <- c("Santo Domingo", "Santiago", "La Vega")
#'   resultado <- gd_detect_column_type(provincias, "provincia")
#' }
#' @export
gd_detect_column_type <- function(column, column_name = "columna") {
  # Crear un data frame temporal
  temp_df <- data.frame(col = column)
  names(temp_df) <- column_name
  
  # Usar la función principal de detección
  result <- gd_detect_level(temp_df)
  
  return(result)
}

#' Analizar todas las columnas de un data frame
#'
#' Esta función analiza todas las columnas de un data frame para identificar
#' cuáles contienen información geográfica y de qué tipo.
#'
#' @param data Un data frame a analizar.
#' @param threshold Umbral mínimo de coincidencias para considerar una detección válida.
#'   Por defecto es 0.7 (70% de coincidencias).
#'
#' @return Un data frame con los resultados del análisis para cada columna.
#' @examples
#' \dontrun{
#'   # Analizar todas las columnas de un data frame
#'   mis_datos <- data.frame(
#'     provincia = c("Santo Domingo", "Santiago"),
#'     municipio = c("Santo Domingo Este", "Santiago"),
#'     valor = c(100, 200)
#'   )
#'   resultados <- gd_analyze_columns(mis_datos)
#' }
#' @export
gd_analyze_columns <- function(data, threshold = 0.7) {
  results <- data.frame(
    column_name = character(),
    detected_level = character(),
    key_variable = character(),
    match_count = integer(),
    total_count = integer(),
    match_ratio = numeric(),
    is_geographic = logical(),
    stringsAsFactors = FALSE
  )
  
  for (col_name in names(data)) {
    # Solo analizar columnas de tipo character o factor
    if (is.character(data[[col_name]]) || is.factor(data[[col_name]])) {
      
      result <- gd_detect_column_type(data[[col_name]], col_name)
      
      match_ratio <- if (!is.na(result$match_count) && !is.na(result$total_count) && result$total_count > 0) {
        result$match_count / result$total_count
      } else {
        0
      }
      
      is_geographic <- !is.null(result$level) && match_ratio >= threshold
      
      results <- rbind(results, data.frame(
        column_name = col_name,
        detected_level = ifelse(is.null(result$level), NA, result$level),
        key_variable = ifelse(is.null(result$key), NA, result$key),
        match_count = ifelse(is.na(result$match_count), 0, result$match_count),
        total_count = ifelse(is.na(result$total_count), 0, result$total_count),
        match_ratio = match_ratio,
        is_geographic = is_geographic,
        stringsAsFactors = FALSE
      ))
    }
  }
  
  return(results)
}
