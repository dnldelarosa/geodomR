# R/map.R
# Funciones para crear mapas coropléticos de República Dominicana

#' Detectar la mejor variable para fill en un mapa
#'
#' Esta función analiza las columnas de un data frame y selecciona automáticamente

#' la variable más apropiada para usar como fill en un mapa coroplético.
#' Usa varianza normalizada para variables numéricas y entropía de Shannon
#' para variables categóricas.
#'
#' @param data Un data frame con los datos a mapear.
#' @param exclude Nombres de columnas a excluir de la consideración (ej: la variable geográfica).
#'
#' @return El nombre de la variable seleccionada como mejor candidata para fill.
#'
#' @examples
#' \dontrun{
#' datos <- data.frame(
#'     provincia = c("Santo Domingo", "Santiago", "La Vega"),
#'     poblacion = c(2500000, 1000000, 400000),
#'     categoria = c("A", "B", "A")
#' )
#' fill_var <- gd_detect_fill(datos, exclude = "provincia")
#' }
#' @export
gd_detect_fill <- function(data, exclude = NULL) {
    # Obtener columnas candidatas (excluir las especificadas)
    candidates <- setdiff(names(data), exclude)

    if (length(candidates) == 0) {
        stop("No hay variables candidatas para fill despu\u00e9s de excluir las especificadas.")
    }

    scores <- sapply(candidates, function(col_name) {
        col <- data[[col_name]]

        # Calcular proporción de NA
        na_ratio <- sum(is.na(col)) / length(col)

        # Penalizar fuertemente si hay más de 20% NA
        if (na_ratio > 0.2) {
            return(0)
        }

        # Remover NA para cálculos
        col_clean <- col[!is.na(col)]

        if (length(col_clean) == 0) {
            return(0)
        }

        # Penalizar valores constantes
        if (length(unique(col_clean)) == 1) {
            return(0)
        }

        if (is.numeric(col_clean)) {
            # Para variables numéricas: coeficiente de variación (CV)
            # CV = sd / mean, pero normalizado para que esté entre 0 y 1
            mean_val <- mean(col_clean, na.rm = TRUE)
            if (mean_val == 0) {
                # Usar solo varianza si la media es 0
                score <- stats::sd(col_clean, na.rm = TRUE) / max(abs(col_clean))
            } else {
                cv <- stats::sd(col_clean, na.rm = TRUE) / abs(mean_val)
                # Normalizar CV usando función sigmoide suave
                score <- 1 - exp(-cv)
            }
            # Dar preferencia a numéricas (multiplicar por 1.5)
            return(score * 1.5 * (1 - na_ratio))
        } else {
            # Para variables categóricas: entropía de Shannon normalizada
            freq <- table(col_clean)
            prob <- freq / sum(freq)
            # Evitar log(0)
            prob <- prob[prob > 0]
            entropy <- -sum(prob * log2(prob))
            # Normalizar por entropía máxima (log2 del número de categorías)
            max_entropy <- log2(length(prob))
            if (max_entropy == 0) {
                return(0)
            }
            normalized_entropy <- entropy / max_entropy
            return(normalized_entropy * (1 - na_ratio))
        }
    })

    # Seleccionar la variable con mayor score
    best_var <- names(which.max(scores))

    if (is.null(best_var) || length(best_var) == 0 || max(scores) == 0) {
        stop(
            "No se pudo detectar una variable apropiada para fill. ",
            "Por favor, especifique el argumento 'fill' manualmente."
        )
    }

    return(best_var)
}


#' Preparar datos para mapeo en República Dominicana
#'
#' Esta función prepara los datos para mapeo uniéndolos con la geometría
#' del nivel administrativo detectado automáticamente.
#'
#' @param data Un data frame con los datos a mapear.
#' @param fill Nombre de la variable para fill. Si es NULL, se detecta automáticamente.
#' @param .level Nivel administrativo opcional. Si es NULL, se detecta automáticamente.
#' @param .name Nombre de la variable geográfica en data. Si es NULL, se detecta automáticamente.
#' @param .key Nombre de la variable clave en los datos geográficos. Si es NULL, se detecta automáticamente.
#'
#' @return Un objeto `sf` con los datos unidos a las geometrías administrativas.
#'
#' @examples
#' \dontrun{
#' datos <- data.frame(
#'     provincia = c("Santo Domingo", "Santiago", "La Vega"),
#'     poblacion = c(2500000, 1000000, 400000)
#' )
#' map_data <- gd_map_data(datos)
#' }
#' @export
gd_map_data <- function(data, fill = NULL, .level = NULL, .name = NULL, .key = NULL) {
    # Detectar nivel administrativo si no se especifica
    info <- gd_detect_level(data, .level, .name, .key)

    if (is.null(info[["level"]])) {
        stop(
            "No se pudo determinar el nivel administrativo del mapa. ",
            "Por favor, especifique el argumento '.level' manualmente (ej: 'provinces', 'municipalities', etc.)."
        )
    }

    if (is.null(info[["name"]])) {
        stop(
            "No se pudo determinar la variable de enlace geogr\u00e1fico en los datos.",
            "Por favor, especifique el argumento '.name' manualmente."
        )
    }

    if (is.null(info[["key"]])) {
        stop(
            "No se pudo determinar la variable clave para el enlace.",
            "Por favor, especifique el argumento '.key' manualmente."
        )
    }

    # Detectar fill automáticamente si no se especifica
    if (is.null(fill)) {
        fill <- gd_detect_fill(data, exclude = info[["name"]])
        # message("Variable de fill detectada autom\u00e1ticamente: '", fill, "'")
    }

    # Obtener geometrías del nivel detectado
    map_geom <- switch(info[["level"]],
        "sections" = gd_sections(),
        "dm" = gd_dm(),
        "municipalities" = gd_municipalities(),
        "provinces" = gd_provinces(),
        "regions" = gd_regions(),
        "zones" = gd_zones(),
        "bparajes" = gd_bparajes(),
        stop("Nivel administrativo no reconocido: ", info[["level"]])
    )

    # Limpiar los nombres en los datos del usuario antes del join
    # Esto asegura que coincidan con los nombres canónicos en las geometrías
    data_clean <- data
    name_col <- info[["name"]]
    level <- info[["level"]]

    # Aplicar función de limpieza según el nivel detectado
    if (level == "provinces" && info[["key"]] == "TOPONIMIA") {
        tryCatch(
            {
                data_clean[[name_col]] <- gd_clean_prov_name(
                    data[[name_col]],
                    .tol = 0.5,
                    .on_error = "na"
                )
            },
            error = function(e) {
                warning("No se pudieron limpiar los nombres de provincias: ", e$message)
            }
        )
    } else if (level == "regions" && info[["key"]] == "TOPONIMIA") {
        tryCatch(
            {
                data_clean[[name_col]] <- gd_clean_region_name(
                    data[[name_col]],
                    .tol = 0.5,
                    .on_error = "na"
                )
            },
            error = function(e) {
                warning("No se pudieron limpiar los nombres de regiones: ", e$message)
            }
        )
    } else if (level == "zones" && info[["key"]] == "TOPONIMIA") {
        tryCatch(
            {
                data_clean[[name_col]] <- gd_clean_zone_name(
                    data[[name_col]],
                    .tol = 0.5,
                    .on_error = "na"
                )
            },
            error = function(e) {
                warning("No se pudieron limpiar los nombres de zonas: ", e$message)
            }
        )
    } else if (level == "municipalities" && info[["key"]] == "TOPONIMIA") {
        tryCatch(
            {
                data_clean[[name_col]] <- gd_clean_municipality_name(
                    data[[name_col]],
                    .tol = 0.5,
                    .on_error = "na"
                )
            },
            error = function(e) {
                warning("No se pudieron limpiar los nombres de municipios: ", e$message)
            }
        )
    } else if (level == "dm" && info[["key"]] == "TOPONIMIA") {
        tryCatch(
            {
                data_clean[[name_col]] <- gd_clean_dm_name(
                    data[[name_col]],
                    .tol = 0.5,
                    .on_error = "na"
                )
            },
            error = function(e) {
                warning("No se pudieron limpiar los nombres de distritos municipales: ", e$message)
            }
        )
    } else if (level == "sections" && info[["key"]] == "TOPONIMIA") {
        tryCatch(
            {
                data_clean[[name_col]] <- gd_clean_section_name(
                    data[[name_col]],
                    .tol = 0.5,
                    .on_error = "na"
                )
            },
            error = function(e) {
                warning("No se pudieron limpiar los nombres de secciones: ", e$message)
            }
        )
    } else if (level == "bparajes" && info[["key"]] == "TOPONIMIA") {
        tryCatch(
            {
                data_clean[[name_col]] <- gd_clean_bparaje_name(
                    data[[name_col]],
                    .tol = 0.5,
                    .on_error = "na"
                )
            },
            error = function(e) {
                warning("No se pudieron limpiar los nombres de barrios/parajes: ", e$message)
            }
        )
    }

    # Aplicar el mismo cleaner a ambos lados para garantizar coincidencia
    # Las geometrías tienen TOPONIMIA en mayúsculas, los datos del usuario pueden variar
    key_col <- info[["key"]]
    if (key_col == "TOPONIMIA") {
        if (level == "provinces") {
            tryCatch(
                {
                    map_geom[[key_col]] <- gd_clean_prov_name(
                        map_geom[[key_col]],
                        .tol = 0.5,
                        .on_error = "omit"
                    )
                },
                error = function(e) {
                    warning("No se pudieron limpiar los nombres en geometr\u0069as: ", e$message)
                }
            )
        } else if (level == "regions") {
            tryCatch(
                {
                    map_geom[[key_col]] <- gd_clean_region_name(
                        map_geom[[key_col]],
                        .tol = 0.5,
                        .on_error = "omit"
                    )
                },
                error = function(e) {
                    warning("No se pudieron limpiar los nombres en geometr\u0069as: ", e$message)
                }
            )
        } else if (level == "zones") {
            tryCatch(
                {
                    map_geom[[key_col]] <- gd_clean_zone_name(
                        map_geom[[key_col]],
                        .tol = 0.5,
                        .on_error = "omit"
                    )
                },
                error = function(e) {
                    warning("No se pudieron limpiar los nombres en geometr\u0069as: ", e$message)
                }
            )
        } else if (level == "municipalities") {
            tryCatch(
                {
                    map_geom[[key_col]] <- gd_clean_municipality_name(
                        map_geom[[key_col]],
                        .tol = 0.5,
                        .on_error = "omit"
                    )
                },
                error = function(e) {
                    warning("No se pudieron limpiar los nombres en geometr\u0069as: ", e$message)
                }
            )
        } else if (level == "dm") {
            tryCatch(
                {
                    map_geom[[key_col]] <- gd_clean_dm_name(
                        map_geom[[key_col]],
                        .tol = 0.5,
                        .on_error = "omit"
                    )
                },
                error = function(e) {
                    warning("No se pudieron limpiar los nombres en geometr\u0069as: ", e$message)
                }
            )
        } else if (level == "sections") {
            tryCatch(
                {
                    map_geom[[key_col]] <- gd_clean_section_name(
                        map_geom[[key_col]],
                        .tol = 0.5,
                        .on_error = "omit"
                    )
                },
                error = function(e) {
                    warning("No se pudieron limpiar los nombres en geometr\u0069as: ", e$message)
                }
            )
        } else if (level == "bparajes") {
            tryCatch(
                {
                    map_geom[[key_col]] <- gd_clean_bparaje_name(
                        map_geom[[key_col]],
                        .tol = 0.5,
                        .on_error = "omit"
                    )
                },
                error = function(e) {
                    warning("No se pudieron limpiar los nombres en geometr\u0069as: ", e$message)
                }
            )
        }
    }

    # Unir datos con geometrías
    map_data <- map_geom %>%
        dplyr::left_join(
            data_clean,
            by = stats::setNames(name_col, key_col)
        )

    # Agregar atributo con el nombre del fill para uso posterior
    attr(map_data, "fill_var") <- fill
    attr(map_data, "geo_level") <- info[["level"]]

    return(map_data)
}


#' Inicializar un ggplot para mapas de República Dominicana
#'
#' Esta función inicializa un objeto ggplot con los datos preparados
#' para mapeo de República Dominicana. Establece automáticamente el
#' mapping de `geometry` y `fill` para que las capas posteriores
#' (como `gd_geom_sf()` o `geom_sf()`) lo hereden sin configuración adicional.
#'
#' @inheritParams gd_map_data
#'
#' @return Un objeto `ggplot` listo para agregar capas.
#'
#' @examples
#' \dontrun{
#' datos <- data.frame(
#'     provincia = c("Santo Domingo", "Santiago"),
#'     poblacion = c(2500000, 1000000)
#' )
#' gd_ggplot(datos) + gd_geom_sf(color = "white")
#' }
#' @export
gd_ggplot <- function(data, fill = NULL, .level = NULL, .name = NULL, .key = NULL) {
    map_data <- gd_map_data(data, fill, .level, .name, .key)

    fill_var <- attr(map_data, "fill_var")
    geo_col <- attr(map_data, "sf_column") %||% "geometry"

    default_mapping <- ggplot2::aes(
        geometry = .data[[geo_col]],
        fill = .data[[fill_var]]
    )

    ggplot2::ggplot(data = map_data, mapping = default_mapping)
}


#' Crear una capa geom_sf para mapas de República Dominicana
#'
#' Esta función crea una capa `geom_sf` que puede agregarse a un ggplot,
#' procesando automáticamente los datos y uniéndolos con geometrías administrativas.
#'
#' Cuando se usa con `gd_ggplot()` (sin `data`), hereda los datos y mappings
#' del ggplot padre. Cuando se proporciona `data`, procesa automáticamente
#' los datos crudos uniéndolos con las geometrías correspondientes.
#'
#' @param data Un data frame o NULL. Si es NULL, usa los datos del ggplot padre.
#' @param ... Argumentos adicionales pasados a `geom_sf` (ej: color, linewidth).
#'   Argumentos especiales que se extraen antes: `fill` (variable de relleno),
#'   `.level`, `.name`, `.key` (detección geográfica).
#'
#' @return Una capa ggplot2 de tipo `geom_sf`.
#'
#' @examples
#' \dontrun{
#' datos <- data.frame(
#'     provincia = c("Santo Domingo", "Santiago"),
#'     poblacion = c(2500000, 1000000)
#' )
#'
#' # Con gd_ggplot (hereda datos y fill)
#' gd_ggplot(datos) + gd_geom_sf(color = "white")
#'
#' # Independiente con datos crudos
#' ggplot2::ggplot() +
#'     gd_geom_sf(data = datos, fill = "poblacion")
#' }
#' @export
gd_geom_sf <- function(data = NULL, ...) {
    .args <- list(...)

    if (!is.null(data)) {
        # Extraer argumentos especiales si están presentes
        fill <- .args$fill
        .level <- .args$.level
        .name <- .args$.name
        .key <- .args$.key

        # Remover argumentos especiales de .args para evitar pasarlos a geom_sf
        .args$fill <- NULL
        .args$.level <- NULL
        .args$.name <- NULL
        .args$.key <- NULL

        # Preparar datos
        map_data <- gd_map_data(data, fill = fill, .level = .level, .name = .name, .key = .key)
        .args$data <- map_data

        # ggplot2 >=4.0 requiere aes(geometry=) explícito en geom_sf
        geo_col <- attr(map_data, "sf_column") %||% "geometry"
        fill_var <- attr(map_data, "fill_var")

        if (is.null(.args$mapping)) {
            .args$mapping <- ggplot2::aes(
                geometry = .data[[geo_col]],
                fill = .data[[fill_var]]
            )
        }
    }

    do.call(ggplot2::geom_sf, .args)
}


#' Crear un mapa coroplético de República Dominicana
#'
#' Esta función crea un mapa coroplético completo de República Dominicana
#' con detección automática del nivel administrativo y la variable de fill.
#' Es la función principal para crear mapas de forma sencilla.
#'
#' @param data Un data frame con los datos a mapear.
#' @param fill Nombre de la variable para fill. Si es NULL, se detecta automáticamente.
#' @param labels Controla las etiquetas en el mapa. Puede ser:
#'   - `NULL` o `FALSE`: sin etiquetas (por defecto).
#'   - `TRUE`: etiquetas con el nombre geográfico canónico (TOPONIMIA).
#'   - Una cadena de caracteres: nombre de la columna a usar como etiqueta.
#' @param label_size Tamaño del texto de las etiquetas. Por defecto 2.5.
#' @param .level Nivel administrativo opcional. Si es NULL, se detecta automáticamente.
#' @param .name Nombre de la variable geográfica en data. Si es NULL, se detecta automáticamente.
#' @param .key Nombre de la variable clave en los datos geográficos. Si es NULL, se detecta automáticamente.
#' @param ... Argumentos adicionales pasados a `geom_sf` (ej: color, linewidth).
#'
#' @return Un objeto `ggplot` representando el mapa coroplético.
#'
#' @examples
#' \dontrun{
#' # Uso mínimo - todo se detecta automáticamente
#' datos <- data.frame(
#'     provincia = c("Santo Domingo", "Santiago", "La Vega"),
#'     poblacion = c(2500000, 1000000, 400000)
#' )
#' gd_map(datos)
#'
#' # Con etiquetas automáticas
#' gd_map(datos, labels = TRUE)
#'
#' # Con etiquetas de una columna específica
#' gd_map(datos, fill = "poblacion", labels = "provincia")
#'
#' # Con argumentos explícitos
#' gd_map(datos, fill = "poblacion", color = "white", linewidth = 0.3)
#' }
#' @export
gd_map <- function(data, fill = NULL, labels = NULL, label_size = 2.5,
                   .level = NULL, .name = NULL, .key = NULL, ...) {
    # Preparar datos (fill se detecta aquí si es NULL)
    map_data <- gd_map_data(data, fill, .level, .name, .key)

    # Obtener el nombre del fill (ya sea especificado o detectado)
    fill_var <- attr(map_data, "fill_var")
    if (is.null(fill_var) && !is.null(fill)) {
        fill_var <- fill
    }

    # Crear el mapa con una capa base para mostrar todos los polígonos
    # Esto asegura que los polígonos sin datos (NA) sean visibles
    # Nota: ggplot2 >=4.0 requiere aes(geometry=) explícito en geom_sf
    geo_col <- attr(map_data, "sf_column") %||% "geometry"
    p <- ggplot2::ggplot(data = map_data) +
        ggplot2::geom_sf(
            mapping = ggplot2::aes(geometry = .data[[geo_col]]),
            fill = "gray90", color = "gray70", linewidth = 0.1
        ) +
        ggplot2::geom_sf(
            mapping = ggplot2::aes(geometry = .data[[geo_col]], fill = .data[[fill_var]]),
            ...
        ) +
        ggplot2::theme_void()

    # Agregar etiquetas si se solicitan
    if (!is.null(labels) && !identical(labels, FALSE)) {
        # Determinar la columna de etiquetas
        if (isTRUE(labels)) {
            label_col <- "TOPONIMIA"
        } else if (is.character(labels) && length(labels) == 1) {
            label_col <- labels
        } else {
            cli::cli_abort(c(
                "x" = "El argumento 'labels' debe ser TRUE, FALSE, NULL, o un nombre de columna.",
                "i" = "Ejemplo: labels = TRUE, labels = 'provincia'"
            ))
        }

        if (!label_col %in% names(map_data)) {
            cli::cli_abort(c(
                "x" = paste0("La columna '", label_col, "' no existe en los datos del mapa."),
                "i" = paste0("Columnas disponibles: ", paste(names(map_data), collapse = ", "))
            ))
        }

        # Calcular puntos interiores para ubicar las etiquetas
        centroids <- sf::st_point_on_surface(map_data)

        p <- p +
            ggplot2::geom_sf_text(
                data = centroids,
                mapping = ggplot2::aes(
                    geometry = .data[[geo_col]],
                    label = .data[[label_col]]
                ),
                size = label_size
            )
    }

    return(p)
}
