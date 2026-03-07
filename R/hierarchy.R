# R/hierarchy.R

# Evitar NOTEs de variables globales en dplyr
utils::globalVariables(c(
  "TOPONIMIA", "CODREG", "PROV", "MUN", "DM", "SEC", "SECC", "REG", "BP",
  "Region", "Provincia", "Municipio", "Distrito_Municipal", "Seccion",
  "current_name"
))

# ══════════════════════════════════════════════════════════════════════════════
# Jerarquía administrativa de la República Dominicana
# ══════════════════════════════════════════════════════════════════════════════

# Orden jerárquico de niveles (de mayor a menor)
.admin_hierarchy <- c("regions", "provinces", "municipalities", "dm", "sections", "bparajes")

# Mapeo de niveles a nombres de columna en español
.level_col_names <- c(
  regions        = "Region",
  provinces      = "Provincia",
  municipalities = "Municipio",
  dm             = "Distrito_Municipal",
  sections       = "Seccion",
  bparajes       = "Barrio_Paraje"
)

# Mapeo de niveles a funciones de limpieza
.get_clean_fn <- function(level) {
  switch(level,
    regions        = gd_clean_region_name,
    provinces      = gd_clean_prov_name,
    municipalities = gd_clean_municipality_name,
    dm             = gd_clean_dm_name,
    sections       = gd_clean_section_name,
    bparajes       = gd_clean_bparaje_name,
    stop("Nivel no reconocido: ", level)
  )
}

# Mapeo de niveles a funciones de carga de datos sf
.get_sf_fn <- function(level) {
  switch(level,
    regions        = gd_regions,
    provinces      = gd_provinces,
    municipalities = gd_municipalities,
    dm             = gd_dm,
    sections       = gd_sections,
    bparajes       = gd_bparajes,
    stop("Nivel no reconocido: ", level)
  )
}


#' Agregar columnas de niveles superiores a un data frame
#'
#' Esta función enriquece un data frame que contiene datos a un nivel
#' administrativo determinado (ej: provincias, municipios) con columnas
#' de nombre para los niveles superiores en la jerarquía territorial.
#'
#' La jerarquía administrativa de la República Dominicana es:
#' Regiones > Provincias > Municipios > Distritos Municipales > Secciones > Barrios/Parajes
#'
#' @param data Un data frame con una columna que identifica un nivel administrativo.
#' @param .levels Vector de caracteres con los niveles superiores que se desean agregar.
#'   Opciones: `"regions"`, `"provinces"`, `"municipalities"`, `"dm"`, `"sections"`.
#'   Si es `NULL` (por defecto), se agregan todos los niveles superiores disponibles.
#' @param .level Cadena de caracteres opcional especificando el nivel administrativo
#'   del data frame. Si no se proporciona, se detecta automáticamente.
#' @param .name Cadena de caracteres opcional especificando el nombre de la variable
#'   en `data` que contiene los nombres de las unidades administrativas.
#' @param .key Cadena de caracteres opcional especificando la clave del nivel.
#' @param .clean Lógico. Si `TRUE` (por defecto), limpia y estandariza los nombres
#'   geográficos en la columna detectada del data frame.
#' @param .tol Tolerancia para la limpieza de nombres (Jaro-Winkler). Por defecto 0.25.
#' @param .on_error Manejo de errores en la limpieza: `"fail"`, `"na"` o `"omit"`.
#'   Por defecto `"na"` para evitar que un solo nombre inválido detenga el proceso.
#'
#' @return Un data frame con columnas adicionales para cada nivel superior solicitado.
#'   Las columnas nuevas se nombran en español: Region, Provincia, Municipio,
#'   Distrito_Municipal, Seccion.
#'
#' @export
#'
#' @examples
#' \dontrun{
#'   # Agregar región a datos con columna de provincia
#'   datos <- data.frame(
#'     provincia = c("Santo Domingo", "Santiago", "La Vega", "Puerto Plata"),
#'     poblacion = c(2500000, 1000000, 400000, 320000)
#'   )
#'   datos_con_region <- gd_add_parent_cols(datos)
#'
#'   # Agregar sólo regiones (no todos los niveles superiores)
#'   datos_con_region <- gd_add_parent_cols(datos, .levels = "regions")
#'
#'   # Agregar provincia y región a datos de municipios
#'   datos_mun <- data.frame(
#'     municipio = c("Santo Domingo Este", "Santiago", "Moca"),
#'     valor = 1:3
#'   )
#'   datos_enriquecidos <- gd_add_parent_cols(datos_mun)
#'
#'   # Sin limpiar la columna original
#'   datos_enriquecidos <- gd_add_parent_cols(datos, .clean = FALSE)
#' }
gd_add_parent_cols <- function(data, .levels = NULL, .level = NULL, .name = NULL,
                                .key = NULL, .clean = TRUE, .tol = 0.25,
                                .on_error = "na") {

  # ── 1. Detectar nivel ──
  info <- gd_detect_level(data, .level = .level, .name = .name, .key = .key)

  if (is.null(info$level)) {
    cli::cli_abort(c(
      "x" = "No se pudo detectar el nivel administrativo de los datos.",
      "i" = "Aseg\u00farese de que el data frame contiene una columna con nombres geogr\u00e1ficos v\u00e1lidos.",
      "i" = "Puede especificar manualmente con los par\u00e1metros .level, .name y .key."
    ))
  }

  current_level <- info$level
  current_col <- info$name

  # ── 2. Validar que el nivel tiene padres ──
  current_idx <- match(current_level, .admin_hierarchy)

  if (is.na(current_idx)) {
    cli::cli_abort(c(
      "x" = paste0("Nivel '", current_level, "' no es parte de la jerarqu\u00eda est\u00e1ndar."),
      "i" = paste0("Niveles v\u00e1lidos: ", paste(.admin_hierarchy, collapse = ", "))
    ))
  }

  if (current_idx == 1) {
    cli::cli_inform("El nivel detectado es 'regions' (el m\u00e1s alto). No hay niveles superiores que agregar.")
    return(data)
  }

  # ── 3. Determinar qué niveles padres agregar ──
  available_parents <- .admin_hierarchy[seq_len(current_idx - 1)]

  if (!is.null(.levels)) {
    invalid <- setdiff(.levels, available_parents)
    if (length(invalid) > 0) {
      cli::cli_warn(c(
        "!" = paste0("Los siguientes niveles no son superiores a '", current_level, "': ",
                     paste(invalid, collapse = ", ")),
        "i" = paste0("Niveles superiores disponibles: ", paste(available_parents, collapse = ", "))
      ))
    }
    parent_levels <- intersect(.levels, available_parents)
  } else {
    parent_levels <- available_parents
  }

  if (length(parent_levels) == 0) {
    cli::cli_inform("No hay niveles superiores v\u00e1lidos para agregar.")
    return(data)
  }

  # ── 4. Construir tabla de lookup ──
  lookup <- .build_parent_lookup(current_level, parent_levels)

  # ── 5. Limpiar la columna del usuario ──
  clean_fn <- .get_clean_fn(current_level)

  if (.clean) {
    data[[current_col]] <- clean_fn(data[[current_col]], .tol = .tol, .on_error = .on_error)
  }

  # ── 6. Hacer el join ──
  result <- dplyr::left_join(
    data, lookup,
    by = stats::setNames("current_name", current_col)
  )

  return(result)
}


# ══════════════════════════════════════════════════════════════════════════════
# Función interna: construir tabla de lookup de padres
# ══════════════════════════════════════════════════════════════════════════════

#' Construir tabla de lookup para niveles superiores
#'
#' Crea un data frame que mapea nombres canónicos del nivel actual a nombres
#' canónicos de los niveles superiores solicitados. Usa los códigos de enlace
#' presentes en los datasets sf para vincular cada registro con sus padres.
#'
#' @param current_level Nivel administrativo actual (ej: "provinces")
#' @param parent_levels Vector de niveles superiores a incluir (ej: "regions")
#' @return Data frame con columnas: current_name, y una columna por cada parent level
#' @keywords internal
.build_parent_lookup <- function(current_level, parent_levels) {

  # Obtener datos sf del nivel actual (sin geometría)
  current_sf <- .get_sf_fn(current_level)(sf = FALSE)

  # Limpiar TOPONIMIA del nivel actual
  current_clean_fn <- .get_clean_fn(current_level)
  lookup <- data.frame(
    current_name = current_clean_fn(current_sf$TOPONIMIA, .on_error = "na"),
    stringsAsFactors = FALSE
  )

  # ── Agregar REG (región) directamente del dataset actual ──
  # Todos los niveles excepto regions tienen columna REG
  if ("regions" %in% parent_levels && "REG" %in% names(current_sf)) {
    reg_sf <- gd_regions(sf = FALSE)
    reg_map <- stats::setNames(
      gd_clean_region_name(reg_sf$TOPONIMIA, .on_error = "na"),
      reg_sf$CODREG
    )
    lookup$Region <- unname(reg_map[current_sf$REG])
  }

  # ── Agregar Provincia directamente del dataset actual ──
  # Municipios, DMs, secciones, bparajes tienen columna PROV
  if ("provinces" %in% parent_levels && "PROV" %in% names(current_sf)) {
    prov_sf <- gd_provinces(sf = FALSE)
    prov_map <- stats::setNames(
      gd_clean_prov_name(prov_sf$TOPONIMIA, .on_error = "na"),
      prov_sf$PROV
    )
    lookup$Provincia <- unname(prov_map[current_sf$PROV])
  }

  # ── Agregar Municipio ──
  # DMs, secciones, bparajes tienen columnas PROV + MUN
  # El ID municipio es compuesto: PROV (2 dígitos) + MUN (2 dígitos)
  if ("municipalities" %in% parent_levels &&
      "PROV" %in% names(current_sf) && "MUN" %in% names(current_sf)) {
    mun_sf <- gd_municipalities(sf = FALSE)
    mun_key <- paste0(mun_sf$PROV, mun_sf$MUN)
    mun_map <- stats::setNames(
      gd_clean_municipality_name(mun_sf$TOPONIMIA, .on_error = "na"),
      mun_key
    )
    current_mun_key <- paste0(current_sf$PROV, current_sf$MUN)
    lookup$Municipio <- unname(mun_map[current_mun_key])
  }

  # ── Agregar Distrito Municipal ──
  # Secciones y bparajes tienen PROV + MUN + DM
  if ("dm" %in% parent_levels &&
      "PROV" %in% names(current_sf) && "MUN" %in% names(current_sf) &&
      "DM" %in% names(current_sf)) {
    dm_sf <- gd_dm(sf = FALSE)
    dm_key <- paste0(dm_sf$PROV, dm_sf$MUN, dm_sf$DM)
    dm_map <- stats::setNames(
      gd_clean_dm_name(dm_sf$TOPONIMIA, .on_error = "na"),
      dm_key
    )
    current_dm_key <- paste0(current_sf$PROV, current_sf$MUN, current_sf$DM)
    lookup$Distrito_Municipal <- unname(dm_map[current_dm_key])
  }

  # ── Agregar Sección ──
  # Bparajes tienen PROV + MUN + DM + SECC/SEC
  if ("sections" %in% parent_levels) {
    sec_col <- if ("SEC" %in% names(current_sf)) "SEC" else if ("SECC" %in% names(current_sf)) "SECC" else NULL
    if (!is.null(sec_col) &&
        "PROV" %in% names(current_sf) && "MUN" %in% names(current_sf) &&
        "DM" %in% names(current_sf)) {
      sec_sf <- gd_sections(sf = FALSE)
      sec_key_col <- if ("SEC" %in% names(sec_sf)) "SEC" else "SECC"
      sec_key <- paste0(sec_sf$PROV, sec_sf$MUN, sec_sf$DM, sec_sf[[sec_key_col]])
      sec_map <- stats::setNames(
        gd_clean_section_name(sec_sf$TOPONIMIA, .on_error = "na"),
        sec_key
      )
      current_sec_key <- paste0(current_sf$PROV, current_sf$MUN, current_sf$DM, current_sf[[sec_col]])
      lookup$Seccion <- unname(sec_map[current_sec_key])
    }
  }

  # Eliminar duplicados
  lookup <- dplyr::distinct(lookup)

  return(lookup)
}
