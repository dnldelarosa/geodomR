# R/bparajes.R

# Evitar NOTEs de variables globales en dplyr
utils::globalVariables(c(
  "BP_ID", "BP_NAME", "BP_NAME_CLEAN", "BP_NAME_OFFICIAL", "BP_NAME_ALIAS"
))

#' Obtener Límites de los Barrios y Parajes de la República Dominicana
#'
#' Descarga (si es necesario) y carga los límites de los barrios y parajes de la
#' República Dominicana, según la division correspondiente al id suministrado.
#'
#' Las divisiones disponibles son:
#'
#' - `"RD_BPARAJES"` (por defecto): Barrios y Parajes de la República Dominicana.
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
#'   # Cargar el objeto sf completo
#'   bparajes_sf <- gd_bparajes()
#'
#'   # Cargar solo la tabla de atributos (sin geometría)
#'   bparajes_df <- gd_bparajes(sf = FALSE)
#' }
gd_bparajes <- function(id = "RD_BPARAJES", sf = TRUE) {
  data_sf <- fetch_and_cache(id = id)

  if (!sf) {
    data_sf <- sf::st_drop_geometry(data_sf)
  }

  return(data_sf)
}

# ── Helpers ─────────────────────────────────────────────────────────────────

# Helper para obtener el dataset de alias de barrios-parajes
.get_bparajes_alias <- function() {
  raw_data <- gd_get_dataset(id = "bparajes_alias", verbose = FALSE)

  if (is.list(raw_data) && "data" %in% names(raw_data)) {
    return(raw_data$data)
  } else {
    return(raw_data)
  }
}

# ── Función interna de limpieza ─────────────────────────────────────────────

.do_bparaje_names_cleaning <- function(
    names, alias_data, parent_filter_ids = NULL, parent_prefix_len = NULL,
    .tol = 0.25, .on_error = "fail"
) {
  .do_generic_names_cleaning(
    names = names, alias_data = alias_data,
    id_col = "BP_ID", name_col = "BP_NAME",
    level_label = "Barrio/paraje",
    prefix_regex = "^(barrio|paraje|bar\\.?)\\s+",
    code_regex = "^\\d{11}$",
    parent_filter_ids = parent_filter_ids,
    parent_prefix_len = parent_prefix_len,
    parent_hint = "Use .section, .dm, or .municipality to disambiguate",
    .tol = .tol, .on_error = .on_error
  )
}

# ── Función exportada ───────────────────────────────────────────────────────

#' Limpia y estandariza los nombres de barrios y parajes de la República Dominicana
#'
#' Esta función limpia y estandariza los nombres de los barrios y parajes en la
#' República Dominicana. Soporta un enfoque híbrido: nombres únicos se resuelven
#' directamente, nombres ambiguos requieren especificar el nivel padre para
#' desambiguar, y códigos de 11 dígitos (BP_ID) se validan directamente.
#'
#' @param bp Vector de caracteres con los nombres (o códigos de 11 dígitos) de
#'   barrios/parajes a limpiar.
#' @param .section Nombre de la sección padre para desambiguar. Se limpia
#'   internamente contra el alias de secciones.
#' @param .dm Nombre del distrito municipal padre para desambiguar.
#' @param .municipality Nombre del municipio padre para desambiguar.
#' @param .tol Nivel de tolerancia numérica para la similitud de cadenas.
#'   Por defecto es 0.25. Un valor más bajo es más estricto.
#' @param .on_error Cadena de caracteres: "fail" (detener), "omit" (devolver
#'   original), o "na" (devolver NA) cuando no se puede resolver.
#'
#' @return Un vector de caracteres con los nombres de barrios/parajes limpiados.
#' @export
#'
#' @examples
#' \dontrun{
#' # Uso básico con nombre único
#' gd_clean_bparaje_name("Los Peralejos")
#'
#' # Código directo (11 dígitos)
#' gd_clean_bparaje_name("01010101001")
#'
#' # Nombre ambiguo con desambiguación por sección
#' gd_clean_bparaje_name("Centro del Pueblo", .section = "Sabana Grande")
#'
#' # Nombre ambiguo con desambiguación por municipio
#' gd_clean_bparaje_name("La Ceiba", .municipality = "Santiago")
#'
#' # Con tolerancia y manejo de errores
#' gd_clean_bparaje_name("Los Peralejo", .tol = 0.3, .on_error = "na")
#' }
gd_clean_bparaje_name <- function(
    bp,
    .section = NULL,
    .dm = NULL,
    .municipality = NULL,
    .tol = 0.25,
    .on_error = "fail"
) {
  .validate_clean_params(.tol, .on_error)

  alias_data <- .get_bparajes_alias()

  # ── Resolver filtro de padre (si se especifica) ──
  parent_filter_ids <- NULL
  parent_prefix_len <- NULL

  if (!is.null(.section)) {
    # Sección → SEC_ID (8 dígitos) → filtrar BP_ID[1:8]
    sec_alias <- .get_sections_alias()
    sec_ids <- .resolve_parent_ids(.section, sec_alias, "SEC_ID", "SEC_NAME")
    if (!is.null(sec_ids)) {
      parent_filter_ids <- sec_ids
      parent_prefix_len <- 8L
    }
  } else if (!is.null(.dm)) {
    # DM → DM_ID (6 dígitos) → filtrar BP_ID[1:6]
    dm_alias <- .get_dm_alias()
    dm_ids <- .resolve_parent_ids(.dm, dm_alias, "DM_ID", "DM_NAME")
    if (!is.null(dm_ids)) {
      parent_filter_ids <- dm_ids
      parent_prefix_len <- 6L
    }
  } else if (!is.null(.municipality)) {
    # Municipio → MUN_ID (4 dígitos) → filtrar BP_ID[1:4]
    mun_alias <- .get_municipios_alias()
    mun_ids <- .resolve_parent_ids(.municipality, mun_alias, "MUN_ID", "MUN_NAME")
    if (!is.null(mun_ids)) {
      parent_filter_ids <- mun_ids
      parent_prefix_len <- 4L
    }
  }

  # Limpiar nombres
  .do_bparaje_names_cleaning(
    bp, alias_data,
    parent_filter_ids = parent_filter_ids,
    parent_prefix_len = parent_prefix_len,
    .tol = .tol,
    .on_error = .on_error
  )
}
