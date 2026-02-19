# R/sections.R

# Evitar NOTEs de variables globales en dplyr
utils::globalVariables(c(
  "SEC_ID", "SEC_NAME", "SEC_NAME_CLEAN", "SEC_NAME_OFFICIAL", "SEC_NAME_ALIAS"
))

#' Obtener Límites de las Secciones de la República Dominicana
#'
#' Descarga (si es necesario) y carga los límites de las secciones de la
#' República Dominicana, según la division correspondiente al id suministrado.
#'
#' Las divisiones disponibles son:
#'
#' - `"RD_SECCIONES"` (por defecto): Secciones de la República Dominicana.
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
#'   secciones_sf <- gd_sections()
#'
#'   # Cargar solo la tabla de atributos (sin geometría)
#'   secciones_df <- gd_sections(sf = FALSE)
#' }
gd_sections <- function(id = "RD_SECCIONES", sf = TRUE) {
  data_sf <- fetch_and_cache(id = id)

  if (!sf) {
    data_sf <- sf::st_drop_geometry(data_sf)
  }

  return(data_sf)
}

# Helper function for section aliases
.get_sections_alias <- function() {
  raw_data <- gd_get_dataset(id = "sections_alias", verbose = FALSE)

  # Extract the data portion from the JSON structure
  if (is.list(raw_data) && "data" %in% names(raw_data)) {
    return(raw_data$data)
  } else {
    return(raw_data)
  }
}

# Helper function for robust section name cleaning
.do_section_names_cleaning <- function(names, alias_data, .tol = 0.25, .on_error = "fail",
                                        parent_filter_ids = NULL, parent_prefix_len = NULL) {
  .do_generic_names_cleaning(
    names = names, alias_data = alias_data,
    id_col = "SEC_ID", name_col = "SEC_NAME",
    level_label = "Section",
    prefix_regex = "^(seccion|secc?\\.?)\\s+",
    code_regex = "^\\d{8}$",
    parent_filter_ids = parent_filter_ids,
    parent_prefix_len = parent_prefix_len,
    parent_hint = "Use .dm or .municipality to disambiguate",
    .tol = .tol, .on_error = .on_error
  )
}

#' Limpia y estandariza los nombres de secciones de la República Dominicana
#'
#' Esta función limpia y estandariza los nombres de las secciones en la
#' República Dominicana, con tolerancia para la similitud de cadenas y opciones para el
#' manejo de errores. Utiliza el sistema oficial de códigos de la División Territorial.
#'
#' Soporta tres modos de entrada:
#' - **Nombre**: se busca en el dataset de aliases (exacto, prefijo, fuzzy).
#' - **Código**: un código de 8 dígitos (SEC_ID) se valida directamente.
#' - **Desambiguación**: si el nombre es ambiguo (ej: "La Ciénaga"), usar
#'   `.dm` o `.municipality` para filtrar por nivel padre.
#'
#' @param sec Vector de caracteres con los nombres (o códigos de 8 dígitos) de
#'   secciones a limpiar.
#' @param .dm Nombre del distrito municipal padre para desambiguar.
#' @param .municipality Nombre del municipio padre para desambiguar.
#' @param .tol Nivel de tolerancia numérica para la similitud de cadenas. Por defecto es 0.25.
#' @param .on_error Cadena de caracteres que especifica el método de manejo de errores.
#'   Puede ser "fail", "omit" o "na". Por defecto es "fail".
#'
#' @return Un vector de caracteres con los nombres de secciones limpiados.
#' @export
#'
#' @examples
#' \dontrun{
#' # Uso básico con nombres de secciones
#' gd_clean_section_name(c("Santo Domingo de Guzmán", "Barro Arriba"))
#'
#' # Con código directo (8 dígitos)
#' gd_clean_section_name("01010101")
#'
#' # Nombre ambiguo con desambiguación por DM
#' gd_clean_section_name("La Ciénaga", .dm = "Baní")
#'
#' # Nombre ambiguo con desambiguación por municipio
#' gd_clean_section_name("La Ciénaga", .municipality = "Baní")
#'
#' # Con tolerancia y manejo de errores
#' gd_clean_section_name("baro ariba", .tol = 0.3, .on_error = "na")
#' }
gd_clean_section_name <- function(sec, .dm = NULL, .municipality = NULL,
                                   .tol = 0.25, .on_error = "fail") {
  .validate_clean_params(.tol, .on_error)

  alias_data <- .get_sections_alias()

  # Resolver filtro de padre
  parent_filter_ids <- NULL
  parent_prefix_len <- NULL

  if (!is.null(.dm)) {
    dm_alias <- .get_dm_alias()
    dm_ids <- .resolve_parent_ids(.dm, dm_alias, "DM_ID", "DM_NAME")
    if (!is.null(dm_ids)) {
      parent_filter_ids <- dm_ids
      parent_prefix_len <- 6L
    }
  } else if (!is.null(.municipality)) {
    mun_alias <- .get_municipios_alias()
    mun_ids <- .resolve_parent_ids(.municipality, mun_alias, "MUN_ID", "MUN_NAME")
    if (!is.null(mun_ids)) {
      parent_filter_ids <- mun_ids
      parent_prefix_len <- 4L
    }
  }

  .do_section_names_cleaning(sec, alias_data, .tol, .on_error,
                              parent_filter_ids = parent_filter_ids,
                              parent_prefix_len = parent_prefix_len)
}
