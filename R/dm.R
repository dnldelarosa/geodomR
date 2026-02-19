# R/dm.R

# Evitar NOTEs de variables globales en dplyr
utils::globalVariables(c(
  "DM_ID", "DM_NAME", "DM_NAME_CLEAN", "DM_NAME_OFFICIAL", "DM_NAME_ALIAS"
))

#' Obtener Límites de los Distritos Municipales de la República Dominicana
#'
#' Descarga (si es necesario) y carga los límites de los distritos municipales
#' de la República Dominicana, según la division correspondiente al id suministrado.
#'
#' Las divisiones disponibles son:
#'
#' - `"RD_DM"` (por defecto): Distritos Municipales de la República Dominicana.
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
#'   distritos_sf <- gd_dm()
#'
#'   # Cargar solo la tabla de atributos (sin geometría)
#'   distritos_df <- gd_dm(sf = FALSE)
#' }
gd_dm <- function(id = "RD_DM", sf = TRUE) {
  data_sf <- fetch_and_cache(id = id)

  if (!sf) {
    data_sf <- sf::st_drop_geometry(data_sf)
  }

  return(data_sf)
}

# Helper function for DM aliases
.get_dm_alias <- function() {
  raw_data <- gd_get_dataset(id = "dm_alias", verbose = FALSE)

  # Extract the data portion from the JSON structure
  if (is.list(raw_data) && "data" %in% names(raw_data)) {
    return(raw_data$data)
  } else {
    # If it's already in the expected format, return as is
    return(raw_data)
  }
}

# Helper function for robust DM name cleaning
.do_dm_names_cleaning <- function(names, alias_data, .tol = 0.25, .on_error = "fail",
                                   parent_filter_ids = NULL, parent_prefix_len = NULL) {
  .do_generic_names_cleaning(
    names = names, alias_data = alias_data,
    id_col = "DM_ID", name_col = "DM_NAME",
    level_label = "DM",
    prefix_regex = "^(distrito\\s+municipal|dist\\.?\\s*mun\\.?|d\\.?\\s*m\\.?)\\s+",
    code_regex = "^\\d{6}$",
    parent_filter_ids = parent_filter_ids,
    parent_prefix_len = parent_prefix_len,
    parent_hint = "Use .municipality to disambiguate",
    .tol = .tol, .on_error = .on_error
  )
}

#' Limpia y estandariza los nombres de distritos municipales de la República Dominicana
#'
#' Esta función limpia y estandariza los nombres de los distritos municipales en la
#' República Dominicana, con tolerancia para la similitud de cadenas y opciones para el
#' manejo de errores. Utiliza el sistema oficial de códigos de la División Territorial.
#'
#' Soporta tres modos de entrada:
#' - **Nombre**: se busca en el dataset de aliases (exacto, prefijo, fuzzy).
#' - **Código**: un código de 6 dígitos (DM_ID) se valida directamente.
#' - **Desambiguación**: si el nombre es ambiguo (ej: "Guayabal"), usar
#'   `.municipality` para filtrar por municipio padre.
#'
#' @param dm Vector de caracteres con los nombres (o códigos de 6 dígitos) de
#'   distritos municipales a limpiar.
#' @param .municipality Nombre del municipio padre para desambiguar.
#' @param .tol Nivel de tolerancia numérica para la similitud de cadenas. Por defecto es 0.25.
#' @param .on_error Cadena de caracteres que especifica el método de manejo de errores. Por defecto es "fail".
#'   Puede ser "fail", "omit" o "na".
#'
#' @return Un vector de caracteres con los nombres de distritos municipales limpiados.
#' @export
#'
#' @examples
#' \dontrun{
#' # Uso básico con nombres de distritos municipales
#' gd_clean_dm_name(c("barro arriba", "punta cana", "verón"))
#'
#' # Con código directo (6 dígitos)
#' gd_clean_dm_name("010102")
#'
#' # Nombre ambiguo con desambiguación por municipio
#' gd_clean_dm_name("Guayabal", .municipality = "Azua")
#'
#' # Con tolerancia y manejo de errores
#' gd_clean_dm_name("baro ariba", .tol = 0.3, .on_error = "na")
#' }
gd_clean_dm_name <- function(dm, .municipality = NULL, .tol = 0.25, .on_error = "fail") {
  .validate_clean_params(.tol, .on_error)

  alias_data <- .get_dm_alias()

  # Resolver filtro de padre
  parent_filter_ids <- NULL
  parent_prefix_len <- NULL

  if (!is.null(.municipality)) {
    mun_alias <- .get_municipios_alias()
    mun_ids <- .resolve_parent_ids(.municipality, mun_alias, "MUN_ID", "MUN_NAME")
    if (!is.null(mun_ids)) {
      parent_filter_ids <- mun_ids
      parent_prefix_len <- 4L
    }
  }

  .do_dm_names_cleaning(dm, alias_data, .tol, .on_error,
                         parent_filter_ids = parent_filter_ids,
                         parent_prefix_len = parent_prefix_len)
}
