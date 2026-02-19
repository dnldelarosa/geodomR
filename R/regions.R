# Evitar NOTEs de variables globales en dplyr
utils::globalVariables(c(
  "REG_ID", "REG_NAME", "REG_NAME_CLEAN", "REG_NAME_OFFICIAL", "distance"
))
# R/regions.R

#' Obtener Límites de las Regiones de Planificación
#'
#' Descarga (si es necesario) y carga los límites de las regiones de
#' planificación según la regionalización correspondiente al id suministrado.
#'
#' Las regionalizaciones disponibles son:
#'
#' - `"RD_RUP"` (por defecto): Regiones Únicas de Planificación según Ley 345-22.
#' - `"RD_REG71004"`: Regiones de planificación según el Decreto 710-04.
#'
#' @param id Nombre del archivo de datos en el servidor remoto.
#' @param sf Lógico. Si es `FALSE`, devuelve un data.frame regular sin la
#'   columna de geometría. Por defecto es `TRUE`.
#' @param verbose Lógico. Si es `TRUE`, muestra mensajes informativos durante la
#' descarga y carga de datos. Por defecto es `FALSE`.
#'
#' @return Un objeto de la clase `sf` o un `data.frame`.
#' @export
#' @importFrom sf st_drop_geometry
#' @examples
#' \dontrun{
#' # Cargar el objeto sf completo
#' regiones_sf <- gd_regions()
#'
#' # Cargar solo la tabla de atributos (sin geometría)
#' regiones_df <- gd_regions(sf = FALSE)
#' }
gd_regions <- function(id = "RD_RUP", sf = TRUE, verbose = FALSE) {
  data_sf <- fetch_and_cache(id = id, verbose = verbose)

  if (!sf) {
    data_sf <- sf::st_drop_geometry(data_sf)
  }

  return(data_sf)
}

# Helper function for region aliases
.get_regiones_alias <- function() {
  raw_data <- gd_get_dataset(id = "regiones_alias", verbose = FALSE)
  
  # Extract the data portion from the JSON structure
  if (is.list(raw_data) && "data" %in% names(raw_data)) {
    return(raw_data$data)
  } else {
    # If it's already in the expected format, return as is
    return(raw_data)
  }
}

# Specific function for cleaning region names using alias dataset
.do_region_names_cleaning <- function(names, alias_data, .tol = 0.25, .on_error = "fail") {
  .do_generic_names_cleaning(
    names = names, alias_data = alias_data,
    id_col = "REG_ID", name_col = "REG_NAME",
    level_label = "Region",
    prefix_regex = "^(region|reg)\\.?\\s+",
    code_regex = "^\\d{2}$",
    .tol = .tol, .on_error = .on_error
  )
}

#' Clean and standardize Dominican Republic region names
#'
#' This function cleans and standardizes the names of regions in the Dominican Republic,
#' with tolerance for string similarity and options for error handling. Uses the
#' Regiones Únicas de Planificación according to Law 345-22.
#'
#' @param reg Character vector of region names to be cleaned.
#' @param .tol Numeric tolerance level for string similarity. Defaults to 0.25.
#' This parameter controls how similar two strings must be to be considered a match.
#' A lower value means stricter matching.
#' @param .on_error Character string specifying the error handling method. Defaults to "fail".
#' It can be one of the following: "fail" to stop execution on error,
#' "omit" to ignore unmatched names, or "na" to return NA for unmatched names.
#'
#' @return A cleaned character vector of region names.
#' @export
#'
#' @examples
#' \dontrun{
#' # Basic usage with region names
#' gd_clean_region_name(c("norte", "yuma", "valle"))
#'
#' # With code (2 digits)
#' gd_clean_region_name("01")
#'
#' # With prefix variants
#' gd_clean_region_name(c("Región Cibao Norte", "Región Valdesia"))
#'
#' # With tolerance and error handling
#' gd_clean_region_name("cibaooo", .tol = 0.8, .on_error = "na")
#' }
gd_clean_region_name <- function(reg, .tol = 0.25, .on_error = "fail") {
  .validate_clean_params(.tol, .on_error)

  alias_data <- .get_regiones_alias()

  .do_region_names_cleaning(reg, alias_data, .tol, .on_error)
}
