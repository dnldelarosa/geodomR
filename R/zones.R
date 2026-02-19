# Evitar NOTEs de variables globales en dplyr
utils::globalVariables(c(
  'ZONE_ID', 'ZONE_CODE', 'ZONE_NAME', 'TOPONIMIA'
))

#' Obtener Datos de Zonas de Residencia
#'
#' Proporciona datos sobre zonas de residencia (urbana/rural) en la República Dominicana.
#' Esta función crea un dataset estándar con las zonas de residencia para uso en análisis
#' y estandarización de datos.
#'
#' @param sf Lógico. Si es `FALSE`, devuelve un data.frame regular.
#'   Por defecto es `TRUE` (aunque no hay geometría para zones).
#'
#' @return Un data.frame con información de zonas de residencia.
#' @export
#' @examples
#' \dontrun{
#'   # Cargar datos de zonas
#'   zones_df <- gd_zones()
#'
#'   # Lo mismo (sf no aplica para zones)
#'   zones_df <- gd_zones(sf = FALSE)
#' }
gd_zones <- function(sf = TRUE) {
  # Dataset base con zonas de residencia estándar
  zones_data <- data.frame(
    ZONE_ID = c("01", "02"),
    ZONE_CODE = c("URB", "RUR"),
    ZONE_NAME = c("Urbana", "Rural"),
    TOPONIMIA = c("Urbana", "Rural"),
    stringsAsFactors = FALSE
  )
  
  return(zones_data)
}

# Helper function for zones aliases
.get_zones_alias <- function() {
  # Use standard geodomR function to get datasets
  datos <- gd_get_dataset(id = "zones_alias")
  return(datos$data)
}

#' Limpiar y estandarizar nombres de zonas de residencia
#'
#' @param zone Vector de nombres de zonas a limpiar.
#' @param .tol Tolerancia para coincidencias fuzzy (0 a 1, por defecto 0.25).
#' @param .on_error Acción en caso de error: "fail", "na" o "omit" (por defecto "fail").
#' @return Vector de nombres de zonas estandarizados.
#' @export 
#' @examples
#' \dontrun{
#'   # Limpiar nombres de zonas
#'   gd_clean_zone_name(c("zona urbana", "campo", "ciudad"))
#' }
gd_clean_zone_name <- function(zone, .tol = 0.25, .on_error = "fail") {
  .validate_clean_params(.tol, .on_error)

  alias_data <- .get_zones_alias()

  .do_generic_names_cleaning(
    names = zone, alias_data = alias_data,
    id_col = "ZONE_ID", name_col = "ZONE_NAME",
    level_label = "Zone",
    prefix_regex = "^(zona|area|\\u00e1rea)\\s+",
    code_regex = "^\\d{2}$",
    .tol = .tol, .on_error = .on_error
  )
}

