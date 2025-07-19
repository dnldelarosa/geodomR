# R/macroregions.R

#' Obtener Límites de las Macro-Regiones
#'
#' Descarga (si es necesario) y carga los límites de las tres macro-regiones
#' de planificación como un objeto `sf`.
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
#'   macro_regiones_sf <- gd_macroregions()
#'
#'   # Cargar solo la tabla de atributos (sin geometría)
#'   macro_regiones_df <- gd_macroregions(sf = FALSE)
#' }
gd_macroregions <- function(id = "RD_MREG", sf = TRUE) {
  data_sf <- fetch_and_cache(id = id)

  if (!sf) {
    data_sf <- sf::st_drop_geometry(data_sf)
  }

  return(data_sf)
}
