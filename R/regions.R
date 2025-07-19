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
#'
#' @return Un objeto de la clase `sf` o un `data.frame`.
#' @export
#' @importFrom sf st_drop_geometry
#' @examples
#' \dontrun{
#'   # Cargar el objeto sf completo
#'   regiones_sf <- gd_regions()
#'
#'   # Cargar solo la tabla de atributos (sin geometría)
#'   regiones_df <- gd_regions(sf = FALSE)
#' }
gd_regions <- function(id = "RD_RUP", sf = TRUE) {
  data_sf <- fetch_and_cache(id = id)

  if (!sf) {
    data_sf <- sf::st_drop_geometry(data_sf)
  }

  return(data_sf)
}
