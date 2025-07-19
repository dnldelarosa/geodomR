# R/bparajes.R

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
