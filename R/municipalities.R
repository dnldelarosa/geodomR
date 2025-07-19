# R/municipalities.R

#' Obtener Límites de los Municipios de la República Dominicana
#'
#' Descarga (si es necesario) y carga los límites de los municipios de la
#' República Dominicana, según la division correspondiente al id suministrado.
#'
#' Las divisiones disponibles son:
#'
#' - `"RD_MUN158"` (por defecto): 158 Municipios de la República Dominicana.
#' - `"RD_MUN155"`: TODO
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
#'   municipios_sf <- gd_municipalities()
#'
#'   # Cargar solo la tabla de atributos (sin geometría)
#'   municipios_df <- gd_municipalities(sf = FALSE)
#' }
gd_municipalities <- function(id = "RD_MUN158", sf = TRUE) {
  data_sf <- fetch_and_cache(id = id)

  if (!sf) {
    data_sf <- sf::st_drop_geometry(data_sf)
  }

  return(data_sf)
}
