# R/dm.R

# TODO: Implementar funciones de limpieza para distritos municipales:
# TODO: - gd_clean_dm_name() función exportada
# TODO: - .get_dm_alias() helper function
# TODO: - .do_dm_names_cleaning() función interna
# TODO: - Crear dataset dm_alias en geodom-data-contributions
# TODO: Esto mejorará significativamente la detección automática de distritos municipales

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
