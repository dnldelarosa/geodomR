# R/sections.R

# TODO: Implementar funciones de limpieza para secciones:
# TODO: - gd_clean_section_name() función exportada  
# TODO: - .get_sections_alias() helper function
# TODO: - .do_section_names_cleaning() función interna
# TODO: - Crear dataset sections_alias en geodom-data-contributions
# TODO: Esto mejorará significativamente la detección automática de secciones

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
