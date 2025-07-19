# R/provinces.R

#' Obtener Límites de las Provincias de la República Dominicana
#'
#' Descarga (si es necesario) y carga los límites de las provincias de la
#' República Dominicana. Permite obtener los datos como objeto `sf` o como
#' `data.frame` sin geometría. Además, puede agregar la columna de región
#' administrativa según la Ley 345-22 si se especifica el argumento `.reg = "rup"`.
#'
#' @param id Nombre del archivo de datos en el servidor remoto. Por defecto es `"RD_PROV"`.
#' @param sf Lógico. Si es `FALSE`, devuelve un `data.frame` regular sin la
#'   columna de geometría. Por defecto es `TRUE`.
#' @param .reg Caracter. Si es `"rup"`, agrega la columna de región administrativa
#'   según la Ley 345-22. Por defecto es `NULL`.
#'
#' @return Un objeto de la clase `sf` o un `data.frame`, dependiendo del valor de `sf`.
#'         Si `.reg = "rup"`, incluye la columna de región administrativa.
#' @export
#' @importFrom sf st_drop_geometry
#' @examples
#' \dontrun{
#'   # Cargar el objeto sf completo
#'   provincias_sf <- gd_provinces()
#'
#'   # Cargar solo la tabla de atributos (sin geometría)
#'   provincias_df <- gd_provinces(sf = FALSE)
#'
#'   # Cargar provincias con columna de región administrativa
#'   provincias_reg <- gd_provinces(.reg = "rup")
#' }
gd_provinces <- function(id = "RD_PROV", sf = TRUE, .reg = NULL) {
  PROV_CODE <- NULL  # Evitar el warning de no utilizado
  REG_CODE <- NULL   # Evitar el warning de no utilizado
  data_sf <- fetch_and_cache(id = id)

  if (!sf) {
    data_sf <- sf::st_drop_geometry(data_sf)
  }

  if (!is.null(.reg)) {
    if (.reg == tolower('rup')) {
      .datos <- gd_get_dataset(id = "division_territorial_rd_ley_345_22")
      .datos[['data']] |>
        dplyr::select(PROV = PROV_CODE, REG_CODE) |>
        dplyr::distinct() |>
        dplyr::left_join(data_sf, by = c("PROV")) -> data_sf
    } else {
      stop(
        "El argumento .reg solo puede ser uno de los siguientes: \n  - 'rup'"
      )
    }
  }

  return(data_sf)
}
