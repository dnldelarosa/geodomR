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
#' @param verbose Lógico. Si es `TRUE`, muestra mensajes de progreso durante la 
#' descarga y procesamiento de datos. Por defecto es `FALSE`.
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
gd_provinces <- function(id = "RD_PROV", sf = TRUE, .reg = NULL, verbose = FALSE) {
  PROV_CODE <- NULL  # Evitar el warning de no utilizado
  REG_CODE <- NULL   # Evitar el warning de no utilizado

  if (verbose) message("Descargando y cargando límites de provincias...")

  data_sf <- fetch_and_cache(id = id, verbose = verbose)

  if (!sf) {
    if (verbose) message("Eliminando geometría, devolviendo data.frame...")
    data_sf <- sf::st_drop_geometry(data_sf)
  }

  if (!is.null(.reg)) {
    if (.reg == tolower('rup')) {
      if (verbose) message("Agregando columna de región administrativa (Ley 345-22)...")
      .datos <- gd_get_dataset(id = "division_territorial_rd_ley_345_22", verbose = verbose)
      .datos[['data']] |>
        dplyr::select(PROV = PROV_CODE, REG_CODE) |>
        dplyr::distinct() |>
        dplyr::left_join(data_sf, by = c("PROV")) -> data_sf
    }
  }

  if (verbose) message("Finalizado.")
  return(data_sf)
}

# Función auxiliar para obtener el dataset de provincias_alias
.get_provincias_alias <- function() {
  tryCatch({
    # Usar la función estándar de geodomR para obtener datasets
    datos <- gd_get_dataset(id = "provincias_alias", verbose = FALSE)
    return(datos$data)
  }, error = function(e) {
    # Si falla, usar los datos básicos de gd_provinces
    prov_data <- gd_provinces(sf = FALSE)
    # Convertir a formato compatible con el dataset de alias
    prov_alias <- data.frame(
      PROV_ID = sprintf("%02d", as.numeric(rownames(prov_data))),
      PROV_CODE = prov_data$PROV_CODE,
      PROV_NAME = prov_data$PROV_NAME,
      stringsAsFactors = FALSE
    )
    return(prov_alias)
  })
}

#' Limpiar nombres de provincias de República Dominicana
#'
#' Esta función limpia y estandariza los nombres de las provincias de la
#' República Dominicana, utilizando un dataset de alias para mayor robustez
#' en el emparejamiento de nombres.
#'
#' Soporta tres modos de entrada:
#' - **Nombre**: se busca en el dataset de aliases (exacto, prefijo, fuzzy).
#' - **Código**: un código de 2 dígitos (PROV_ID) se valida directamente.
#'
#' @param prov Vector de caracteres con nombres (o códigos de 2 dígitos) de
#'   provincias a limpiar.
#' @param .tol Nivel de tolerancia numérico para similitud de cadenas. Por defecto 0.25.
#' @param .on_error Método de manejo de errores. Por defecto "fail".
#'   Puede ser "fail", "omit" o "na".
#'
#' @return Vector de caracteres con nombres de provincias limpiados.
#' @export
#'
#' @examples
#' \dontrun{
#'   # Uso básico
#'   gd_clean_prov_name(c("azua", "barahona", "stgo"))
#'
#'   # Con código directo (2 dígitos)
#'   gd_clean_prov_name("02")
#'
#'   # Con mayor tolerancia
#'   gd_clean_prov_name(c("azua", "barahona"), .tol = 0.5)
#' }
gd_clean_prov_name <- function(prov, .tol = 0.25, .on_error = "fail") {
  .validate_clean_params(.tol, .on_error)

  alias_data <- .get_provincias_alias()

  .do_generic_names_cleaning(
    names = prov, alias_data = alias_data,
    id_col = "PROV_ID", name_col = "PROV_NAME",
    level_label = "Province",
    prefix_regex = "^(provincia|prov)\\.?\\s+(de\\s+)?",
    code_regex = "^\\d{2}$",
    .tol = .tol, .on_error = .on_error
  )
}

