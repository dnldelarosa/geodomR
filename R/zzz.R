# R/zzz.R

# Entorno interno para almacenar variables/configuraciones del paquete.
.pkg_env <- new.env(parent = emptyenv())

.onLoad <- function(libname, pkgname) {
  cache_path <- getOption("geodom.cache.path", default = fs::path(fs::path_home_r(), ".geodom"))
  fs::dir_create(cache_path)

  # 2. Guardar el "board" de cach\u00e9 local en el entorno del paquete.
  # Usaremos este board para guardar los datos como pines .json para compatibilidad con Python.
  .pkg_env$geodom_cache_board <- pins::board_folder(cache_path)

  # 3. Guardar la URL base para todas las descargas.
  .pkg_env$base_data_url <- "https://geodom-worker.drdsdaniel.workers.dev/"

  options(geodom.cache.path = cache_path)
}

.onAttach <- function(libname, pkgname) {
  cache_path <- getOption("geodom.cache.path")
  if (!is.null(cache_path)) {
    packageStartupMessage(
      "GeoDOM: Usando cach\u00e9 local en '",
      cache_path,
      "'"
    )
  }
}
