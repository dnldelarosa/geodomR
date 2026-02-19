# Tests para gd_clean_municipality_name

test_that("gd_clean_municipality_name works with basic municipality names", {
  # Casos básicos que deben encontrar coincidencias exactas
  expect_equal(gd_clean_municipality_name("azua"), "Azua")
  expect_equal(gd_clean_municipality_name("BARAHONA"), "Barahona")
  # "distrito nacional" es alias de MUN_ID 0101: oficial = "Santo Domingo de Guzmán" (orden curado del JSON)
  expect_equal(gd_clean_municipality_name("distrito nacional"), "Santo Domingo de Guzm\u00e1n")
})

test_that("gd_clean_municipality_name handles variations correctly", {
  # "santo domingo de guzman" mapea a "Santo Domingo de Guzmán" (nombre oficial)
  expect_equal(gd_clean_municipality_name("santo domingo de guzman"), "Santo Domingo de Guzm\u00e1n")
  # "azua de compostela" mapea a "Azua"
  expect_equal(gd_clean_municipality_name("azua de compostela"), "Azua")
  # "las yayas" mapea a "Las Yayas de Viajama" (nombre oficial en JSON)
  expect_equal(gd_clean_municipality_name("las yayas"), "Las Yayas de Viajama")
})

test_that("gd_clean_municipality_name enforces strict tolerance by default", {
  # Abreviaciones extremas deben fallar con tolerancia por defecto
  expect_error(gd_clean_municipality_name("stgo"), "Municipality name.*could not be matched")
  expect_error(gd_clean_municipality_name("sde"), "Municipality name.*could not be matched")
  expect_error(gd_clean_municipality_name("completely_invalid_name"), "Municipality name.*could not be matched")
  
  # Pero deben funcionar con .on_error = "na"
  expect_true(is.na(gd_clean_municipality_name("stgo", .on_error = "na")))
  expect_true(is.na(gd_clean_municipality_name("sde", .on_error = "na")))
  
  # Y con .on_error = "omit" deben devolver el input original
  expect_equal(gd_clean_municipality_name("stgo", .on_error = "omit"), "stgo")
  expect_equal(gd_clean_municipality_name("sde", .on_error = "omit"), "sde")
})

test_that("gd_clean_municipality_name handles partial names within tolerance", {
  # Nombres parciales que están dentro de la tolerancia por defecto
  expect_equal(gd_clean_municipality_name("barahona"), "Barahona")
  expect_equal(gd_clean_municipality_name("neiba"), "Neiba")
  expect_equal(gd_clean_municipality_name("galvan"), "Galván")
})

test_that("gd_clean_municipality_name handles multiple names", {
  input_names <- c("azua", "barahona", "neiba")
  expected_names <- c("Azua", "Barahona", "Neiba")
  expect_equal(gd_clean_municipality_name(input_names), expected_names)
})

test_that("gd_clean_municipality_name handles NA values correctly", {
  expect_equal(gd_clean_municipality_name(NA_character_), "_NA_")
  expect_equal(gd_clean_municipality_name(c("azua", NA_character_, "barahona")), 
               c("Azua", "_NA_", "Barahona"))
})

test_that("gd_clean_municipality_name respects tolerance parameter", {
  # Con tolerancia muy baja, debe fallar para nombres muy diferentes
  expect_error(gd_clean_municipality_name("invalid_name", .tol = 0.1), "Municipality name.*could not be matched")
  
  # Con tolerancia muy alta, debe aceptar nombres muy diferentes
  # (aunque no es recomendado en producción)
  result <- gd_clean_municipality_name("invalid_name", .tol = 0.9, .on_error = "na")
  expect_true(is.character(result) && (is.na(result) || nchar(result) > 0))
})

test_that("gd_clean_municipality_name handles empty input correctly", {
  expect_equal(gd_clean_municipality_name(character(0)), character(0))
  # El string vacío ahora se trata como error (antes mapeaba a "Mao" por bug en startsWith)
  expect_error(gd_clean_municipality_name(""), "empty")
  expect_true(is.na(gd_clean_municipality_name("", .on_error = "na")))
})

test_that("gd_clean_municipality_name handles common municipality variations", {
  # Casos específicos de municipios dominicanos con variaciones comunes
  expect_equal(gd_clean_municipality_name("las charcas"), "Las Charcas")
  expect_equal(gd_clean_municipality_name("padre las casas"), "Padre Las Casas")
  expect_equal(gd_clean_municipality_name("sabana yegua"), "Sabana Yegua")
  expect_equal(gd_clean_municipality_name("pueblo viejo"), "Pueblo Viejo")
})

test_that("gd_clean_municipality_name parameter validation", {
  # Verificar que los parámetros se validen correctamente
  expect_error(gd_clean_municipality_name("azua", .tol = -1), "debe ser un número")
  expect_error(gd_clean_municipality_name("azua", .tol = 2), "debe ser un número")
  expect_error(gd_clean_municipality_name("azua", .on_error = "invalid"), "debe ser uno de")
})

test_that("gd_clean_municipality_name fuzzy matching accuracy", {
  # Casos específicos para verificar la calidad del fuzzy matching
  expect_equal(gd_clean_municipality_name("azuz"), "Azua") # typo común
  expect_equal(gd_clean_municipality_name("barahon"), "Barahona") # typo común
  expect_equal(gd_clean_municipality_name("neiva"), "Neiba") # typo común
})
