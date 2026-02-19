# Tests para gd_clean_zone_name

test_that("gd_clean_zone_name works with basic zone names", {
  # Casos básicos que deben encontrar coincidencias
  expect_equal(gd_clean_zone_name("urbana"), "Urbana")
  expect_equal(gd_clean_zone_name("RURAL"), "Rural")
  expect_equal(gd_clean_zone_name("urbano"), "Urbana")
})

test_that("gd_clean_zone_name handles variations correctly", {
  # Casos con variaciones que deben mapearse correctamente
  expect_equal(gd_clean_zone_name("zona urbana"), "Urbana")
  expect_equal(gd_clean_zone_name("area urbana"), "Urbana")
  expect_equal(gd_clean_zone_name("área urbana"), "Urbana")
})

test_that("gd_clean_zone_name enforces strict tolerance by default", {
  # Nombres muy diferentes deben fallar con tolerancia por defecto
  expect_error(gd_clean_zone_name("completely_invalid_zone"), "could not be matched")
  
  # Pero deben funcionar con .on_error = "na"
  expect_true(is.na(gd_clean_zone_name("invalid_zone", .on_error = "na")))
  
  # Y con .on_error = "omit" deben devolver el input original
  expect_equal(gd_clean_zone_name("invalid_zone", .on_error = "omit"), "invalid_zone")
})

test_that("gd_clean_zone_name handles multiple names", {
  input_names <- c("urbana", "rural", "zona urbana")
  expected_names <- c("Urbana", "Rural", "Urbana")
  expect_equal(gd_clean_zone_name(input_names), expected_names)
})

test_that("gd_clean_zone_name handles NA values correctly", {
  expect_equal(gd_clean_zone_name(NA_character_), "_NA_")
  expect_equal(gd_clean_zone_name(c("urbana", NA_character_, "rural")), 
               c("Urbana", "_NA_", "Rural"))
})

test_that("gd_clean_zone_name respects tolerance parameter", {
  # Con tolerancia muy baja, debe fallar para nombres muy diferentes
  expect_error(gd_clean_zone_name("invalid_zone", .tol = 0.1), "could not be matched")
  
  # Con tolerancia alta y .on_error = "na", debe manejar nombres inválidos
  result <- gd_clean_zone_name("invalid_zone", .tol = 0.9, .on_error = "na")
  expect_true(is.character(result) && (is.na(result) || nchar(result) > 0))
})

test_that("gd_clean_zone_name handles empty input correctly", {
  expect_equal(gd_clean_zone_name(character(0)), character(0))
  # El string vacío causa error - usar .on_error = "na" para manejarlo
  expect_true(is.na(gd_clean_zone_name("", .on_error = "na")))
})

test_that("gd_clean_zone_name parameter validation", {
  # Verificar que los parámetros se validen correctamente
  expect_error(gd_clean_zone_name("urbana", .tol = -1), "debe ser un número")
  expect_error(gd_clean_zone_name("urbana", .tol = 2), "debe ser un número")
  expect_error(gd_clean_zone_name("urbana", .on_error = "invalid"), "debe ser uno de")
})
