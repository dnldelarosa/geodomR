# Tests para gd_clean_prov_name

test_that("gd_clean_prov_name works with basic province names", {
  # Casos básicos
  expect_equal(gd_clean_prov_name("azua"), "Azua")
  expect_equal(gd_clean_prov_name("BARAHONA"), "Barahona")
  expect_equal(gd_clean_prov_name("distrito nacional"), "Distrito Nacional")
})

test_that("gd_clean_prov_name enforces strict tolerance by default", {
  # Abreviaciones deben fallar con tolerancia por defecto para evitar matches incorrectos
  expect_error(gd_clean_prov_name("stgo"), "could not be matched")
  expect_error(gd_clean_prov_name("rod"), "could not be matched")
  # "srodriguez" ahora coincide via JW (dist 0.23 < 0.25 threshold) — es abreviación legítima
  expect_equal(gd_clean_prov_name("srodriguez"), "Santiago Rodríguez")
  
  # Pero deben funcionar con .on_error = "na"
  expect_true(is.na(gd_clean_prov_name("stgo", .on_error = "na")))
  expect_true(is.na(gd_clean_prov_name("rod", .on_error = "na")))
  
  # Y con .on_error = "omit" deben devolver el input original
  expect_equal(gd_clean_prov_name("stgo", .on_error = "omit"), "stgo")
  expect_equal(gd_clean_prov_name("rod", .on_error = "omit"), "rod")
})

test_that("gd_clean_prov_name works with complete names (no abbreviations)", {
  # Casos que deben mapearse a "Santiago" - nombres completos funcionan con tolerancia por defecto
  expect_equal(gd_clean_prov_name("santiago"), "Santiago")
  expect_equal(gd_clean_prov_name("santi"), "Santiago")
  expect_equal(gd_clean_prov_name("sant"), "Santiago")
  expect_equal(gd_clean_prov_name("santiago de los caballeros"), "Santiago")
  
  # Casos que deben mapearse a "Santiago Rodríguez" - nombres completos funcionan con tolerancia por defecto
  expect_equal(gd_clean_prov_name("santiago rodriguez"), "Santiago Rodríguez")
  expect_equal(gd_clean_prov_name("santiago rod"), "Santiago Rodríguez")
})

test_that("gd_clean_prov_name handles multiple names", {
  input_names <- c("azua", "barahona", "monte plata")
  expected_names <- c("Azua", "Barahona", "Monte Plata")
  expect_equal(gd_clean_prov_name(input_names), expected_names)
})

test_that("gd_clean_prov_name handles NA values correctly", {
  expect_equal(gd_clean_prov_name(NA_character_), "_NA_")
  expect_equal(gd_clean_prov_name(c("azua", NA_character_, "barahona")), 
               c("Azua", "_NA_", "Barahona"))
})

test_that("gd_clean_prov_name respects tolerance parameter", {
  # Con tolerancia muy baja, debe fallar para nombres muy diferentes
  expect_error(gd_clean_prov_name("xxxxx", .tol = 0.05, .on_error = "fail"))
  
  # Con tolerancia baja y on_error = "na", debe retornar NA
  result <- gd_clean_prov_name("xxxxx", .tol = 0.05, .on_error = "na")
  expect_true(is.na(result))
})

test_that("gd_clean_prov_name handles error modes correctly", {
  # Usar un nombre que sabemos da una distancia alta
  problem_name <- "zzzxxx"  # Un nombre que claramente no existe
  
  # Modo "na" debe retornar NA cuando la distancia excede la tolerancia
  result_na <- gd_clean_prov_name(problem_name, .tol = 0.05, .on_error = "na")
  expect_true(is.na(result_na))
  
  # Modo "omit" debe retornar el nombre original cuando la distancia excede la tolerancia
  result_omit <- gd_clean_prov_name(problem_name, .tol = 0.05, .on_error = "omit")
  expect_equal(result_omit, problem_name)
  
  # Modo "fail" debe generar error cuando la distancia excede la tolerancia
  expect_error(gd_clean_prov_name(problem_name, .tol = 0.05, .on_error = "fail"))
})

test_that("gd_clean_prov_name works with prefixes", {
  # Debe manejar prefijos como "provincia de" - el bug original que se arregló
  expect_equal(gd_clean_prov_name("provincia de azua"), "Azua")
  expect_equal(gd_clean_prov_name("Provincia de Santiago"), "Santiago")
  expect_equal(gd_clean_prov_name("Provincia de El Seibo"), "El Seibo")  # El caso específico del bug
})

test_that("gd_clean_prov_name enforces strict tolerance by default", {
  # Abreviaciones deben fallar con tolerancia por defecto para evitar matches incorrectos
  expect_error(gd_clean_prov_name("stgo"), "could not be matched")
  expect_error(gd_clean_prov_name("rod"), "could not be matched")
  
  # Pero deben funcionar con .on_error = "na"
  expect_true(is.na(gd_clean_prov_name("stgo", .on_error = "na")))
  expect_true(is.na(gd_clean_prov_name("rod", .on_error = "na")))
  
  # Y con .on_error = "omit" deben devolver el input original
  expect_equal(gd_clean_prov_name("stgo", .on_error = "omit"), "stgo")
  expect_equal(gd_clean_prov_name("rod", .on_error = "omit"), "rod")
})

test_that("gd_clean_prov_name handles empty strings", {
  # Cadenas vacías ahora se tratan como error (antes mapeaban por bug en startsWith)
  expect_error(gd_clean_prov_name(""), "empty")
  expect_true(is.na(gd_clean_prov_name("", .on_error = "na")))
})
