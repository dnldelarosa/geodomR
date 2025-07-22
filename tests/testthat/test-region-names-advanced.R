# Tests para gd_clean_region_name

test_that("gd_clean_region_name works with basic region names", {
  expect_equal(gd_clean_region_name("cibao norte"), "Cibao Norte")
  expect_equal(gd_clean_region_name("VALDESIA"), "Valdesia") 
  expect_equal(gd_clean_region_name("ozama"), "Ozama")
})

test_that("gd_clean_region_name works with aliases", {
  expect_equal(gd_clean_region_name("norte"), "Cibao Norte")  # "norte" debe mapearse a Cibao Norte
  expect_equal(gd_clean_region_name("yuma"), "Del Yuma")  # "yuma" debe mapearse a Del Yuma
  expect_equal(gd_clean_region_name("valle"), "El Valle")  # "valle" debe mapearse a El Valle
  expect_equal(gd_clean_region_name("este"), "Del Yuma")  # "este" debe mapearse a Del Yuma
})

test_that("gd_clean_region_name correctly handles prefixes", {
  expect_equal(gd_clean_region_name("región cibao norte"), "Cibao Norte")
  expect_equal(gd_clean_region_name("Región Valdesia"), "Valdesia")
  expect_equal(gd_clean_region_name("REGION ENRIQUILLO"), "Enriquillo")
})

test_that("gd_clean_region_name works with partial names", {
  expect_equal(gd_clean_region_name("sur"), "Cibao Sur") 
  expect_equal(gd_clean_region_name("nordeste"), "Cibao Nordeste")
  expect_equal(gd_clean_region_name("noroeste"), "Cibao Noroeste")
  expect_equal(gd_clean_region_name("central"), "Cibao Sur") # Alias histórico
})

test_that("gd_clean_region_name handles tolerance correctly", {
  # Con tolerancia baja debe fallar
  expect_error(gd_clean_region_name("cibaooo", .tol = 0.1))
  
  # Con tolerancia alta debe funcionar - "cibaooo" es más similar a "Cibao Noroeste"
  expect_equal(gd_clean_region_name("cibaooo", .tol = 0.8), "Cibao Noroeste") 
})

test_that("gd_clean_region_name handles errors correctly", {
  # Con .on_error = "na" debe devolver NA para nombres no encontrados
  expect_equal(gd_clean_region_name("xyz_inexistente", .on_error = "na"), NA_character_)
  
  # Con .on_error = "omit" debe devolver el nombre original limpio
  expect_equal(gd_clean_region_name("xyz_inexistente", .on_error = "omit"), "xyz_inexistente")
})

test_that("gd_clean_region_name handles NA correctly", {
  expect_equal(gd_clean_region_name(NA), "_NA_")
  expect_equal(gd_clean_region_name(c("cibao norte", NA)), c("Cibao Norte", "_NA_"))
})

test_that("gd_clean_region_name works with historical aliases", {
  expect_equal(gd_clean_region_name("gran santo domingo"), "Ozama")
  expect_equal(gd_clean_region_name("metropolitana"), "Ozama") 
  expect_equal(gd_clean_region_name("cibao central"), "Cibao Sur")
})
