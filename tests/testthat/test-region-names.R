test_that("gd_clean_region_name limpia y normaliza correctamente", {
  expect_equal(
    gd_clean_region_name("Región Ozama"),
    "Ozama"
  )
  expect_equal(
    gd_clean_region_name("Región Cibao Norte"),
    "Cibao Norte"
  )
  expect_equal(
    gd_clean_region_name("Región Valdesia"),
    "Valdesia"
  )
  expect_equal(
    gd_clean_region_name("Región Del Yuma"),  # Corrected: Del Yuma, not Yuma
    "Del Yuma"
  )
  expect_equal(
    gd_clean_region_name("Región Enriquillo"),
    "Enriquillo"
  )
  expect_equal(
    gd_clean_region_name("Región El Valle"),
    "El Valle"
  )
  expect_equal(
    gd_clean_region_name("Región Higuamo"),
    "Higuamo"
  )
  expect_equal(
    gd_clean_region_name("Región Cibao Sur"),  # Corrected: Sur, not Central
    "Cibao Sur"
  )
  expect_equal(
    gd_clean_region_name(NA),
    "_NA_"
  )
})
