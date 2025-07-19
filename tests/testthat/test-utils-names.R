test_that(".text_cleaning normaliza y limpia correctamente", {
  expect_equal(
    .text_cleaning("Provincia de Azúa"),
    "azua"
  )
  expect_equal(
    .text_cleaning("Ayuntamiento de Santo Domingo (Zona urbana)"),
    "santo domingo"
  )
  expect_equal(
    .text_cleaning(NA),
    "_na_"
  )
  expect_equal(
    .text_cleaning("Municipio de San José de Ocoa"),
    "san jose ocoa"
  )
  expect_equal(
    .text_cleaning("El Seibo"),
    "seibo"
  )
  expect_equal(
    .text_cleaning("Los Alcarrizos"),
    "alcarrizos"
  )
  expect_equal(
    .text_cleaning("Azúa de Compostela"),
    "azua compostela"
  )
})
