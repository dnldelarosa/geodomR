# Tests para gd_clean_dm_name

# --- Coincidencias exactas ---

test_that("gd_clean_dm_name works with basic DM names (cabeceras)", {
  expect_equal(gd_clean_dm_name("Distrito Nacional"), "Distrito Nacional")
  expect_equal(gd_clean_dm_name("Azua"), "Azua")
  expect_equal(gd_clean_dm_name("Barahona"), "Barahona")
  expect_equal(gd_clean_dm_name("Los Alcarrizos"), "Los Alcarrizos")
})

test_that("gd_clean_dm_name works with actual DMs (non-cabeceras)", {
  expect_equal(gd_clean_dm_name("Barro Arriba"), "Barro Arriba")
  expect_equal(gd_clean_dm_name("Cabarete"), "Cabarete")
  expect_equal(gd_clean_dm_name("Los Jovillos"), "Los Jovillos")
})

test_that("gd_clean_dm_name handles case insensitivity", {
  expect_equal(gd_clean_dm_name("barro arriba"), "Barro Arriba")
  expect_equal(gd_clean_dm_name("CABARETE"), "Cabarete")
  expect_equal(gd_clean_dm_name("distrito nacional"), "Distrito Nacional")
})

# --- Manejo del sufijo (D. M.) ---

test_that("gd_clean_dm_name strips (D. M.) suffix correctly", {
  expect_equal(gd_clean_dm_name("Barro Arriba (D. M.)"), "Barro Arriba")
  expect_equal(gd_clean_dm_name("Cabarete (D. M.)"), "Cabarete")
  expect_equal(gd_clean_dm_name("Los Jovillos (D. M.)"), "Los Jovillos")
})

# --- Aliases semánticos ---

test_that("gd_clean_dm_name resolves semantic aliases", {
  # Nombre corto → oficial
  expect_equal(
    gd_clean_dm_name("Doña Emma Balaguer"),
    "Doña Emma Balaguer Viuda Vallejo"
  )
  # Componentes de nombres compuestos
  # "Las Barías" es ambiguo: DM 020103 (Azua) y 170110 (Peravia)
  expect_error(gd_clean_dm_name("Las Barías"), "ambiguous")
  # Desambiguar con .municipality
  expect_equal(gd_clean_dm_name("Las Barías", .municipality = "Azua"), "Las Barías-La Estancia")
  expect_equal(gd_clean_dm_name("Las Barías", .municipality = "Baní"), "Las Barías")
  expect_equal(gd_clean_dm_name("La Estancia"), "Las Barías-La Estancia")
})

test_that("gd_clean_dm_name resolves Verón/Punta Cana aliases", {
  expect_equal(gd_clean_dm_name("Verón Punta Cana"), "Verón Punta Cana")
  expect_equal(gd_clean_dm_name("Verón"), "Verón Punta Cana")
  expect_equal(gd_clean_dm_name("Punta Cana"), "Verón Punta Cana")
})

# --- Acentos ---

test_that("gd_clean_dm_name handles accented and unaccented input", {
  expect_equal(gd_clean_dm_name("galvan"), "Galván")
  expect_equal(gd_clean_dm_name("paraiso"), "Paraíso")
  expect_equal(gd_clean_dm_name("cabarete"), "Cabarete")
})

# --- Vectores múltiples ---

test_that("gd_clean_dm_name handles multiple names", {
  input <- c("barro arriba", "cabarete", "distrito nacional")
  expected <- c("Barro Arriba", "Cabarete", "Distrito Nacional")
  expect_equal(gd_clean_dm_name(input), expected)
})

# --- NA ---

test_that("gd_clean_dm_name handles NA values correctly", {
  expect_equal(gd_clean_dm_name(NA_character_), "_NA_")
  expect_equal(
    gd_clean_dm_name(c("barro arriba", NA_character_, "cabarete")),
    c("Barro Arriba", "_NA_", "Cabarete")
  )
})

# --- Manejo de errores ---

test_that("gd_clean_dm_name fails on unmatched names by default", {
  expect_error(
    gd_clean_dm_name("nombre_totalmente_invalido"),
    "could not be matched"
  )
})

test_that("gd_clean_dm_name .on_error = 'na' returns NA for unmatched", {
  result <- gd_clean_dm_name("nombre_totalmente_invalido", .on_error = "na")
  expect_true(is.na(result))
})

test_that("gd_clean_dm_name .on_error = 'omit' returns original for unmatched", {
  result <- gd_clean_dm_name("nombre_totalmente_invalido", .on_error = "omit")
  expect_equal(result, "nombre_totalmente_invalido")
})

# --- Validación de parámetros ---

test_that("gd_clean_dm_name validates parameters", {
  expect_error(gd_clean_dm_name("azua", .tol = -1), "debe ser un número")
  expect_error(gd_clean_dm_name("azua", .tol = 2), "debe ser un número")
  expect_error(gd_clean_dm_name("azua", .on_error = "invalid"), "debe ser uno de")
})

# --- Tolerancia ---

test_that("gd_clean_dm_name respects tolerance parameter", {
  # Con tolerancia muy baja, nombres con typos deben fallar
  expect_error(
    gd_clean_dm_name("baro ariba", .tol = 0.05),
    "could not be matched"
  )
  # Con tolerancia razonable, typos comunes deben resolverse
  result <- gd_clean_dm_name("baro ariba", .tol = 0.3, .on_error = "na")
  expect_true(is.character(result))
})

# --- Input vacío ---

test_that("gd_clean_dm_name handles empty input", {
  expect_equal(gd_clean_dm_name(character(0)), character(0))
})

# --- Fuzzy matching ---

test_that("gd_clean_dm_name fuzzy matching catches common typos", {
  # Typos comunes dentro de la tolerancia por defecto
  expect_equal(gd_clean_dm_name("cabaretee"), "Cabarete")
  expect_equal(gd_clean_dm_name("barahon"), "Barahona")
})
