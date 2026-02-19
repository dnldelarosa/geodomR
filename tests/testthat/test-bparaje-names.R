# Tests para gd_clean_bparaje_name

# --- Coincidencias exactas (nombres únicos) ---

test_that("gd_clean_bparaje_name works with unique BP names", {
  expect_equal(gd_clean_bparaje_name("Los Peralejos"), "Los Peralejos")
  expect_equal(gd_clean_bparaje_name("Palma Real"), "Palma Real")
  expect_equal(gd_clean_bparaje_name("Arroyo Manzano"), "Arroyo Manzano")
  expect_equal(gd_clean_bparaje_name("Los Cacicazgos"), "Los Cacicazgos")
})

test_that("gd_clean_bparaje_name handles case insensitivity", {
  expect_equal(gd_clean_bparaje_name("los peralejos"), "Los Peralejos")
  expect_equal(gd_clean_bparaje_name("PALMA REAL"), "Palma Real")
  expect_equal(gd_clean_bparaje_name("arroyo manzano"), "Arroyo Manzano")
})

# --- Códigos directos (11 dígitos) ---

test_that("gd_clean_bparaje_name resolves 11-digit BP codes", {
  expect_equal(gd_clean_bparaje_name("01010101001"), "Los Peralejos")
  expect_equal(gd_clean_bparaje_name("01010101002"), "Palma Real")
})

test_that("gd_clean_bparaje_name fails on invalid BP codes", {
  expect_error(
    gd_clean_bparaje_name("99999999999"),
    "not found"
  )
  result <- gd_clean_bparaje_name("99999999999", .on_error = "na")
  expect_true(is.na(result))
})

# --- Aliases (alternativas parentéticas y con "o") ---

test_that("gd_clean_bparaje_name resolves parenthetical aliases", {
  # "El Córbano (El Córvano)" tiene aliases "El Córbano", "El Córvano"
  expect_equal(
    gd_clean_bparaje_name("El Córbano (El Córvano)"),
    "El Córbano (El Córvano)"
  )
  # "El Córbano" solo es ambiguo: múltiples BPs con ese nombre
  expect_error(gd_clean_bparaje_name("El Córbano"), "ambiguous")
  # Con desambiguación por DM se resuelve (DM 020404 = Monte Bonito)
  expect_equal(
    gd_clean_bparaje_name("El Córbano", .dm = "Monte Bonito", .section = "El Córbano (El Córvano)"),
    "El Córbano (El Córvano)"
  )
  expect_equal(gd_clean_bparaje_name("El Córvano"), "El Córbano (El Córvano)")
})

test_that("gd_clean_bparaje_name resolves 'o' separator aliases", {
  # "Las Paredes o Los Paredones" → aliases "Las Paredes", "Los Paredones"
  expect_equal(
    gd_clean_bparaje_name("Las Paredes o Los Paredones"),
    "Las Paredes o Los Paredones"
  )
})

# --- Nombres ambiguos ---

test_that("gd_clean_bparaje_name fails on ambiguous names by default", {
  expect_error(
    gd_clean_bparaje_name("Centro del Pueblo"),
    "ambiguous"
  )
})

test_that("gd_clean_bparaje_name .on_error = 'na' returns NA for ambiguous", {
  result <- gd_clean_bparaje_name("Centro del Pueblo", .on_error = "na")
  expect_true(is.na(result))
})

# --- Desambiguación por padre ---

test_that("gd_clean_bparaje_name disambiguates by .section", {
  # "Centro del Pueblo" en sección "Clavellina" (02010801) → BP 02010801001
  result <- gd_clean_bparaje_name(
    "Centro del Pueblo",
    .section = "Clavellina"
  )
  expect_equal(result, "Centro del Pueblo")
})

# --- NA ---

test_that("gd_clean_bparaje_name handles NA values correctly", {
  expect_equal(gd_clean_bparaje_name(NA_character_), "_NA_")
  expect_equal(
    gd_clean_bparaje_name(c("Los Peralejos", NA_character_)),
    c("Los Peralejos", "_NA_")
  )
})

# --- Vectores múltiples ---

test_that("gd_clean_bparaje_name handles multiple names", {
  input <- c("los peralejos", "Palma Real", "Arroyo Manzano")
  expected <- c("Los Peralejos", "Palma Real", "Arroyo Manzano")
  expect_equal(gd_clean_bparaje_name(input), expected)
})

# --- Manejo de errores ---

test_that("gd_clean_bparaje_name fails on unmatched names by default", {
  expect_error(
    gd_clean_bparaje_name("nombre_totalmente_invalido_xyz123"),
    "could not be"
  )
})

test_that("gd_clean_bparaje_name .on_error = 'omit' returns original", {
  result <- gd_clean_bparaje_name("nombre_totalmente_invalido_xyz123", .on_error = "omit")
  expect_equal(result, "nombre_totalmente_invalido_xyz123")
})

# --- Validación de parámetros ---

test_that("gd_clean_bparaje_name validates parameters", {
  expect_error(gd_clean_bparaje_name("Palma Real", .tol = -1), "debe ser un número")
  expect_error(gd_clean_bparaje_name("Palma Real", .tol = 2), "debe ser un número")
  expect_error(gd_clean_bparaje_name("Palma Real", .on_error = "invalid"), "debe ser uno de")
})

# --- Tolerancia ---

test_that("gd_clean_bparaje_name respects tolerance parameter", {
  expect_error(
    gd_clean_bparaje_name("Palam Ral", .tol = 0.05),
    "could not be matched"
  )
  result <- gd_clean_bparaje_name("Palma Ral", .tol = 0.3, .on_error = "na")
  expect_true(is.character(result))
})

# --- Input vacío ---

test_that("gd_clean_bparaje_name handles empty input", {
  expect_equal(gd_clean_bparaje_name(character(0)), character(0))
})

# --- Acentos ---

test_that("gd_clean_bparaje_name handles accented and unaccented input", {
  expect_equal(gd_clean_bparaje_name("los rios"), "Los Ríos")
  expect_equal(gd_clean_bparaje_name("altos de arroyo hondo"), "Altos de Arroyo Hondo")
})
