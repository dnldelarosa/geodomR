# Tests para gd_clean_section_name

# --- Coincidencias exactas ---

test_that("gd_clean_section_name works with cabeceras (zona urbana sections)", {
  expect_equal(gd_clean_section_name("Santo Domingo de Guzmán"), "Santo Domingo de Guzmán")
  expect_equal(gd_clean_section_name("Azua de Compostela"), "Azua de Compostela")
  expect_equal(gd_clean_section_name("Jimaní"), "Jimaní")
})

test_that("gd_clean_section_name works with regular sections", {
  expect_equal(gd_clean_section_name("Barro Arriba"), "Barro Arriba")
  expect_equal(gd_clean_section_name("Villa Corazón de Jesús"), "Villa Corazón de Jesús")
  expect_equal(gd_clean_section_name("Barro en Medio"), "Barro en Medio")
})

test_that("gd_clean_section_name handles case insensitivity", {
  expect_equal(gd_clean_section_name("barro arriba"), "Barro Arriba")
  expect_equal(gd_clean_section_name("AZUA DE COMPOSTELA"), "Azua de Compostela")
  expect_equal(gd_clean_section_name("santo domingo de guzman"), "Santo Domingo de Guzmán")
})

# --- Manejo del sufijo (Zona urbana) ---

test_that("gd_clean_section_name strips (Zona urbana) suffix correctly", {
  expect_equal(
    gd_clean_section_name("Santo Domingo de Guzmán (Zona urbana)"),
    "Santo Domingo de Guzmán"
  )
  expect_equal(
    gd_clean_section_name("Barro Arriba (Zona urbana)"),
    "Barro Arriba"
  )
  expect_equal(
    gd_clean_section_name("Azua de Compostela (Zona urbana)"),
    "Azua de Compostela"
  )
})

# --- Aliases semánticos ---

test_that("gd_clean_section_name resolves short aliases", {
  # "Azua" es alias de "Azua de Compostela"
  expect_equal(gd_clean_section_name("Azua"), "Azua de Compostela")
})

test_that("gd_clean_section_name resolves parenthetical alternatives", {
  # "El Rodeo (Majagual)" tiene aliases "El Rodeo" y "Majagual"
  expect_equal(gd_clean_section_name("El Rodeo (Majagual)"), "El Rodeo (Majagual)")
  # "El Rodeo" solo es ambiguo: 3 secciones con ese nombre
  expect_error(gd_clean_section_name("El Rodeo"), "ambiguous")
  # Con desambiguación por municipio se resuelve (mun 0302 = Galván)
  expect_equal(
    gd_clean_section_name("El Rodeo", .municipality = "Galván"),
    "El Rodeo (Majagual)"
  )
  # "Majagual" es ambiguo: 3 secciones con ese nombre
  expect_error(gd_clean_section_name("Majagual"), "ambiguous")
})

# --- Acentos ---

test_that("gd_clean_section_name handles accented and unaccented input", {
  expect_equal(gd_clean_section_name("jimani"), "Jimaní")
  expect_equal(gd_clean_section_name("loma de cabrera"), "Loma de Cabrera")
})

# --- Vectores múltiples ---

test_that("gd_clean_section_name handles multiple names", {
  input <- c("barro arriba", "Azua de Compostela", "jimani")
  expected <- c("Barro Arriba", "Azua de Compostela", "Jimaní")
  expect_equal(gd_clean_section_name(input), expected)
})

# --- NA ---

test_that("gd_clean_section_name handles NA values correctly", {
  expect_equal(gd_clean_section_name(NA_character_), "_NA_")
  expect_equal(
    gd_clean_section_name(c("barro arriba", NA_character_, "jimani")),
    c("Barro Arriba", "_NA_", "Jimaní")
  )
})

# --- Manejo de errores ---

test_that("gd_clean_section_name fails on unmatched names by default", {
  expect_error(
    gd_clean_section_name("nombre_totalmente_invalido"),
    "could not be matched"
  )
})

test_that("gd_clean_section_name .on_error = 'na' returns NA for unmatched", {
  result <- gd_clean_section_name("nombre_totalmente_invalido", .on_error = "na")
  expect_true(is.na(result))
})

test_that("gd_clean_section_name .on_error = 'omit' returns original for unmatched", {
  result <- gd_clean_section_name("nombre_totalmente_invalido", .on_error = "omit")
  expect_equal(result, "nombre_totalmente_invalido")
})

# --- Validación de parámetros ---

test_that("gd_clean_section_name validates parameters", {
  expect_error(gd_clean_section_name("azua", .tol = -1), "debe ser un número")
  expect_error(gd_clean_section_name("azua", .tol = 2), "debe ser un número")
  expect_error(gd_clean_section_name("azua", .on_error = "invalid"), "debe ser uno de")
})

# --- Tolerancia ---

test_that("gd_clean_section_name respects tolerance parameter", {
  # Con tolerancia muy baja, nombres con typos deben fallar
  expect_error(
    gd_clean_section_name("baro ariba", .tol = 0.05),
    "could not be matched"
  )
  # Con tolerancia razonable, typos comunes deben resolverse
  result <- gd_clean_section_name("baro ariba", .tol = 0.3, .on_error = "na")
  expect_true(is.character(result))
})

# --- Input vacío ---

test_that("gd_clean_section_name handles empty input", {
  expect_equal(gd_clean_section_name(character(0)), character(0))
})

# --- Fuzzy matching ---

test_that("gd_clean_section_name fuzzy matching catches common typos", {
  # "Loma de Cabrra" es demasiado similar a "La Loma" — usar desambiguación
  expect_equal(
    gd_clean_section_name("Loma de Cabrra", .municipality = "Loma de Cabrera"),
    "Loma de Cabrera"
  )
  expect_equal(gd_clean_section_name("Barrro Arriba"), "Barro Arriba")
})
