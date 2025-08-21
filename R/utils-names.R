# Evitar NOTEs de variables globales en dplyr
utils::globalVariables(c(
  'PROV_ID', 'PROV_NAME', 'PROV_NAME_CLEAN', 'PROV_NAME_OFFICIAL', 'input_name', 'input_clean', '.',
  'distance', 'distance_norm', 'starts_with_input', 'input_starts_with_name', 'abbreviation_bonus',
  'length_penalty', 'total_score', 'match_type', 'reference_name', 'reference_clean', 'REGION_NAME_OFFICIAL',
  'ZONE_ID', 'ZONE_NAME', 'ZONE_NAME_OFFICIAL', 'MUN_NAME', 'MUN_NAME_CLEAN', 'MUN_NAME_OFFICIAL',
  'PROV_NAME_ALIAS', 'REGION_NAME_ALIAS', 'ZONE_NAME_ALIAS', 'MUN_NAME_ALIAS'
))

# Funciones de limpieza y normalización de nombres administrativos

# Función compartida de limpieza de texto para uso en todos los niveles administrativos
# Esta función replica la funcionalidad de .text_cleaning de rgisDR
# para su uso en geodomR. No se han realizado mejoras ni refactorizaciones.

.text_cleaning <- function(names) {
  # Asegura que el input sea character
  names <- as.character(names)
  # Reemplaza NA por '_na_' (con guiones bajos)
  names <- tidyr::replace_na(names, "_na_")
  # Limpieza básica
  names <- stringr::str_to_lower(names)
  names <- stringr::str_squish(names)
  # Normaliza tildes antes de eliminar prefijos
  names <- chartr("áéíóúüñ", "aeiouun", names)
  # Elimina palabras irrelevantes
  names <- stringr::str_remove(names, stringr::regex("^region[ ]?", ignore_case = TRUE))
  names <- stringr::str_remove(names, stringr::regex("^provincia[ ]?de[ ]?", ignore_case = TRUE))
  names <- stringr::str_remove(names, stringr::regex("^provincia[ ]?", ignore_case = TRUE))
  names <- stringr::str_remove(names, stringr::regex("^municipio[ ]?", ignore_case = TRUE))
  names <- stringr::str_remove(names, stringr::regex("^ayuntamiento[ ]?de[ ]?", ignore_case = TRUE))
  names <- stringr::str_remove(names, stringr::regex(" \\(d[.]?[ ]?m[.]?\\)", ignore_case = TRUE))
  names <- stringr::str_remove(names, stringr::regex(" [(]?zona urbana[)]?", ignore_case = TRUE))
  # Elimina artículos y preposiciones solo si están al inicio
  names <- stringr::str_remove(names, stringr::regex("^el[ ]", ignore_case = TRUE))
  names <- stringr::str_remove(names, stringr::regex("^la[s]?[ ]?", ignore_case = TRUE))
  names <- stringr::str_remove(names, stringr::regex("^los[ ]?", ignore_case = TRUE))
  names <- stringr::str_remove(names, stringr::regex("^de[l]?[ ]?", ignore_case = TRUE))
  # Elimina 'de' como palabra completa en cualquier posición
  names <- stringr::str_replace_all(names, stringr::regex("\\bde\\b", ignore_case = TRUE), "")
  names <- stringr::str_squish(names)
  # Solo elimina caracteres especiales si no es '_na_'
  names <- ifelse(names == "_na_", names, stringr::str_remove_all(names, stringr::regex("[^0-9a-z ]", ignore_case = TRUE)))
  names <- stringr::str_squish(names)
  names
}

# Ejemplo de uso:
# .text_cleaning(c("Provincia de Azúa", NA, "Ayuntamiento de Santo Domingo (Zona urbana)"))
