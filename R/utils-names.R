# Funciones de limpieza y normalización de nombres administrativos
#
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
  # Elimina palabras irrelevantes
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
  # Elimina caracteres especiales y tildes, pero preserva guion bajo para '_na_'
  names <- chartr("áéíóúüñ", "aeiouun", names)
  # Solo elimina caracteres especiales si no es '_na_'
  names <- ifelse(names == "_na_", names, stringr::str_remove_all(names, stringr::regex("[^0-9a-z ]", ignore_case = TRUE)))
  names <- stringr::str_squish(names)
  names
}

# Ejemplo de uso:
# .text_cleaning(c("Provincia de Azúa", NA, "Ayuntamiento de Santo Domingo (Zona urbana)"))
