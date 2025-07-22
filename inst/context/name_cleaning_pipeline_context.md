# Contexto general del pipeline de limpieza y normalización de nombres administrativos en GeoDOM

## Objetivo
Replicar y mejorar la robustez del proceso de limpieza y normalización de nombres administrativos (regiones, provincias, municipios, secciones, etc.) de `rgisDR` en `geodomR`, permitiendo que variantes, alias y errores comunes sean reconocidos y mapeados al nombre oficial.

## Componentes principales
- `.text_cleaning`: Función base que normaliza y limpia nombres administrativos (minúsculas, elimina tildes, artículos, preposiciones, prefijos como "provincia", "región", etc.).
- Funciones específicas por nivel administrativo (`dr_clean_region_name`, `dr_clean_prov_name`, `dr_clean_mun_name`, etc.) que aplican `.text_cleaning` y luego mapean al nombre oficial usando datasets de referencia.
- Datasets de referencia:
  - Para provincias: incluye múltiples variantes y alias por código (ejemplo: "Azua", "Azua de Compostela").
  - Para regiones: actualmente solo un nombre oficial, pero se planea agregar alias.
  - Para municipios, secciones, etc.: se sigue el mismo principio.

## Proceso general
1. **Limpieza básica:**
   - Se aplica `.text_cleaning` al nombre de entrada.
2. **Matching contra referencia:**
   - Se compara el nombre limpio contra una lista de nombres oficiales y alias (también limpios).
   - Si hay coincidencia, se retorna el nombre oficial.
   - Si no, se retorna el nombre limpio o se maneja como error según configuración.
3. **Robustez:**
   - El sistema debe reconocer variantes, errores comunes y alias, no solo el nombre oficial exacto.
   - Los datasets de referencia deben mantenerse actualizados y contener alias relevantes.

## Estado actual
- `.text_cleaning` y `dr_clean_region_name` implementados y testeados.
- Datasets de referencia para provincias robustos; para regiones en proceso de mejora.
- Tests unitarios cubren casos de variantes y alias.

## Problemas identificados
- Falta de alias en regiones y otros niveles limita la robustez.
- Algunos artículos ("el", "la") pueden ser parte del nombre oficial y deben preservarse en el matching final.

## Solución propuesta
- Crear y mantener archivos de alias para cada nivel administrativo (`region_aliases.csv`, `prov_aliases.csv`, etc.).
- Modificar las funciones de limpieza para usar estos archivos en el matching.
- Documentar el pipeline y sus decisiones en archivos de contexto.

## Próximos pasos
1. Completar y documentar los datasets de alias para todos los niveles.
2. Unificar la lógica de matching y manejo de errores en todas las funciones de limpieza.
3. Mantener este archivo actualizado como referencia para el desarrollo y mantenimiento del pipeline.

---

**Este archivo documenta el contexto, decisiones y próximos pasos para el pipeline de limpieza de nombres administrativos en GeoDOM.**
