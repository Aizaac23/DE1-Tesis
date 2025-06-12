# CODIGO QUE VERIFICA DATOS SUMAS ERRONEAS EN LOS DATOS 
# MS_T != G + MS_B + MS_J

library(dplyr)

noc = datos_materia %>%
  mutate(
    suma_MS = MS_G + MS_B + MS_J,
    coincide = MS_T == suma_MS
  ) %>%
  select(MS_T, MS_G, MS_B, MS_J, suma_MS, coincide)

print(noc, n = 1000)
datos_materia %>%
  mutate(
    suma_MS = MS_G + MS_B + MS_J,
    coincide = MS_T == suma_MS
  ) %>%
  filter(!coincide)
