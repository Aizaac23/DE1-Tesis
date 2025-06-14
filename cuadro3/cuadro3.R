library(readxl)
library(dplyr)
library(purrr)

# 1) Leer el Excel sin nombres automáticos
cuadro3 <- read_excel("C:/Users/manol/Escritorio/parelas divididas/parcelas.xlsx",
                      col_names = FALSE)

# 2) Filas de datos (a partir de la 4) y columnas:
#    2 = repetición, 3 = niveles, 4 = variedades,
#    5 = Mg, 9 = Na, 13 = Fe, 17 = Cu, 21 = Mn, 25 = Zn
datos <- cuadro3[5:nrow(cuadro3),
                 c(2, 3, 4, 5, 9, 13, 17, 21, 25)]
View(datos)
# ── 1. Verifica estructura ──────────────────────────────────────────────
datos %>% glimpse()      # (opcional) inspecciona tipos y nombres “...n”

# ── 2. Renombra columnas por posición ───────────────────────────────────
datos %>% 
  rename(
    Variedades = ...4,
    niveles    = ...3,
    repeticion = ...2,
    Mg = ...5,     # total Mg
    Na = ...9,     # total Na
    Fe = ...13,    # total Fe
    Cu = ...17,    # total Cu
    Mn = ...21,    # total Mn
    Zn = ...25     # total Zn
  ) -> datos

# ── 3. Asegúrate de que nutrientes sean numéricos ───────────────────────
datos <- datos %>% 
  mutate(
    Mg = as.numeric(Mg),
    Na = as.numeric(Na),
    Fe = as.numeric(Fe),
    Cu = as.numeric(Cu),
    Mn = as.numeric(Mn),
    Zn = as.numeric(Zn)
  )

# ── 4. Crea factor “parcela” (whole-plot) ───────────────────────────────
parcela <- interaction(datos$repeticion, datos$niveles)

# ── 5. Ajusta los modelos ANOVA split-plot ──────────────────────────────
attach(datos)  # (idéntico a tu código original)

mod_Mg <- aov(Mg ~ repeticion + Error(parcela) + Variedades + niveles*Variedades, datos)
mod_Na <- aov(Na ~ repeticion + Error(parcela) + Variedades + niveles*Variedades, datos)
mod_Fe <- aov(Fe ~ repeticion + Error(parcela) + Variedades + niveles*Variedades, datos)
mod_Cu <- aov(Cu ~ repeticion + Error(parcela) + Variedades + niveles*Variedades, datos)
mod_Mn <- aov(Mn ~ repeticion + Error(parcela) + Variedades + niveles*Variedades, datos)
mod_Zn <- aov(Zn ~ repeticion + Error(parcela) + Variedades + niveles*Variedades, datos)

# ── 6. Resúmenes ────────────────────────────────────────────────────────
summary(mod_Mg)
summary(mod_Na)
summary(mod_Fe)
summary(mod_Cu)
summary(mod_Mn)
summary(mod_Zn)

