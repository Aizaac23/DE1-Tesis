library(dplyr)
library(readxl)
cuadros2 <- read_excel("Tesis Diseños/Cuadros tipeados (1).xlsx", sheet = "Cuadro 2")
cuadros2 %>% attach()

cuadros2 %>% select(2:9) -> datos_materia 
datos_materia <- datos_materia[4:nrow(datos_materia),]
datos_materia %>% glimpse()
datos_materia
names(datos_materia)
datos_materia %>% rename( MS_T = MS...5 , MS_G =MS...6, MS_B = MS...7, MS_J = MS...8,
                          MS_RI = MS...9, Variedades = ...4, niveles = ...3, repeticion = ...2) -> datos_materia
datos_materia
datos_materia$MS_T <- as.numeric(datos_materia$MS_T)
datos_materia$MS_G <- as.numeric(datos_materia$MS_G)
datos_materia$MS_B <- as.numeric(datos_materia$MS_B)
datos_materia$MS_J <- as.numeric(datos_materia$MS_J)
datos_materia$MS_RI <- as.numeric(datos_materia$MS_RI)
glimpse(datos_materia)
head(datos_materia)
summary(datos_materia)




# Materia Seca Total------------------------------------------------------------



# Descripción de las variables --------------------------------------------


# Factor de interés -------------------------------------------------------

# Niveles nombre en R (parcela principal )

# Parcelas Principales (Main Plots): Constituidas por los niveles de fertilidad del suelo. 
# Se aplicaron cuatro tratamientos de fertilización NPK:

#D1: 00-00-00 kg/Ha de N-P₂O₅-K₂O

#D2: 60-40-30 kg/Ha de N-P₂O₅-K₂O

#D3: 90-60-45 kg/Ha de N-P₂O₅-K₂O

#D4: 120-80-60 kg/Ha de N-P₂O₅-K₂O


# bloque secundario o sub parcelas ----------------------------------------

# Variedades 

# Subparcelas (Subplots): Constituidas por las variedades de quinua. Se estudiaron tres variedades:

# 'Sajama' (precoz y dulce)

# 'Blanca de Juli' (semitardía y semiprecoz)

# 'Tahuaco' (semitardía y semiprecoz)

# Factores (Variables Independientes):

# Nivel de fertilidad (factor de parcela principal con 4 niveles).

# Variedad de quinua (factor de subparcela con 3 niveles


# Variable respuesta ------------------------------------------------------


# Variables de Respuesta (Variables Dependientes):

# Rendimiento y Componentes: Se registraron a la cosecha.

# Rendimiento de grano (kg/ha).

# Rendimiento de broza (kg/ha). La broza se compone de tallos y parte de la panoja.

# Rendimiento de "jipi" (kg/ha). El "jipi" se compone del perigonio del fruto y partes menudas de tallos, hojas y panojas.

#Rendimiento total de materia seca (kg/ha)



# bloqueo  ----------------------------------------------------------------

# Repetición
#La fuente describe físicamente los bloques en el campo, indicando que el área experimental
#se dividió en secciones o grupos llamados bloques, con dimensiones específicas,
#y muestra un gráfico de su disposición (GRAFICO 03). Se identificaron como Bloque I,
#Bloque II, Bloque III y Bloque IV en el gráfico


# MATERIA SECA ------------------------------------------------------------


library(agricolae)
modelo_MSB <- with(datos_materia,sp.plot(repeticion,niveles,Variedades,MS_B))
modelo_MSG <- with(datos_materia,sp.plot(repeticion,niveles,Variedades,MS_G))
modelo_MST <- with(datos_materia,sp.plot(repeticion,niveles,Variedades,MS_T))
modelo_MSJ <- with(datos_materia,sp.plot(repeticion, niveles,Variedades, MS_J))

# Usaremos aov para extraer los residuos


# NORMALIDAD --------------------------------------------------------------


# MATERIA SECA TOTAL ------------------------------------------------------


mod_MSTaov = aov(MS_T ~ repeticion + Variedades + Error(repeticion/Variedades) +
                   niveles + Variedades:niveles, datos_materia)
mod_MSTaov %>% summary
mod_MSTaov$Within %>% residuals() %>% shapiro.test()


# MATERIA SECA CON GRANO --------------------------------------------------

mod_MSGaov = aov(MS_G ~ repeticion + Variedades + Error(repeticion/Variedades) +
                   niveles + Variedades:niveles, datos_materia)
mod_MSGaov$Within %>% residuals() %>% shapiro.test()


# MATERIA SECA BROZA ------------------------------------------------------

mod_MSBaov = aov(MS_B ~ repeticion + Variedades + Error(repeticion/Variedades) +
                   niveles + Variedades:niveles, datos_materia)
mod_MSBaov$Within %>% residuals() %>% shapiro.test()


# MATERIA SECA JIPI -------------------------------------------------------

mod_MSJaov = aov(MS_J ~ repeticion + Variedades + Error(repeticion/Variedades) +
                   niveles + Variedades:niveles, datos_materia)
mod_MSJaov$Within %>% residuals() %>% shapiro.test()

# MATERIA SECA RAÍZ -------------------------------------------------------

mod_MSRIaov = aov(MS_RI ~ repeticion + Variedades + Error(repeticion/Variedades) +
                   niveles + Variedades:niveles, datos_materia)
mod_MSRIaov$Within %>% residuals() %>% shapiro.test()


# HOMOGENEIDAD ------------------------------------------------------------
plot(fitted(mod_MSTaov$Within), residuals(mod_MSTaov$Within),
     xlab = "Valores ajustados", ylab = "Residuos")

plot(fitted(mod_MSJaov$Within), residuals(mod_MSJaov$Within),
     xlab = "Valores ajustados", ylab = "Residuos")

plot(fitted(mod_MSGaov$Within), residuals(mod_MSGaov$Within),
     xlab = "Valores ajustados", ylab = "Residuos")

plot(fitted(mod_MSBaov$Within), residuals(mod_MSBaov$Within),
     xlab = "Valores ajustados", ylab = "Residuos")

plot(fitted(mod_MSRIaov$Within), residuals(mod_MSRIaov$Within),
     xlab = "Valores ajustados", ylab = "Residuos")

library(lmtest)

bptest(mod_MSTaov$Within %>% residuals() ~ fitted(mod_MSTaov$Within), data = datos_materia)

bptest(mod_MSGaov$Within %>% residuals() ~ fitted(mod_MSGaov$Within), data = datos_materia)

bptest(mod_MSJaov$Within %>% residuals() ~ fitted(mod_MSJaov$Within), data = datos_materia)

bptest(mod_MSBaov$Within %>% residuals() ~ fitted(mod_MSBaov$Within), data = datos_materia)

bptest(mod_MSRIaov$Within %>% residuals() ~ fitted(mod_MSRIaov$Within), data = datos_materia)


# PRUEBAS DE COMPARACION --------------------------------------------------

library(agricolae)
resumen <- summary(mod_MSTaov)

# =======================
# === 1. Parcela grande (Variedades)
# =======================
tabla_grande <- resumen[["Error: repeticion:Variedades"]][[1]]

(dfa <- tabla_grande["Residuals", "Df"])
(Ea  <- tabla_grande["Residuals", "Mean Sq"])


# =======================
# === 2. Subparcelas (niveles y Variedades:niveles)
# =======================
tabla_within <- resumen[["Error: Within"]][[1]]

(dfb <- tabla_within["Residuals", "Df"])
(Eb  <- tabla_within["Residuals", "Mean Sq"])

# =======================
# === 3. Niveles de factores
# =======================
(a <- nlevels(datos_materia$Variedades))
(b <- nlevels(datos_materia$niveles))
(r <- nlevels(datos_materia$repeticion))

# =======================
# === 4. Error combinado y gl ajustado (Satterthwaite)
# =======================

#Aproximación 
(Eab <- (Ea + (b - 1) * Eb) / (b * r))
(dfab <- (Ea + (b - 1) * Eb)^2 / ((Ea^2 / dfa) + (((b - 1)^2) * Eb^2 / dfb)))

# =======================
# === 5. Comparaciones HSD
# =======================

# Variedades (parcela grande)
(comparacion_variedades <- HSD.test(y = datos_materia$MS_T, trt = datos_materia$Variedades,
                                   DFerror = dfa, MSerror = Ea, alpha = 0.05, console = T) %>% plot)


# Niveles (subparcela)
comparacion_niveles <- HSD.test(y = datos_materia$MS_T,
                                trt = datos_materia$niveles,
                                DFerror = dfb, MSerror = Eb, alpha = 0.05, console = T) %>% plot
