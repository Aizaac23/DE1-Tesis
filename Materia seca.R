library(dplyr)
library(readxl)
cuadros2 <- read_excel("Cuadros tipeados (1).xlsx", sheet = "Cuadro 2")

cuadros2 %>% attach()

cuadros2 %>% dplyr::select(2:9) -> datos_materia 
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
datos_materia$repeticion <- as.factor(datos_materia$repeticion)
datos_materia$Variedades <- as.factor(datos_materia$Variedades)
datos_materia$niveles <- as.factor(datos_materia$niveles)
glimpse(datos_materia)


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

# MATERIA SECA TOTAL ------------------------------------------------------------
parcela <- interaction(datos_materia$repeticion, datos_materia$niveles)

mod_MST<- aov(MS_T ~ repeticion + Error(parcela) + Variedades + niveles*Variedades, datos_materia)

mod_MST %>% summary()
shapiro.test(mod_MST$Within$residuals) -> Normalidad

if (Normalidad$p.value < 0.05) {
  print("Los residuos no son normales")
} else {
  print("Los residuos son normales")
}


# PRUEBAS DE COMPARACION --------------------------------------------------

library(agricolae)

parcela <- interaction(datos_materia$repeticion, datos_materia$niveles)

b<-nlevels(datos_materia$Variedades);b  #niveles de la variedades
a<-nlevels(datos_materia$niveles);a     # niveles de fertilidad del suelo  
r<-nlevels(datos_materia$repeticion);r  # Repeticiones 
dfa <- df.residual(mod_MSTaov$parcela)  # Grafos de libertad de la parcela 
dfa 

Ea <-deviance(mod_MSTaov$parcela)/dfa   # CME de los niveles 
Ea
dfb <- df.residual(mod_MSTaov$Within)   # Grados de libertad de subparcela 
dfb 
Eb <-deviance(mod_MSTaov$Within)/dfb    # CME de la sub subparcela 
Eb
Eab <- (Ea +(b-1)*Eb)/(b*r)             # CME de la interacción  
Eab

# Satterthwaite
dfab<-(Ea +(b-1)*Eb)^2/(Ea^2/dfa +((b-1)*Eb)^2/dfb)
# Comparison niveles, niveles(1), niveles(2), niveles(3)
comparison1 <-with(datos_materia,LSD.test(MS_T,niveles,dfa,Ea))
comparison2 <-with(datos_materia,LSD.test(MS_T[Variedades=="1"],niveles[Variedades=="1"],dfab,Eab))
comparison3 <-with(datos_materia,LSD.test(MS_T[Variedades=="2"],niveles[Variedades=="2"],dfab,Eab))
comparison4 <-with(datos_materia,LSD.test(MS_T[Variedades=="3"],niveles[Variedades=="3"],dfab,Eab))
# Comparison Variedades, Variedades(1), Variedades(2)
comparison5 <-with(datos_materia,LSD.test(MS_T,Variedades,dfb,Eb))
comparison6 <-with(datos_materia,LSD.test(MS_T[niveles=="1"],Variedades[niveles=="1"],dfb,Eb))
comparison7 <-with(datos_materia,LSD.test(MS_T[niveles=="2"],Variedades[niveles=="2"],dfb,Eb))

# Hacemos las pruebas de comparación, pero dado que no cumple normalidad, no tiene sustento estadístico para seguir, por ello


# Método no paramétrico-----------------------------------

library(nparLD)

# Crear identificador único de parcela principal

datos_materia$parcela_id <- interaction(datos_materia$repeticion, datos_materia$niveles)

library(nparLD)
modelo_np <- nparLD(
  formula = MS_T ~ niveles * Variedades,
  data = datos_materia,
  subject = datos_materia$parcela_id,     # Identificador de unidad experimental principal
  description = F,         # Mostrar descripción del diseño
  plot.CI = TRUE,             # Generar gráficos con IC
  show.covariance = FALSE,    # No mostrar matriz de covarianzas (opcional)
  time1.order = levels(datos_materia$Variedades),  # Orden de subparcelas
  group1.order = levels(datos_materia$niveles)     # Orden de parcelas principales
)


modelo_np %>% summary -> resultados

#Anova no paramétrico 

Anova_np <- resultados$ANOVA.test.time; Anova_np

# Gráfico de interacción detallado
plot(modelo_np, 
     main = "Interacción Niveles × Variedades",
     xlab = "Variedades (Subparcela)",
     ylab = "Efecto Relativo (MS_T)",
     factor.names = c("Niveles", "Variedades"))
# EFecto principal de la parcela principal (niveles)
# p-valor = 0.0523 (No significativo)

# Efecto principal de la subparcela (Variedades)
# p-valor = 0.00017 (Significativo)

# Efecto de la interacción niveles*variedades 
# p-valor = 2.97e-12 (Altamente significativa) 

# EFecto de tiempo dentro de grupos (interacción específica)
# p-valor = 2.72e-07 (Significativo)

#Dado que la interacción salió significativa, el objetivo será encontrar la mejor interacción que aumente el rendimiento de materia seca 



# ANÁLISIS DESCRIPTIVO ----------------------------------------------------

 
library(ggplot2)
ggplot(datos_materia, aes(x = interaction(niveles, Variedades), y = MS_T)) +
  geom_boxplot(fill = "lightblue") +
  labs(title = "Rendimiento de las mejores combinaciones", x = "Combinación") +
  theme(axis.text.x = element_text(angle = 90))

#rendimiento de las mejores  combinaciones a simple vista podemos ver que 4.3 es la mejor, pero haremos prueba de comparaciones

medias <- datos_materia %>% group_by(niveles, Variedades) %>% 
  summarise(Media_MS_T = mean(MS_T),SD = sd(MS_T), n = n()) %>%
  arrange(desc(Media_MS_T))
medias

ggplot(medias, aes(x = interaction(niveles, Variedades), y = Media_MS_T, fill = niveles)) +
  geom_col(width = 0.7) +
  geom_errorbar(aes(ymin = Media_MS_T - SD, ymax = Media_MS_T + SD), width = 0.2) +
  labs(
    title = "Rendimiento (MS_T) por combinación de Nivel y Variedad",
    x = "Combinación (Nivel.Variedad)",
    y = "Media de MS_T"
  ) +
  theme_minimal()

# Podemos observar que el nivel 4 con variedad 3 es la mejor 


# Pruebas de comparación 

#"Comparaciones por pares de la interacción Niveles × Variedades"
#(Efectos simples no paramétricos, método Tukey)

library(nparcomp)
datos_materia$combinacion <- interaction(datos_materia$niveles, datos_materia$Variedades)
comp_interaccion <- nparcomp(MS_T ~ combinacion, 
                             data = datos_materia,
                             type = "Tukey",
                             asy.method = "mult.t")
summary(comp_interaccion)

comp_interaccion$Analysis

comparaciones_4.3 <- comp_interaccion$Analysis %>% 
  filter(grepl("4.3", Comparison)); comparaciones_4.3

# Estimator = 0.999 (casi 1) cuando 4.3 es el segundo elemento (p(1.1, 4.3)).

#Esto significa que hay un 99.9% de probabilidad de que 4.3 sea mayor que 1.1.



