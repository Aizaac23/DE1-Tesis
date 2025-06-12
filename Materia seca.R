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

### ISAAC
library(agricolae)
modelo_MSB <- with(datos_materia,sp.plot(repeticion,niveles,Variedades,MS_B))
modelo_MSG <- with(datos_materia,sp.plot(repeticion,niveles,Variedades,MS_G))
modelo_MST <- with(datos_materia,sp.plot(repeticion,niveles,Variedades,MS_T))
# Response: MS_T
#                      Df    Sum Sq  Mean Sq  F value    Pr(>F)    
# repeticion           3  28932269  9644090  45.7505 4.422e-10 ***
#   niveles            3 179521622 59840541 132.2828 9.231e-08 ***
#   Ea                 9   4071315   452368                       
# Variedades           2  29209055 14604528  69.2822 1.072e-10 ***
#  niveles:Variedades  6   3989213   664869   3.1541   0.02002 *  
#   Eb                 24   5059143   210798                       


modelo_AOV_MST <- aov(valores ~ niveles * Variedades + Error(repeticion/Variedades), data = datos_materia)
summary(modelo_AOV_MST)
#### LO DE ABAJO NO HE TOCADO


modelo <- aov(MS_G ~ niveles * Variedades + Error(repeticion/Variedades), data = datos_materia)
summary(modelo)

glimpse(datos_materia)
modelo_aov %>% summary

modelo_aov$Within$residuals %>% shapiro.test()
# NO CUMPLE NORMALIDAD DE LAS VARIEDADES


# Usaremos un modelo con permutación  -------------------------------------

library(lmPerm)
modelo_perm<- lmp(MS_G~ repeticion + Variedades + niveles + 
                    Variedades:niveles, datos_materia)
modelo_perm %>% anova

library(permuco)

modelo_perm <- aovperm(MS_G ~ repeticion + Variedades*niveles + Error(repeticion/Variedades), 
                       data = datos_materia)

modelo_perm %>% anova
library(psych)
(medias  = describeBy(datos_materia$MS_G, datos_materia$Variedades))


