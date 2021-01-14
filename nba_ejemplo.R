# Ejemplo de NBA

# En este script se muestra como resolver las preguntas
# correspondientes al ejercicio que realizamos en clase

# Como primer paso usamos [setwd] para indicar en que folder 
# de nuestra computadora estaremos trabajando. En este folder
# debera estar la base de datos en formato Stata que utilizaremos
# Ojo: utilizar las diagonales en direccion correcta [/]
setwd("C:/Users/aaeag/Dropbox (Personal)/ITAM-Clases/Microeconometria_aplicada/Intro Stata y R/Ejercicios R para clase/MCO")

# Aqui cargamos las librerias relevantes para este ejemplo
# Para la tarea se recomienda cargar estas librerias. Ver la ppt
# de introduccion, recuerden que las librerias deben haber sido
# instaladas antes utilizando el comando [install.packages]
library(haven)
library(tidyverse)
library(stats)
library(stargazer)
library(car)
library(sandwich)

#Se carga la base de datos
datos_nba <- read_dta("nba_salaries_12.dta")
datos_nba <- as.data.frame(datos_nba)

# Usualmente es buena idea ver la estructura de la base de
# datos incluyendo los nombres de variables para empezar
# a trabajar con ellas. Utilicen [str() o glimpse()] para
# ver la estructura de la base o [names()] para ver el
# nombre de las variables
str(datos_nba)

# Pregunta 1: Analisis exploratorio. Las tablas se crean con el
# paquete [stargazer] que debe ser instalado y pre-cargado
stargazer(datos_nba, type = "text", title="Estadisticas Descriptivas", digits=1, out="table1.txt")

#La anterior pregunta tambien se puede hacer con el comando [summary]
summary(datos_nba)

# Para mayor detalle en el uso del comando [ggplot] y [mutate] se
# recomienda ver el curso de "Tidyverse" en DataCamp y las guias que
# estan disponibles en Canvas > Intro Stata y R > Cheatsheets

# Histograma
ggplot(datos_nba,aes(x=wage)) + geom_histogram()

#Scatterplot
ggplot(datos_nba,aes(x=exper,y=wage)) + geom_point()

#Generar nuevas variables 
datos_nba <- datos_nba %>% mutate(ln_wage = log(wage),
                                  sq_wage = wage^2,
                                  old = as.factor(age > 27))

# Pregunta 2: Regresiones
#Desactivar la notacion cientifica
options(scipen=999)
#Especificacion 1
reg1 <- lm(wage~ age, datos_nba)
se1 <- sqrt(diag(vcovHC(reg1,type="HC1")))
#Especificacion 2
reg2 <- lm(wage~ age + exper,datos_nba)
se2 <- sqrt(diag(vcovHC(reg2,type="HC1")))
#Especificacion 3
reg3 <- lm(wage~ age + exper + points + rebounds + assists +
              black + log(avgmin) + center + forward,datos_nba)
se3 <- sqrt(diag(vcovHC(reg3,type="HC1")))
#Especificacion 4
reg4 <- lm(log(wage)~ age + exper + points + rebounds + assists + 
                black + log(avgmin) + center + forward,datos_nba)
se4 <- sqrt(diag(vcovHC(reg4,type="HC1")))

#Producir la tabla usando los resultados
stargazer(reg1, reg2, reg3, reg4, type="text",
          dep.var.labels=c("wage","log(wage)"), out="tabla1reg.txt",
          se = list(se1,se2,se3,se4))

#Pregunta 5
#Agregar Guard como variable explicativa
reg5 <- lm(log(wage)~ age + exper + points + rebounds + assists + 
                black + log(avgmin) + center + forward + guard,datos_nba)
summary(reg5)

#Pregunta 6
ggplot(datos_nba,aes(x=points,y=log(wage))) + geom_point() + geom_smooth(method="lm")


# Aside: como hacer una grafica para tres grupos, cada uno con su propia regresion
# Creo grupos por posicion del jugador. Es importante que esta variable sea de tipo "factor" para ggplot
datos_nba <- datos_nba %>% mutate(posicion = factor(1*guard+2*forward+3*center, labels = c("guard","forward","center")))

# Genero la grafica. La opcion se=FALSE me permite quitar los errores estandar de la linea
# Ojo: esta NO es la estrategia para las lineas paralelas
ggplot(datos_nba,aes(x=points,y=log(wage),color=posicion)) + geom_point() + geom_smooth(method="lm",se = FALSE)


#Pregunta 7
# Agregar las variables no lineales
datos_nba <- datos_nba %>% mutate(points2 = points^2, points3 = points^3)

#Especificacion
reg_p7 <- lm(log(wage)~ age + exper + points + points2 + points3 + 
                  rebounds + assists + black + log(avgmin) + 
                  center + forward ,datos_nba)
se7 <- sqrt(diag(vcovHC(reg_p7,type="HC1")))

#Prueba de hipotesis multidimensional
linearHypothesis(reg_p7,c("points2=0","points3=0"),white.adjust="hc1")

#Pregunta 9
# Inciso a
datos_nba <- datos_nba %>% mutate(guard_black = guard * black)
reg_p9a <- lm(log(wage)~ age + exper + points + rebounds + 
                   assists + log(avgmin) + black + guard + guard_black,
                   data=datos_nba)
se9a <- sqrt(diag(vcovHC(reg_p9a,type="HC1")))
l_vec= c(0,0,0,0,0,0,0,1,0,1)
linearHypothesis(reg_p9a,l_vec,white.adjust="hc1")

# Inciso b
reg_p9b <- lm(log(wage)~ age + exper + points + rebounds + assists + 
                   log(avgmin) + guard_black,datos_nba)
se9b <- sqrt(diag(vcovHC(reg_p9b,type="HC1")))
l_vecb= c(0,0,0,0,0,0,0,1)
linearHypothesis(reg_p9b,l_vecb,white.adjust="hc1")

#Pregunta 11
datos_nba <- datos_nba %>% mutate(center_reb = center * rebounds,
                                  guard_reb = guard * rebounds)
reg_p11 <- lm(log(wage)~ age + exper + points + rebounds + assists + 
                   log(avgmin) + black + center + guard + 
                   center_reb + guard_reb,datos_nba)
se11 <- sqrt(diag(vcovHC(reg_p11,type="HC1")))

# Tabla con las ultimas estimaciones
stargazer(reg_p7, reg_p9a, reg_p9b, reg_p11, type="text",
          dep.var.labels=c("log(wage)"), out="tabla2reg.txt",
          se = list(se7,se9a,se9b,se11))