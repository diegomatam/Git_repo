library(readxl) # llamamos a la paqueteria que usamos para leer archivos excel
tvtot <- read_excel("tvtot.xlsx") # importamos a R la base de datos
View(tvtot) # para observar la base de datos importad 
class(tvtot) # nos indica que clase es la base de datos

# Pregunta 1.- Para cada una de las variables respuesta en la encuesta, indica el 
# tipo de variable y su escala de medicion.

# colonia : es variable cuantitativa con escala de medicion ordinal
# manzana : es variable cuantitativa con escala de medicion ordinal
# adultos : es variable cuantitativa con escala de medicion de razon 
# ninos   : es variable cuantitativa con escala de medicion de razon
# teles   : es variable cuantitativa con escala de medicion de razon
# renta   : es variable cuantitativa con escala de medicion de razon
# tvtot (horas)   : es variable cuantitativa con escala de medicion de razon
# tipo    : es variable cualitativa con escala de medicion nominal
# valor   : es variable cuantitativa con escala de medicion de razon


# Pregunta 2.- Describa las variables adultos, ninos, teles y tipo.
# considere el uso de tablas y/o graficas. 

# Primero para la variabe adultos:

adultos <- tvtot$adultos # extraemos y nombramos la variable de la base de datos
class(adultos) # podemos ver que la variable "adultos" es numerica 

summary(adultos) # el comando summary nos da un resumen de la variable 
                 # como se observa en la consola, el valor minimo de adultos es de 1
                 # la mediana es de 2 adultos, la media es de 2.375 audltos y el maximo es de 4

mean(adultos)    # tambien podemos calcular la media con el comando mean 

hist(adultos)    # podemos generar un histograma para la variable adultos usando el comando hist.
                 # como se observa, el comando por default grafica el numero de adultos en el eje x 
                 # y el la frecuencia relativa en el eje y. 

?hist            # cuando tengamos duda sobre algun comando podemos usar la ayuda de R utilizando un 
                 # signo de interrogacion ? antes del comando.

hist(adultos,breaks = 0:4)     # si observamos el histograma anterior podemos ver que los "breaks" o cortes no eran logicos
                               # podemos personalizar estos cortes con el comando break.

hist(adultos, breaks = 0:4, 
     main = "Histograma adultos", 
     xlab = "Adultos por hogar", 
     ylab = "Frecuencia Relativa", 
     col = "red")                      # podemos personalizar las grafica


aggregate(tvtot$adultos,by=list(Category=tvtot$colonia),FUN=sum)    # podemos ver cuantos adultos hay en cada colonia
                                                                    # en la colonia 1 hay 41 adultos y en la colonia 2 hay 54 adultos

aggregate(tvtot$adultos,by=list(Category=tvtot$tipo),FUN=sum)       # igual que arriba, podemos ver cuantos alumnos hay dependiendo de
                                                                    # el tipo de television que hay en el hogar


# Para la variable ninos:

ninos <- tvtot$ninos  # extraemos y nombramos la variable de la base de datos

summary(ninos) # el comando summary nos da un resumen de la variable 
               # como se observa en la consola, el valor minimo de adultos es de 0
               # la mediana es de 1.5 adultos, la media es de 1.475 audltos y el maximo es de 3

# para analizar la variable adultos usamos graficas predetermiandas en R, sin embargo, existe un paquete para elementos visuales 
# mucho mejor llamada ggplot, este paquete funciona en "capas". Vamos a usar este paquete para el histograma de la variable ninos.

install.packages("ggplot2")
library(ggplot2)

?ggplot       # recordar que si tenemos duda de algun comando, podemos pedir la ayuda de R


ggplot(tvtot, aes(x=ninos)) +                       # como vemos, el comando ggplot funcion por "capas", funciona para hacer todo tipo de
  geom_histogram( binwidth=1, fill="blue",          # grafico y tienes más funciones para personalizar los graficos
                  color="black", alpha=0.5) +
  ggtitle("Histograma niños") +
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab("Niños por hogar") + ylab("Frecuencia")

aggregate(tvtot$ninos,by=list(Category=tvtot$colonia),FUN=sum)

aggregate(tvtot$ninos,by=list(Category=tvtot$tipo),FUN=sum)


# Para la variable teles:

teles <- tvtot$teles   # extraemos y nombramos la variable de la base de datos

summary(teles)         # el comando summary nos da un resumen de la variable 
                       # como se observa en la consola, el valor minimo de teles es de 0
                       # la mediana es de 3 teles, la media es de 2.45 teles y el maximo es de 5


ggplot(tvtot, aes(x=teles)) +                       # como vemos, el comando ggplot funcion por "capas", funciona para hacer todo tipo de
  geom_histogram( binwidth=1, fill="green",         # grafico y tienes más funciones para personalizar los graficos
                  color="black", alpha=0.2) +
  ggtitle("Histograma teles") +
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab("Teles por hogar") + ylab("Frecuencia")

aggregate(tvtot$teles,by=list(Category=tvtot$colonia),FUN=sum)

aggregate(tvtot$teles,by=list(Category=tvtot$tipo),FUN=sum)


# Para la variable tipo:

tipo <- tvtot$tipo   # extraemos y nombramos la variable de la base de datos, pero OJO, esta variable no es numerica, es cualitativa
class(tipo)          # con el comando class podemos ver de a que clase pertenece la varial, en este caso pertenece a character
summary(tipo)        # si usamos el comando summary solo cuenta los elementos dentro de este vector, sin embargo, existe otra clase de 
                     # variable llamada "factor", en esta clase podemos incluir variables cualitativas que se puedan jerarquizar de cierta manera,
                     # por ejemplo orden alfabético (que R toma por default) o cualquier jerarquizacion que se quiera dar. 

tipo <- as.factor(tipo)   # con el comando as.factor estamos transformando la variable tipo que era de clase character a una de tipo factor
tipo                      # cuando llamamos a la variable, podemos ver en la consola los niveles, en este caso jerarquiza por orden alfab.


summary(tipo)        # una vez que la variable es factor, podemos usar el comando summary


ggplot(tvtot, aes(x=tipo)) +                       # como vemos, el comando ggplot funcion por "capas",OJO, cuando queremos hacer 
  geom_bar(fill="red",color="black",alpha=0.2)+    # un hisograma para variables cualitativas, no podemos usar geom_histogram,
  ggtitle("Histograma Tipo") +                     # tenemos que usar geom_bar
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab("Tipo de tele por hogar") + ylab("Frecuencia")

install.packages("vtree")  # vamos a usar otro tipo de visualizacion, para esto es necesario el paquete vtree
library(vtree)

vtree(tvtot,"tipo")       # este tipo de representaciones son útiles para variables cualitativas, además, podemos extender este tipo
                          # de representaciones.

vtree(tvtot,c("colonia","tipo"),                                 
      fillcolor = c(colonia= "deepskyblue1",tipo="seagreen"),
      horiz = FALSE,title = "Hogares")


# Pregunta 3.- Para cada una de las variables cuantitativas tvtot,renta y valor:
#              a) construya un histograma
#              b) construya una ojiva             
#              c) determine su media, mediana y moda(s)
#              d) encuentre el primer y tercer cuartil, y su rango intercuartilico



# para la variable tvtot: 

tvtotal <- tvtot$tvtot  # primero extraemos de la base de datos la variable y la llamamos tvtotal 
tvtotal
sort(tvtotal)

# a) histograma:

ggplot(tvtot, aes(x=tvtot))+
  geom_histogram(binwidth = 3, fill="red",
                 color="black",alpha=0.3)+
  ggtitle("Histograma horas de tv")+
  theme(plot.title = element_text(hjust = 0.5))+
  xlab("Horas de tv") + ylab("Frecuencia")


#b) ojiva  (necesitamos llamar al paquete fdth)

library(fdth)

df=fdt(tvtotal,breaks = "Sturges");df

x_oj_tvt=c(0,12.409,24.817,37.226,49.63,62.043,74.451,86.86)         # en el eje de las x´s tenemos se usan los limites inferiors de cada clase cerrando con el
y_oj_tvt=c(0,5,22.5,47.5,60,75,85,100)                               # ultimo limite superior. Para el eje y, usamos las frecuencias relativa acumuladas. 
plot(x_oj_tvt,y_oj_tvt,type = "p",pch=20,lty=1,xlab="Horas de tv",   # para graicar usamos el comando plot, type="p" es porque queremos puntos
     ylab="Frecuencia relativa acumulada %",xaxt="n",yaxt="n",
     main="Ojiva para horas totales de tv")
axis(side=1,c(0,12.4,24.8,37.2,49.6,62,74.5,86.6),labels=T)          # con el comando axis (side=1 es eje x) damos que valors deben de aparecer
axis(side=2,c(0,10,20,30,40,50,60,70,80,90,100),labels=T,las=2)      # igual que antes  
lines(x_oj_tvt,y_oj_tvt)                                             # con el comando lines aplicamos una linea que una los puntos que graficamos antes


#c) media, mediana y moda


mean(tvtotal)     # con el comando mean podemos ver la media, en este caso es de 43.975 horas de tv semanales

median(tvtotal)   # con el comando median podemos ver la mediana, en este caso es de 39 horas de tv semanales

# no existe una funcion predeterminada para la moda, pero en el paquete modeest existe una funcion

install.packages("modeest")
library(modeest) 

mfv1(tvtotal)     # con el comando mfv1 podemos ver la moda, en este caso es de 20 horas de tv semanales


#d) encuentre el primer y tercer cuartil, y su rango intercuartilico

quantile(tvtotal)   # con el comando quantile podemos ver los cuartiles, que imprime R por default, sin embargo, podmos ver cualquier percentil

quantile(tvtotal,probs = c(0.10,0.20,0.30))  # de esta manera podemos ver cualquier cuantil que queramos


# para la variable renta: 

renta <- tvtot$renta  # primero extraemos de la base de datos la variable y la llamamos renta 
renta
sort(renta)

# a) histograma:

ggplot(tvtot, aes(x=renta))+
  geom_histogram(binwidth = 3, fill="red",
                 color="black",alpha=0.3)+
  ggtitle("Histograma renta")+
  theme(plot.title = element_text(hjust = 0.5))+
  xlab("Renta") + ylab("Frecuencia")


#b) ojiva  (necesitamos llamar al paquete fdth)

library(fdth)

df_renta=fdt(renta,breaks = "Sturges");df_renta

x_oj_renta=c(0,12.264,24.529,36.793,49.057,62.321,73.586,85.85)         # en el eje de las x´s tenemos se usan los limites inferiors de cada clase cerrando con el
y_oj_renta=c(0,5,22.5,47.5,60,75,85,100)                               # ultimo limite superior. Para el eje y, usamos las frecuencias relativa acumuladas. 
plot(x_oj_renta,y_oj_renta,type = "p",pch=20,lty=1,xlab="Renta",   # para graicar usamos el comando plot, type="p" es porque queremos puntos
     ylab="Frecuencia relativa acumulada %",xaxt="n",yaxt="n",
     main="Ojiva para renta")
axis(side=1,c(0,12.2,24.5,36.7,49,62.2,73.5,85.8),labels=T)          # con el comando axis (side=1 es eje x) damos que valors deben de aparecer
axis(side=2,c(0,10,20,30,40,50,60,70,80,90,100),labels=T,las=2)      # igual que antes  
lines(x_oj_renta,y_oj_renta)                                             # con el comando lines aplicamos una linea que una los puntos que graficamos antes


#c) media, mediana y moda

mean(renta)     # con el comando mean podemos ver la media, en este caso es de 58.25 

median(renta)   # con el comando median podemos ver la mediana, en este caso es de 62.5

mfv1(renta)     # como ya cargamos el paquete,no es necesario volverlo a hacer, con el comando mfv1 podemos ver la moda, en este caso es de 65


#d) encuentre el primer y tercer cuartil, y su rango intercuartilico

quantile(renta)   # con el comando quantile podemos ver los cuartiles, que imprime R por default, sin embargo, podmos ver cualquier percentil



# para la variable valor: 

valor <- tvtot$valor  # primero extraemos de la base de datos la variable y la llamamos valor
valor
sort(valor)

# a) histograma:

hist(valor, xlab = "Valor", ylab = "Frecuencia", main = "Histograma para Valor", col = "yellow")


#b) ojiva  (necesitamos llamar al paquete fdth)

library(fdth)

df_valor=fdt(valor,breaks = "Sturges");df_valor

x_oj_valor=c(79128.72,121257.224,163385.729,205514.233,247642.737,289771.241,331899.746,374028.25)    
y_oj_valor=c(0,7.5,27.5,42.5,60,75,87.5,100)                              
plot(x_oj_valor,y_oj_valor,type = "p",pch=20,lty=1,xlab="Valor",  
     ylab="Frecuencia relativa acumulada %",xaxt="n",yaxt="n",
     main="Ojiva para valor")
axis(side=1,c(79128,121257,163385,205514,247642,289771,331899,374028),labels=T)          
axis(side=2,c(0,10,20,30,40,50,60,70,80,90,100),labels=T,las=2)       
lines(x_oj_valor,y_oj_valor)                                             

#c) media, mediana y moda

mean(valor)     # con el comando mean podemos ver la media, en este caso es de 227973.8 

median(valor)   # con el comando median podemos ver la mediana, en este caso es de 216393

mfv1(valor)     # como ya cargamos el paquete,no es necesario volverlo a hacer, con el comando mfv1 podemos ver la moda, en este caso es de 79928


#d) encuentre el primer y tercer cuartil, y su rango intercuartilico

quantile(valor)   # con el comando quantile podemos ver los cuartiles, que imprime R por default, sin embargo, podmos ver cualquier percentil



# Pregunta 4.- Con base en su ojiva, ¿que porcentaje de hogares ve 50 horas semanales o mas de telvision? 
# ¿cuantas horas de television ve el 40% de los hogares?


# aproximadamente el 40% de los hogares ve 50 horas semanales o mas de television.
# el 40% de los hogares ven aproximandamente 31 hora de television a la semana



# Pregunta 5.- ¿Cuanto es lo mas que pagaria de renta el 80% de la poblacion muestra?

























