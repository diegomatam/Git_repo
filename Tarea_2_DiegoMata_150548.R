### PAQUETES ###
#install.packages("psych")
library(psych)

#install.packages("stargazer")
library(stargazer)

install.packages("dplyr")
library(dplyr)

#install.packages("ggplot2")
library(ggplot2)

#install.packages("quantreg")
library(quantreg)

#install.packages("sandwich")
library(sandwich)

#install.packages("lmtest")
library(lmtest)

#install.packages("modelsummary")
library(modelsummary)

#install.packages("estimatr")
library(estimatr)

#install.packages("car")
library(car)

#install.packages("plyr")
library(plyr)

#install.packages("tidyverse")
library(tidyverse)

install.packages("stats")
library(stats)

predict
### DATOS ###

library(readr)
data <- read_csv("~/Econometría/T2_covid.csv")
View(data)

options(scipen=999)

### Definir variables ###

confirmed<- data$confirmed

confirmed_per_mil<- data$confirmed_per_mil

deaths<- data$deaths

deaths_per_mil<- data$deaths_per_mil

tests_performed<- data$tests_performed

gdp_pc<- data$gdp_pc

median_age<- data$median_age

age_65_older<- data$aged_65_older

diab_prev<- data$diab_prev

cardio_dr<- data$cardio_dr

hosp_beds_per_thou<- data$hosp_beds_per_thou

hdi<- data$hdi

overwgh_prev<- data$overwgh_prev

########## PREGUNTA 1 ##########
### Summary para cada variable ###

describe(confirmed)

describe(confirmed_per_mil)

describe(deaths)

describe(deaths_per_mil)

describe(tests_performed)

describe(gdp_pc)

describe(median_age)

describe(age_65_older)

describe(diab_prev)

describe(cardio_dr)

describe(hosp_beds_per_thou)

describe(hdi)

describe(overwgh_prev)

########## PREGUNTA 2 ##########

#(a)#

data<-mutate(data,tests_per_mil=(tests_performed*confirmed_per_mil/confirmed))
View(data)
tests_per_mil<- data$tests_per_mil

ggplot(data,aes(tests_per_mil,confirmed_per_mil)) + 
  geom_point() + geom_smooth(method = "lm", color="red") +
  ggtitle("Diagrama de dispersión") +
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab("Test per mil") + ylab("Confirmed per mil")


modelo1<- lm(confirmed_per_mil~tests_per_mil)
summary(modelo1)

#(b)#

ggplot(data,aes(log(tests_per_mil),confirmed_per_mil)) + 
  geom_point() + geom_smooth(method = "lm", color="red") +
  ggtitle("Diagrama de dispersión")+ 
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab("ln(test per mil)") + ylab("Confirmed per mil")


modelo2<- lm(confirmed_per_mil~log(tests_per_mil))
summary(modelo2)


#(c)#

ggplot(data,aes(log(tests_per_mil),log(confirmed_per_mil))) + 
  geom_point() + 
  geom_smooth(method = "lm", color="blue") +
  ggtitle("Diagrama de dispersión") + 
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab("ln(test per mil)") + ylab("ln(confirmed per mil)")

modelo3<- lm(log(confirmed_per_mil)~log(tests_per_mil))
summary(modelo3)

?rq

#(d)#

# regresión cuantil 0.1:

modelo_q<- rq(log(confirmed_per_mil)~log(tests_per_mil), tau = c(0.25,0.5,0.75))
summary(modelo_q)
plot(summary(modelo_q), parm = "log(tests_per_mil)", main = "ln(tests per mil)" ) 


ggplot(data,aes(log(tests_per_mil),log(confirmed_per_mil))) + 
  geom_point() + 
  geom_quantile(aes(colour="0.25"), quantiles=0.25) +
  geom_quantile(aes(colour="0.5"), quantiles=0.5) +
  geom_quantile(aes(colour="0.75"), quantiles=0.75) +
  scale_color_discrete("Cuantil") +
  geom_smooth(method = "lm", color="yellow", se=F)

?parm  

summary(modelo_q, se="boot")


########## PREGUNTA 3 ##########
#(a)#

ggplot(data,aes(log(gdp_pc),log(confirmed_per_mil))) + 
  geom_point() + 
  geom_smooth(method = "lm", color="blue") +
  ggtitle("Diagrama de dispersión") + 
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab("ln(gdp pc)") + ylab("ln(confirmed per mil)")


modelo4<- lm(log(confirmed_per_mil)~log(gdp_pc))
summary(modelo4)


#(b)# ERRORES DE MEDICIÓN 

#(I):


#(II):

gdp_pc_er<- data$gdp_pc_er
modelo5<- lm(log(confirmed_per_mil)~log(gdp_pc_er))
summary(modelo5)


#(c)#

#?

#(d)#

#(I):

modelo6<- lm(log(gdp_pc)~log(tests_per_mil))
summary(modelo6)

#(II):


residuales<- residuals(lm(log(gdp_pc)~log(tests_per_mil), na.action = na.exclude))

length(residuales)

modelo7<- lm(log(confirmed_per_mil)~residuales)
summary(modelo7)


ggplot(data,aes(residuales,log(confirmed_per_mil))) + 
  geom_point() + 
  geom_smooth(method = "lm", color="blue") +
  ggtitle("Diagrama de dispersión") + 
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab("Residuales") + ylab("ln(confirmed per mil)")



#(e)#

modelo8<- lm(log(confirmed_per_mil)~log(gdp_pc)+log(tests_per_mil))
summary(modelo8)



########## PREGUNTA 4 ##########

# Generar variables:

data<- mutate(data,cfr=deaths/confirmed)
View(data)

eur<- ifelse(data$continent=='EU',1,0)
eur
data<- mutate(data,eur)
View(data)


asia<- ifelse(data$continent=='AS',1,0)
asia
data<- mutate(data,asia)
View(data)

nam<- ifelse(data$continent=='NAM',1,0)
data<- mutate(data,nam)
View(data)


 
std_hdi<- scale(data$hdi)
data<- mutate(data,std_hdi)
View(data)


hm_cfr<- ifelse(data$cfr>0.019, 1,0)
hm_cfr
data<- mutate(data,hm_cfr)
View(data)

# Estimaciones:


modelo9<- lm (log(data$deaths)~data$overwgh_prev+log(data$cardio_dr)+data$diab_prev+log(data$tests_per_mil)+log(data$gdp_pc))
coeftest(modelo9, vcov=vcovHC(modelo9,type = "HC1"))
stargazer(coeftest(modelo9, vcov=vcovHC(modelo9,type = "HC1")))
modelsummary(modelo9, vcov=vcovHC(modelo9,type = "HC1"),stars = T)



modelo10<- lm (data$deaths_per_mil~data$overwgh_prev+log(data$cardio_dr)+data$diab_prev+log(data$tests_per_mil)+data$eur+data$asia+data$median_age)
coeftest(modelo10,vcov=vcovHC(modelo10,type = "HC1"))
stargazer(coeftest(modelo10,vcov=vcovHC(modelo10,type = "HC1")))
modelsummary(modelo10, vcov=vcovHC(modelo10,type = "HC1"),stars = T)


stdi_sq<-std_hdi^2
data<- mutate(data,stdi_sq)
view(data)
modelo11<- lm (data$cfr~data$overwgh_prev+log(data$cardio_dr)+data$diab_prev+log(data$tests_per_mil)+data$std_hdi+data$stdi_sq+data$median_age)
coeftest(modelo11,vcov=vcovHC(modelo11,type = "HC1"))
stargazer(coeftest(modelo11,vcov=vcovHC(modelo11,type = "HC1")))
modelsummary(modelo11, vcov=vcovHC(modelo11,type = "HC1"),stars = T)


modelo12<- lm (data$hm_cfr~data$overwgh_prev+log(data$cardio_dr)+data$diab_prev+log(data$hosp_beds_per_thou)+log(data$tests_per_mil)+data$aged_65_older+log(data$gdp_pc)+data$nam)
coeftest(modelo12,vcov=vcovHC(modelo12,type = "HC1"))
stargazer(coeftest(modelo12,vcov=vcovHC(modelo12,type = "HC1")))
modelsummary(modelo12, vcov=vcovHC(modelo12,type = "HC1"),stars = T)



########## PREGUNTA 5 ##########

## ? ##



########## PREGUNTA 6 ##########  
#(a):

predict(modelo10)[51]

data$deaths_per_mil[data$country=="Mexico"]


#(b):

a<-coef(modelo11)
a<-a[c(6,7)]
a

confint(modelo11,parm = "std_hdi",vcov=vcovHC(modelo11,type = "HC1"),level = 0.95)
confint(modelo11,"stdi_sq",vcov=vcovHC(modelo11,type = "HC1"),level = 0.95)



#(c):

c<-coef(modelo12,vcov=vcovHC(modelo12,type = "HC1"))
c
c<-c[c(2,4,6)]
c
#Suponemos que el gobierno es México, los cambios serían los siguientes:
#Para el número de pruebas por cada millón de habitantes:

#Tests_per_mil:
c[3]/100
cambio_tests<- (c[3]/100)*15
cambio_tests

#Overwgh_prev:
c[1]
cambio_overwgh<- c[1]*1.015
cambio_overwgh

#Diab_prev:
c[2]
cambio_diab<- c[2]*1.02
cambio_diab

# Predicción original:

mex_1<-predict(modelo12)[51]
mex_1
# Predicción con los cambios:

prediccion_mex_cambios<- mex_1+cambio_diab
prediccion_mex_cambios
prediccion_mex_cambios_1<- prediccion_mex_cambios+cambio_overwgh
prediccion_mex_cambios_1
prediccion_mex_cambios_2<- prediccion_mex_cambios_1+cambio_tests
prediccion_mex_cambios_2

prediccion_mex_cambios_2-mex_1

vec_hip= c(0,1.5,0,2,0,15,0,0,0)
result <- c(0)
hip <- linearHypothesis(modelo12, vec_hip, result, white.adjust="hc1")
hip
stargazer(hip)

########## PREGUNTA 7 ##########
#(a): Diferencias entre Europa y Asia
 

## Primero crear el país promedio eruopeo y asiático para las variables explicativas


eu_prom <- data %>% filter(continent == "EU")%>% 
  summarise(overwgh_prev = mean(overwgh_prev, na.rm = TRUE), 
            cardio_dr = mean(cardio_dr, na.rm = TRUE), diab_prev = mean(diab_prev, na.rm = TRUE), 
            tests_per_mil = mean(tests_per_mil, na.rm = TRUE), 
            median_age = mean(median_age, na.rm = TRUE), eur = mean(eur),
            asia = mean(asia))

view(eu_prom)

as_prom <- data %>% filter(continent == "AS") %>% 
  summarise(overwgh_prev = mean(overwgh_prev, na.rm = TRUE), 
            cardio_dr = mean(cardio_dr, na.rm = TRUE),
            diab_prev = mean(diab_prev, na.rm = TRUE), 
            tests_per_mil = mean(tests_per_mil, na.rm = TRUE), 
            median_age = mean(median_age, na.rm = TRUE), eur = mean(eur),
            asia = mean(asia))

view(as_prom)

death_eu <- 926.363+5.096*eu_prom$overwgh_prev-(149.412*log(eu_prom$cardio_dr))+3.015*eu_prom$diab_prev-
  6.685*log(eu_prom$tests_per_mil)+8.536*eu_prom$eur-61.645*eu_prom$asia-5.660*eu_prom$median_age


death_as <- 926.363+5.096*as_prom$overwgh_prev-(149.412*log(as_prom$cardio_dr))+3.015*as_prom$diab_prev-
  6.685*log(as_prom$tests_per_mil)+8.536*as_prom$eur-61.645*as_prom$asia-5.660*as_prom$median_age



death_eu
death_as

diff<- death_eu - death_as
diff

eu_prom$overwgh_prev - as_prom$overwgh_prev
log(eu_prom$cardio_dr) - log(as_prom$cardio_dr)
eu_prom$diab_prev - as_prom$diab_prev
log(eu_prom$tests_per_mil) - log(as_prom$tests_per_mil)
eu_prom$eur - as_prom$eur
eu_prom$asia - as_prom$asia
eu_prom$median_age - as_prom$median_age


vec = c(0, 20.50393, 0.02341373, -3.986395, 0.4449775, 1, -1, 10.29489)
linearHypothesis(modelo10, vec, white.adjust="hc1") 
stargazer(linearHypothesis(modelo10, vec, white.adjust="hc1")) #copiar esto en latex


#II:

## Para Europa: 

eu_as <- data %>% filter(continent %in% c("AS", "EU"))
view(eu_as)
modelo10_eu_as <- lm(eu_as$deaths_per_mil ~ eu_as$overwgh_prev + log(eu_as$cardio_dr) + eu_as$diab_prev + 
                   log(eu_as$tests_per_mil) + 
                   eu_as$eur + eu_as$median_age)
se_eu_as <- sqrt(diag(vcovHC(modelo10_eu_as, type = "HC1")))

# el coeficiente asociado a eur es el importante!!
stargazer(modelo10_eu_as, se = list(se_eu_as))



vec_1 = c(0, 20.50393, 0.02341373, -3.986395, 0.4449775, 1, 10.29489)
linearHypothesis(modelo10_eu_as,vec_1,white.adjust="hc1")
stargazer(linearHypothesis(modelo10_eu_as,vec_1,white.adjust="hc1"))
# la diferencia sí es significativa



## III: 



qf(0.995,39,29,lower.tail = F)

qf(0.005,39,29,lower.tail = F)

pf(2.25,39,29,lower.tail = F)*2


qnorm(0.025,0,1,lower.tail = T)





