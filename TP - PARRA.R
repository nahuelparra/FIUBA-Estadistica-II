rm (list=ls())

install.packages("dplyr")
install.packages("readxl")
install.packages("ggplot2")

library(dplyr)
library(readxl)
library(ggplot2)

#datos <- read_excel("D:/OneDrive/OneDrive - fi.uba.ar/Facultad/01 - Estadistica II/TP/datos_99458_PARRA_PABLO_NAHUEL.xlsx")
#View(datos)

datos <- read_excel("datos_99458_PARRA_PABLO_NAHUEL.xlsx")
View(datos)

#codifico todas mis variables dummy en base a lo que me interesa

#si 1, no 0
datos$satisfecho<-ifelse(datos$satisfecho=="si",1,0)

#hombre 0, mujer 1
datos$sexo <- ifelse(datos$sexo=="Hombre",0,1)

#baja lealtad 0, alta lealtad 1
datos$lealtad <- ifelse(datos$lealtad=="Baja Lealtad",0,1)

#personal 0, negocios 1
datos$tipo_viaje <- ifelse(datos$tipo_viaje=="Personal",0,1)

#codificacion ordinal
# ESTE ESTA BIEN, MI BASE ES LA QUE NO PUSE, O SEA, ECO, y estoy agregando 2 nuevas columnas 
datos$clase_EcoPlus <- ifelse(datos$clase=="Eco",0,1)
datos$clase_Business <- ifelse(datos$clase=="Business",1,0)

#elimino columna clase, ya que ahora la reemplace con otras dos
datos$clase <- NULL 

View(datos)
#datos

###PUNTO 1 
#el punto me pone todas las variables, no resto clase, porque ya la elimine arriba con NULL
logistic <- glm(satisfecho~., data=datos, family="binomial")
summary(logistic)


###PUNTO 2
#regresion sin variables explicativas
RLM.Vacio <- glm(formula=satisfecho~1,data=datos, family="binomial")
summary(RLM.Vacio)

#regresion con todas las variables explicativas
RLM.Completo <- glm(formula=satisfecho~.,data=datos,family="binomial")
summary(RLM.Completo)

#regresion backward
RLM.Backward <- step(RLM.Completo,
                     scope=list(lower=RLM.Vacio, upper=RLM.Completo),
                     direction="backward")
summary(RLM.Backward)

#regresion forward
RLM.Forward <- step(RLM.Vacio,
                    scope=list(lower=RLM.Vacio, upper=RLM.Completo),
                    direction="forward")
summary(RLM.Forward)


###PUNTO 3

datos_3 <- datos

datos_3

#elimino en este nuevo dataframe las columnas que no necesito
datos_3$sexo <- NULL 
datos_3$lealtad <- NULL 
datos_3$edad <- NULL 
datos_3$tipo_viaje <- NULL 
datos_3$min_demora_partida <- NULL 

datos_3

logistic_3 <- glm(satisfecho~., data=datos_3, family="binomial")
summary(logistic_3)

# Representación gráfica del modelo.
ggplot(datos_3) +
  geom_hline(yintercept = 0:1, color = "grey")+
  geom_point(aes(x = minutos_demora_arribo, y = satisfecho)) +
  theme_bw()

ggplot(datos_3) +
  geom_hline(yintercept = 0:1, color = "grey")+
  geom_jitter(aes(x = minutos_demora_arribo, y = satisfecho),
              width = 0.1, height  = .04,
              alpha = .5) +
  geom_hline(yintercept = 1, color = "grey")+
  theme_bw()


m1 <- glm(formula = satisfecho ~ minutos_demora_arribo,
          data = datos_3,
          family = "binomial")
summary(m1)


predict(m1)
datos_3$predict_z <- predict(m1)
datos_3$predict_p <- 1/(1+exp(-predict(m1)))

View(datos_3)


#ESTE ESTA RE CHETO OK

ggplot(datos_3) +
  geom_hline(yintercept = 0:1, color = "grey")+
  geom_jitter(aes(x = minutos_demora_arribo, y = satisfecho),
              width = 0.1, height  = .04,
              alpha = .5) +
  geom_line(aes(x = minutos_demora_arribo, y = predict_p), col = 'blue')+
  geom_hline(yintercept = 1, color = "grey")+
  theme_bw()
################3


ggplot(challenger) +
  geom_hline(yintercept = 0:1, color = "grey")+
  geom_jitter(aes(x = grados_C, y = Falla),
              width = 0.1, height  = .04,
              alpha = .5) +
  geom_line(aes(x = grados_C, y = predict_p), col = 'blue')+
  geom_line(aes(x = grados_C, y = predict_p_LI), col = 'blue', alpha = .3)+
  geom_line(aes(x = grados_C, y = predict_p_LS), col = 'blue', alpha = .3)+
  geom_hline(yintercept = 1, color = "grey")+
  theme_bw()


#calculo distancia promedio
x1 <- mean(datos_3$distancia)
x1

max(datos_3$distancia)
min(datos_3$distancia)

#pongo los coeficientes en variables, para que sea mas facil trabajar

b0 <- -2.160e-01
b1 <- -2.183e-04
b2 <- -4.649e-03
b3 <- 1.141e-01
b4 <- 1.430e+00

#hago un nuevo data frame para poner las probabilidades y graficarlas, asi con las 3 clases

df_Eco <- data.frame (minutos_demora_arribo = 0:800,
                  p_Eco = 1/(1+exp(-(b0 + b1*x1 + b2* 0:800))))

View(df_Eco)

ggplot(datos_3) +
  geom_hline(yintercept = 0:1, color = "grey")+
  geom_jitter(aes(x = minutos_demora_arribo, y = satisfecho),
              width = 0.1, height  = .04,
              alpha = .5) +
  geom_line(aes(x = minutos_demora_arribo, y = p_Eco),
            col = 'blue', data = df_Eco, inherit.aes = F)+
  theme_bw()

df_EcoPlus <- data.frame (minutos_demora_arribo = 0:800,
                     p_EcoPlus = 1/(1+exp(-(b0 + b1*x1 + b2* 0:800 + b3 + b4))))

View(df_EcoPlus)

ggplot(datos_3) +
  geom_hline(yintercept = 0:1, color = "grey")+
  geom_jitter(aes(x = minutos_demora_arribo, y = satisfecho),
              width = 0.1, height  = .04,
              alpha = .5) +
  geom_line(aes(x = minutos_demora_arribo, y = p_EcoPlus),
            col = 'red', data = df_EcoPlus, inherit.aes = F)+
  theme_bw()


df_Business <- data.frame (minutos_demora_arribo = 0:800,
                              p_Business = 1/(1+exp(-(b0 + b1*x1 + b2* 0:800 + b4))))

View(df_Business)

ggplot(datos_3) +
  geom_hline(yintercept = 0:1, color = "grey")+
  geom_jitter(aes(x = minutos_demora_arribo, y = satisfecho),
              width = 0.1, height  = .04,
              alpha = .5) +
  geom_line(aes(x = minutos_demora_arribo, y = p_Business),
            col = 'green', data = df_Business, inherit.aes = F)+
  theme_bw()

#graficos superpuestos

ggplot(datos_3) +
  geom_hline(yintercept = 0:1, color = "grey")+
  geom_jitter(aes(x = minutos_demora_arribo, y = satisfecho),
              width = 0.1, height  = .04,
              alpha = .5) +
  geom_line(aes(x = minutos_demora_arribo, y = p_Eco),
            col = 'blue', data = df_Eco, inherit.aes = F)+
  geom_line(aes(x = minutos_demora_arribo, y = p_EcoPlus),
            col = 'red', data = df_EcoPlus, inherit.aes = F)+
  geom_line(aes(x = minutos_demora_arribo, y = p_Business),
            col = 'green', data = df_Business, inherit.aes = F)+
  theme_bw()

###PUNTO 4
install.packages("caret")
install.packages("e1071")
  library(caret)
  library(e1071)
  
  # Training and Testing sets
  
  set.seed(123)
  sample <- sample(c(TRUE, FALSE), nrow(datos), replace=TRUE, prob=c(0.75,0.25))
  train <- datos[sample, ]
  test  <- datos[!sample, ]
  
  train
  test
  
  #punto a
  logistic_a <- glm(satisfecho~distancia+minutos_demora_arribo+clase_EcoPlus+clase_Business, data=train, family="binomial")
  summary(logistic_a)
  
  #hago un nuevo data frame para poner las probabilidades
  pronostico <-predict(logistic_a, type="response")
  pronostico
  
  #grafico el histograma
  hist(pronostico)
  
  #punto c, matriz de confusion
  
  #use model to predict probability of default
  predicted <- predict(logistic_a, test, type="response")
  predicted
  
  #convert defaults from "Yes" and "No" to 1's and 0's
  #test$default <- ifelse(test$satisfecho==1, 1, 0)
  
  tab <- table(test$satisfecho)
  tab
  
  #str(test)
  #test_satisfecho <- as.factor(test$satisfecho)
  #str(test_satisfecho)
  
   
  confusionMatrix(table(ifelse(predicted >0.6, 1,0), test$satisfecho))
  
  

