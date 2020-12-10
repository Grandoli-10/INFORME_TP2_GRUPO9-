#INFORME TP2 GRUPO 9 ####
####LIBRERIAS ####

library(tseries)   
library(forecast)
library(astsa)
library(readr)
library(readxl)   
library(zoo)
library(xts)
library(ggplot2)
library(psych)
library(stats)
library(fitdistrplus)
library(urca)

####ALGORITMOS ####

Significance_Test <- function(model, f = c("arma","ar.yw","ar.ols","ar.mle","arima","sarima")){
  
  if(f == "arma"){
    
    desvio = NULL
    for(i in 1:sqrt(length(model$vcov))){
      desvio[i] = sqrt(model$vcov[i,i])
    }
    
    coef = NULL
    for(i in 1:length(model$coef)){
      coef[i] = model$coef[i]
    }
    
    tabla = cbind(coef,desvio)
    tabla = as.data.frame(tabla)
    tabla$estadistico = coef/desvio
    
    tabla$decision = NULL
    for(i in 1:nrow(tabla)){
      if(abs(tabla$estadistico[i]) > 2){
        tabla$decision[i] = "Rechazo"
      } else {
        tabla$decision[i] = "No Rechazo"
      }
    }
    
    tabla$significativo = NULL
    for(i in 1:nrow(tabla)){
      if(tabla$decision[i] == "Rechazo"){
        tabla$significativo[i] = "Significativo"
      } else {
        tabla$significativo[i] = "No Significativo"
      }
    }
  }else if(f == "ar.yw"){
    
    
    desvio = NULL
    for(i in 1:sqrt(length(model$asy.var.coef))){
      desvio[i] = sqrt(model$asy.var.coef[i,i])
    }
    
    coef = NULL
    for(i in 1:length(model$ar)){
      coef[i] = model$ar[i]
    }
    
    tabla = cbind(coef,desvio)
    tabla = as.data.frame(tabla)
    tabla$estadistico = coef/desvio
    
    tabla$decision = NULL
    for(i in 1:nrow(tabla)){
      if(abs(tabla$estadistico[i]) > 2){
        tabla$decision[i] = "Rechazo"
      } else {
        tabla$decision[i] = "No Rechazo"
      }
    }
    
    tabla$significativo = NULL
    for(i in 1:nrow(tabla)){
      if(tabla$decision[i] == "Rechazo"){
        tabla$significativo[i] = "Significativo"
      } else {
        tabla$significativo[i] = "No Significativo"
      }
    }
  }else if(f == "ar.ols"){
    
    
    desvio = NULL
    for(i in 1:(length(model$asy.se.coef$ar))){
      desvio[i] = (model$asy.se.coef$ar[i])
    }
    
    coef = NULL
    for(i in 1:length(model$ar)){
      coef[i] = model$ar[i]
    }
    
    tabla = cbind(coef,desvio)
    tabla = as.data.frame(tabla)
    tabla$estadistico = coef/desvio
    
    tabla$decision = NULL
    for(i in 1:nrow(tabla)){
      if(abs(tabla$estadistico[i]) > 2){
        tabla$decision[i] = "Rechazo"
      } else {
        tabla$decision[i] = "No Rechazo"
      }
    }
    
    tabla$significativo = NULL
    for(i in 1:nrow(tabla)){
      if(tabla$decision[i] == "Rechazo"){
        tabla$significativo[i] = "Significativo"
      } else {
        tabla$significativo[i] = "No Significativo"
      }
    }
  }else if(f == "ar.mle"){
    
    
    desvio = NULL
    for(i in 1:sqrt(length(model$asy.var.coef))){
      desvio[i] = sqrt(model$asy.var.coef[i,i])
    }
    
    coef = NULL
    for(i in 1:length(model$ar)){
      coef[i] = model$ar[i]
    }
    
    tabla = cbind(coef,desvio)
    tabla = as.data.frame(tabla)
    tabla$estadistico = coef/desvio
    
    tabla$decision = NULL
    for(i in 1:nrow(tabla)){
      if(abs(tabla$estadistico[i]) > 2){
        tabla$decision[i] = "Rechazo"
      } else {
        tabla$decision[i] = "No Rechazo"
      }
    }
    
    tabla$significativo = NULL
    for(i in 1:nrow(tabla)){
      if(tabla$decision[i] == "Rechazo"){
        tabla$significativo[i] = "Significativo"
      } else {
        tabla$significativo[i] = "No Significativo"
      }
    }
  } else if(f == "arima"){
    
    desvio = NULL
    for(i in 1:sqrt(length(model$var.coef))){
      desvio[i] = sqrt(model$var.coef[i,i])
    }
    coef = NULL
    for(i in 1:length(model$coef)){
      coef[i] = model$coef[i]
    }
    
    tabla = cbind(coef,desvio)
    tabla = as.data.frame(tabla)
    tabla$estadistico = coef/desvio
    
    tabla$decision = NULL
    for(i in 1:nrow(tabla)){
      if(abs(tabla$estadistico[i]) > 2){
        tabla$decision[i] = "Rechazo"
      } else {
        tabla$decision[i] = "No Rechazo"
      }
    }
    
    tabla$significativo = NULL
    for(i in 1:nrow(tabla)){
      if(tabla$decision[i] == "Rechazo"){
        tabla$significativo[i] = "Significativo"
      } else {
        tabla$significativo[i] = "No Significativo"
      }
    }
    
  } else {
    
    desvio = NULL
    for(i in 1:(length(model$ttable))/4){
      desvio[i] = (model$ttable[i,2])
    }
    coef = NULL
    for(i in 1:(length(model$ttable))/4){
      coef[i] = model$ttable[i,1]
    }
    
    tabla = cbind(coef,desvio)
    tabla = as.data.frame(tabla)
    tabla$estadistico = coef/desvio
    
    tabla$decision = NULL
    for(i in 1:nrow(tabla)){
      if(abs(tabla$estadistico[i]) > 2){
        tabla$decision[i] = "Rechazo"
      } else {
        tabla$decision[i] = "No Rechazo"
      }
    }
    
    tabla$significativo = NULL
    for(i in 1:nrow(tabla)){
      if(tabla$decision[i] == "Rechazo"){
        tabla$significativo[i] = "Significativo"
      } else {
        tabla$significativo[i] = "No Significativo"
      }
    }
  }
  names(tabla) = c("Coeficientes","Desvio","Estadístico","Decisión","Significativo")
  
  return(tabla)
}

Normality_Test <- function(ts,type = c("JB", "AD", "SW")){
  require(tseries)
  require(nortest)
  if(type == "JB"){
    p_val = jarque.bera.test(ts)$p.value
    stat  = jarque.bera.test(ts)$statistic
  } else if(type == "AD"){
    p_val = ad.test(ts)$p.value
    stat  = ad.test(ts)$statistic
  } else {
    p_val = shapiro.test(ts)$p.value
    stat  = shapiro.test(ts)$statistic
  }
  
  table = data.frame(P_Value = p_val,
                     Statistic = stat)
  return(table)
}


####PATH #####

getwd()
path = "D:/Desktop/FACULTAD/2020/2 do CUATRIMESTRE/COMPUTACION ACTUARIAL/TP 2"
setwd(path)
getwd()
dir()

####CARGAMOS EL DATASET ####

datos <- read.csv2("TASA LIBOR1.csv", header=T)
View(datos)
time=(datos$observation_date)
FECHA <- as.Date(time)
data.class(FECHA)
data.class(datos$USD1MTD156N)

LIBOR <- ts(datos$USD1MTD156N,frequency = 12,start = c(2009,1), end = c(2020,1))
data.class(LIBOR)
plot(LIBOR)
is.na(LIBOR)



for(i in 1:length(LIBOR)){         #UTILIZAMOS BUCLE FOR PARA COMPLETAR LOS NA CON PROMEDIO DE LOS ULTIMOS 7 DIAS 
    if(is.na(LIBOR[i]) == TRUE){
      LIBOR[i] <- mean(LIBOR[i-7]:LIBOR[i-1],rm.na=TRUE)
  }
}

View(LIBOR)
plot(LIBOR)


####SERIE DE TIEMPO  ORIGINAL ####

autoplot(LIBOR) + ggtitle("TASA LIBOR MENSUAL EN USD") + xlab("AÑO") + ylab("TASA")

#Grafico de la FAC
ggAcf(LIBOR,type = "correlation")   #DECRECIMIENTO LINEAL

#Grafico de la FACP
ggAcf(LIBOR,type = "partial")

#Podemos intuir la NO estacionariedad. 

descomposicion_serie <- decompose(LIBOR)
plot(descomposicion_serie)

adf.test(LIBOR)       #VEMOS QUE LA SERIE NO ES ESTACIONARIA, porque el p-value es alto, por ende no rechazamos h0 

####TRANSFORMACION SERIE DE TIEMPO -- DIFERENCIAS ####

LIBOR_DIF <- diff(LIBOR)
View(LIBOR_DIF)


autoplot(LIBOR_DIF) + ggtitle("DIFERENCIAS DE LA TASA LIBOR MENSUAL EN USD") + xlab("AÑO") + ylab("VARIACION")
  
#Grafico de la FAC
ggAcf(LIBOR_DIF,type = "correlation")

#Grafico de la FACP
ggAcf(LIBOR_DIF,type = "partial")


descomposicion_serie_dif <- decompose(LIBOR_DIF)
plot(descomposicion_serie_dif)

adf.test(LIBOR_DIF)    #RECHAZAMOS H0

####ESTADISTICA DESCRIPTIVA ####

hist(LIBOR_DIF, breaks = 15, col = rainbow(10))

boxplot(LIBOR_DIF, col = "green", main = "Boxplot Serie LIBOR DIF")

TABLA.1 <- describe(LIBOR_DIF)
write.csv2(TABLA.1, file= "TABLA1.csv")

####ESTIMACION ####

ar(LIBOR_DIF)    #AL NO PODER LEER LOS GRAFICOS DE FORMA CORRECTA, UTILIZAMOS LA FUNCION AR PARA PREDECIR EL ORDEN DEL MODELO

SARIMA_21_0 <- sarima(LIBOR_DIF,21,0,0)

SIGNIF_SARIMA_21_0 <- Significance_Test(SARIMA_21_0,"sarima")

COEF.SARIMA_21_0 <- SIGNIF_SARIMA_21_0$Coeficientes[1:22]

MATRIZ_COEFICIENTES <- matrix(COEF.SARIMA_21_0)

colnames(MATRIZ_COEFICIENTES) <- c("ARIMA (21,1,0)")
rownames(MATRIZ_COEFICIENTES) <- c("AR 1", "AR 2", "AR 3","AR 4", "AR 5", "AR 6","AR 7", "AR 8", "AR 9","AR 10","AR 11","AR 12","AR 13","AR 14","AR 15","AR 16", "AR 17", "AR 18","AR 19","AR 20","AR 21", "INTERCEPTO" )

MATRIZ_COEFICIENTES

DECISION <- Significance_Test(SARIMA_21_0,"sarima")

MATRIZ_COEFICIENTES <- cbind(MATRIZ_COEFICIENTES,DECISION[,5])

MATRIZ_COEFICIENTES


####ANALISIS RESIDUOS ####

RESIDUOS_21_0 <- SARIMA_21_0$fit$residuals

 #CORRELACION

ggAcf(RESIDUOS_21_0,type="correlation")

ggAcf(RESIDUOS_21_0,type="partial")

RESIDUOS_21_0 <- as.numeric(RESIDUOS_21_0)
Box.test(RESIDUOS_21_0)

  #TEST DE NORMALIDAD

Normality_Test(RESIDUOS_21_0,"SW")
Normality_Test(RESIDUOS_21_0,"JB")
Normality_Test(RESIDUOS_21_0,"AD") 


NORMALIDAD_JB <- Normality_Test(RESIDUOS_21_0,"JB")
NORMALIDAD_AD <- Normality_Test(RESIDUOS_21_0,"AD")
NORMALIDAD_SW <- Normality_Test(RESIDUOS_21_0,"SW")

P_VALUE_NORMALIDAD <-  rbind(c(NORMALIDAD_AD$P_Value,NORMALIDAD_JB$P_Value,NORMALIDAD_SW$P_Value))
ESTAD_NORMALIDAD <- rbind(c(NORMALIDAD_AD$Statistic,NORMALIDAD_JB$Statistic,NORMALIDAD_SW$Statistic))

NORMALIDAD <- matrix(NA,nrow=2,ncol=3)
NORMALIDAD[1,] <- P_VALUE_NORMALIDAD
NORMALIDAD[2,] <- ESTAD_NORMALIDAD
colnames(NORMALIDAD) <- c("Anderson Darling","Jarque-Bera","Shapiro-Wilk")
rownames(NORMALIDAD) <- c("P-Values","Estadisticos")

NORMALIDAD

  #TEST DE INCORRELACION

####PREDICCION ####

training <- window(LIBOR_DIF,start=c(2009,3),end=c(2018,1))
validation <- window(LIBOR_DIF,start=c(2018,1),end=c(2020,1))

  # NAIVE FORECAST
naive = snaive(training, h=length(validation))
plot(LIBOR_DIF, col="blue", xlab="AÑO", ylab="TIPO DE CAMBIO REAL", main="Seasonal Naive Forecast", type='l')
lines(naive$mean, col="red", lwd=2)

  # SARIMA
sarima_forecast = sarima.for(training,length(validation), 10,0,0)
plot(LIBOR_DIF, col="blue", xlab="AÑO", ylab="TIPO DE CAMBIO REAL", main="Serie Original & SARIMA(1,0,1) Forecast", type='l')
lines(sarima_forecast$pred, col="red", lwd=2)
sarima_forecast$pred



