---#librerias----
library(dplyr)
library(lubridate)

----#abrir archivo despachos----

frecuencia_despachos <- read.csv2("~/Documentos/Proyectos/Subtes/trenes-despachados.csv", stringsAsFactors=FALSE)

----#analisis de frecuencias----

frecuencia_despachos$td_cab1 <- as.POSIXct(paste(frecuencia_despachos$FR1_FECHA,frecuencia_despachos$FR1_SALC1),format="%d/%m/%Y %H:%M:%S")
frecuencia_despachos$td_cab2 <- as.POSIXct(paste(frecuencia_despachos$FR1_FECHA,frecuencia_despachos$FR1_SALC2),format="%d/%m/%Y %H:%M:%S")

frecuencia_habiles <- frecuencia_despachos %>% filter(FR1_TIPO=="H")

mean.POSIXct(frecuencia_habiles$td_cab1[1:4])
int_diff(frecuencia_habiles$td_cab1[1:4])
abs(frecuencia_habiles$td_cab1[1] - frecuencia_habiles$td_cab1[2])
frecuencia_habiles$lag_frecuencia1 <- NULL
frecuencia_habiles$lag_frecuencia2 <- NULL
frecuencia_habiles$intervalo_despacho_cab1 <- NULL

frecuencia_habiles$FR1_FECHA <-  as.POSIXct(frecuencia_habiles$FR1_FECHA,format="%d/%m/%Y")
frecuencia_habiles <- frecuencia_habiles %>% mutate(Mes=format(FR1_FECHA,"%Y-%m"))

#LINEA A
frecuencia_habiles_A <- frecuencia_habiles %>% filter(FR1_LINEA=='A')
frecuencia_habiles_A$lag_frecuencia1 <-  lag(frecuencia_habiles_A$td_cab1,1)
frecuencia_habiles_A$lag_frecuencia2 <-  lag(frecuencia_habiles_A$td_cab2,1)

frecuencia_habiles_A$intervalo_despacho_cab1 <- as.numeric((frecuencia_habiles_A$td_cab1- frecuencia_habiles_A$lag_frecuencia1)/60)
frecuencia_habiles_A$intervalo_despacho_cab2 <- (frecuencia_habiles_A$td_cab2- frecuencia_habiles_A$lag_frecuencia2)/60


#LINEA B
frecuencia_habiles_B <- frecuencia_habiles %>% filter(FR1_LINEA=='B')
frecuencia_habiles_B$lag_frecuencia1 <-  lag(frecuencia_habiles_B$td_cab1,1)
frecuencia_habiles_B$lag_frecuencia2 <-  lag(frecuencia_habiles_B$td_cab2,1)

frecuencia_habiles_B$intervalo_despacho_cab1 <- (frecuencia_habiles_B$td_cab1- frecuencia_habiles_B$lag_frecuencia1)/60
frecuencia_habiles_B$intervalo_despacho_cab2 <- (frecuencia_habiles_B$td_cab2- frecuencia_habiles_B$lag_frecuencia2)/60

#LINEA C
frecuencia_habiles_C <- frecuencia_habiles %>% filter(FR1_LINEA=='C')
frecuencia_habiles_C$lag_frecuencia1 <-  lag(frecuencia_habiles_C$td_cab1,1)
frecuencia_habiles_C$lag_frecuencia2 <-  lag(frecuencia_habiles_C$td_cab2,1)

frecuencia_habiles_C$intervalo_despacho_cab1 <- (frecuencia_habiles_C$td_cab1- frecuencia_habiles_C$lag_frecuencia1)/60
frecuencia_habiles_C$intervalo_despacho_cab2 <- (frecuencia_habiles_C$td_cab2- frecuencia_habiles_C$lag_frecuencia2)/60

#LINEA D
frecuencia_habiles_D <- frecuencia_habiles %>% filter(FR1_LINEA=='D')
frecuencia_habiles_D$lag_frecuencia1 <-  lag(frecuencia_habiles_D$td_cab1,1)
frecuencia_habiles_D$lag_frecuencia2 <-  lag(frecuencia_habiles_D$td_cab2,1)

frecuencia_habiles_D$intervalo_despacho_cab1 <- (frecuencia_habiles_D$td_cab1- frecuencia_habiles_D$lag_frecuencia1)/60
frecuencia_habiles_D$intervalo_despacho_cab2 <- (frecuencia_habiles_D$td_cab2- frecuencia_habiles_D$lag_frecuencia2)/60

#LINEA E
frecuencia_habiles_E <- frecuencia_habiles %>% filter(FR1_LINEA=='E')
frecuencia_habiles_E$lag_frecuencia1 <-  lag(frecuencia_habiles_E$td_cab1,1)
frecuencia_habiles_E$lag_frecuencia2 <-  lag(frecuencia_habiles_E$td_cab2,1)

frecuencia_habiles_E$intervalo_despacho_cab1 <- (frecuencia_habiles_E$td_cab1- frecuencia_habiles_E$lag_frecuencia1)/60
frecuencia_habiles_E$intervalo_despacho_cab2 <- (frecuencia_habiles_E$td_cab2- frecuencia_habiles_E$lag_frecuencia2)/60

#LINEA H
frecuencia_habiles_H <- frecuencia_habiles %>% filter(FR1_LINEA=='H')
frecuencia_habiles_H$lag_frecuencia1 <-  lag(frecuencia_habiles_H$td_cab1,1)
frecuencia_habiles_H$lag_frecuencia2 <-  lag(frecuencia_habiles_H$td_cab2,1)

frecuencia_habiles_H$intervalo_despacho_cab1 <- (frecuencia_habiles_H$td_cab1- frecuencia_habiles_H$lag_frecuencia1)/60
frecuencia_habiles_H$intervalo_despacho_cab2 <- (frecuencia_habiles_H$td_cab2- frecuencia_habiles_H$lag_frecuencia2)/60

----#plot de frecuencias por mes----

boxplot(intervalo_despacho_cab1 ~ Mes,data = frecuencia_habiles_A[frecuencia_habiles_A$intervalo_despacho_cab1<200 & frecuencia_habiles_A$intervalo_despacho_cab1>0,])
