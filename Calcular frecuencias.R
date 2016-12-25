---#librerias----
library(dplyr)
library(lubridate)
library(ggplot2)

----#abrir archivo despachos----

frecuencia_despachos <- read.csv2("trenes-despachados.csv", stringsAsFactors=FALSE)

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
frecuencia_habiles_A$intervalo_despacho_cab2 <- as.numeric((frecuencia_habiles_A$td_cab2- frecuencia_habiles_A$lag_frecuencia2)/60)

frecuencia_habiles_A <- frecuencia_habiles_A[!is.na(frecuencia_habiles_A$intervalo_despacho_cab1) & !is.na(frecuencia_habiles_A$intervalo_despacho_cab2),]

#LINEA B
frecuencia_habiles_B <- frecuencia_habiles %>% filter(FR1_LINEA=='B')
frecuencia_habiles_B$lag_frecuencia1 <-  lag(frecuencia_habiles_B$td_cab1,1)
frecuencia_habiles_B$lag_frecuencia2 <-  lag(frecuencia_habiles_B$td_cab2,1)

frecuencia_habiles_B$intervalo_despacho_cab1 <- as.numeric((frecuencia_habiles_B$td_cab1- frecuencia_habiles_B$lag_frecuencia1)/60)
frecuencia_habiles_B$intervalo_despacho_cab2 <- as.numeric((frecuencia_habiles_B$td_cab2- frecuencia_habiles_B$lag_frecuencia2)/60)

#LINEA C
frecuencia_habiles_C <- frecuencia_habiles %>% filter(FR1_LINEA=='C')
frecuencia_habiles_C$lag_frecuencia1 <-  lag(frecuencia_habiles_C$td_cab1,1)
frecuencia_habiles_C$lag_frecuencia2 <-  lag(frecuencia_habiles_C$td_cab2,1)

frecuencia_habiles_C$intervalo_despacho_cab1 <- as.numeric((frecuencia_habiles_C$td_cab1- frecuencia_habiles_C$lag_frecuencia1)/60)
frecuencia_habiles_C$intervalo_despacho_cab2 <- as.numeric((frecuencia_habiles_C$td_cab2- frecuencia_habiles_C$lag_frecuencia2)/60)

#LINEA D
frecuencia_habiles_D <- frecuencia_habiles %>% filter(FR1_LINEA=='D')
frecuencia_habiles_D$lag_frecuencia1 <-  lag(frecuencia_habiles_D$td_cab1,1)
frecuencia_habiles_D$lag_frecuencia2 <-  lag(frecuencia_habiles_D$td_cab2,1)

frecuencia_habiles_D$intervalo_despacho_cab1 <- as.numeric((frecuencia_habiles_D$td_cab1- frecuencia_habiles_D$lag_frecuencia1)/60)
frecuencia_habiles_D$intervalo_despacho_cab2 <- as.numeric((frecuencia_habiles_D$td_cab2- frecuencia_habiles_D$lag_frecuencia2)/60)

#LINEA E
frecuencia_habiles_E <- frecuencia_habiles %>% filter(FR1_LINEA=='E')
frecuencia_habiles_E$lag_frecuencia1 <-  lag(frecuencia_habiles_E$td_cab1,1)
frecuencia_habiles_E$lag_frecuencia2 <-  lag(frecuencia_habiles_E$td_cab2,1)

frecuencia_habiles_E$intervalo_despacho_cab1 <- as.numeric((frecuencia_habiles_E$td_cab1- frecuencia_habiles_E$lag_frecuencia1)/60)
frecuencia_habiles_E$intervalo_despacho_cab2 <- as.numeric((frecuencia_habiles_E$td_cab2- frecuencia_habiles_E$lag_frecuencia2)/60)

#LINEA H
frecuencia_habiles_H <- frecuencia_habiles %>% filter(FR1_LINEA=='H')
frecuencia_habiles_H$lag_frecuencia1 <-  lag(frecuencia_habiles_H$td_cab1,1)
frecuencia_habiles_H$lag_frecuencia2 <-  lag(frecuencia_habiles_H$td_cab2,1)

frecuencia_habiles_H$intervalo_despacho_cab1 <- as.numeric((frecuencia_habiles_H$td_cab1- frecuencia_habiles_H$lag_frecuencia1)/60)
frecuencia_habiles_H$intervalo_despacho_cab2 <- as.numeric((frecuencia_habiles_H$td_cab2- frecuencia_habiles_H$lag_frecuencia2)/60)

#frecuencia total de lineas

frec_tot <- rbind(frecuencia_habiles_A,frecuencia_habiles_B,frecuencia_habiles_C,frecuencia_habiles_D,frecuencia_habiles_E,frecuencia_habiles_H)

----#plot de frecuencias por mes----

boxplot(intervalo_despacho_cab1 ~ Mes,data = frecuencia_habiles_A[frecuencia_habiles_A$intervalo_despacho_cab1<50 & frecuencia_habiles_A$intervalo_despacho_cab1>0,])

boxplot(intervalo_despacho_cab1 ~ Mes,data = frecuencia_habiles_B[frecuencia_habiles_B$intervalo_despacho_cab1<50 & frecuencia_habiles_B$intervalo_despacho_cab1>0,])

boxplot(intervalo_despacho_cab1 ~ Mes,data = frecuencia_habiles_E[frecuencia_habiles_E$intervalo_despacho_cab1<50 & frecuencia_habiles_E$intervalo_despacho_cab1>0,])

ggplot(data = frec_tot[frec_tot$intervalo_despacho_cab1<15 & frec_tot$intervalo_despacho_cab1>0,], aes(x=Mes, y=intervalo_despacho_cab1)) + geom_boxplot(aes(fill=FR1_LINEA))
ggplot(data = frec_tot[frec_tot$intervalo_despacho_cab2<15 & frec_tot$intervalo_despacho_cab2>0,], aes(x=Mes, y=intervalo_despacho_cab2)) + geom_boxplot(aes(fill=FR1_LINEA))

mean(frecuencia_habiles_A$intervalo_despacho_cab1[frecuencia_habiles_A$intervalo_despacho_cab1<30 & frecuencia_habiles_A$intervalo_despacho_cab1>0])

aggregate(frecuencia_habiles_A[frecuencia_habiles_A$intervalo_despacho_cab1<30 & frecuencia_habiles_A$intervalo_despacho_cab1>0,c("intervalo_despacho_cab1","Mes")], list(factor(frecuencia_habiles_A$Mes[frecuencia_habiles_A$intervalo_despacho_cab1<30 & frecuencia_habiles_A$intervalo_despacho_cab1>0])), mean)

aggregate(frecuencia_habiles_B[frecuencia_habiles_B$intervalo_despacho_cab1<30 & frecuencia_habiles_B$intervalo_despacho_cab1>0,c("intervalo_despacho_cab1","Mes")], list(factor(frecuencia_habiles_B$Mes[frecuencia_habiles_B$intervalo_despacho_cab1<30 & frecuencia_habiles_B$intervalo_despacho_cab1>0])), mean)


ggplot(frecuencia_habiles_A[frecuencia_habiles_A$intervalo_despacho_cab1<50 & frecuencia_habiles_A$intervalo_despacho_cab1>0,], aes(intervalo_despacho_cab1)) +
  geom_density()

frec_cab1 <- select(frec_tot,FR1_FECHA,FR1_LINEA,FR1_TIPO,FR1_REGIST,FR1_ORDEN,FR1_TREN,FR1_CAUC1,FR1_SCAUC1,FR1_COCC1,FR1_KM,FR1_KMV,FR1_VIAC1,FR1_SALC1,td_cab1,Mes,intervalo_despacho_cab1)
frec_cab2 <- select(frec_tot,FR1_FECHA,FR1_LINEA,FR1_TIPO,FR1_REGIST,FR1_ORDEN,FR1_TREN,FR1_CAUC2,FR1_SCAUC2,FR1_COCC1,FR1_KM,FR1_KMV,FR1_VIAC2,FR1_SALC2,td_cab2,Mes,intervalo_despacho_cab2)

names <- c("FECHA","LINEA","TIPO","REGIST","ORDEN","TREN","CAUC","SCAUC","COCC","KM","KMV","VIAC","SALC","td_cab","Mes","intervalo_despacho")

names(frec_cab1) <- names
names(frec_cab2) <- names

frec_tota_filt <- rbind(frec_cab1,frec_cab2)
frec_tota_filt <- frec_tota_filt[!is.na(frec_tota_filt$intervalo_despacho),]
frec_tota_filt <- frec_tota_filt[frec_tota_filt$intervalo_despacho<=15 & frec_tota_filt$intervalo_despacho>0 ,]

frec_tota_filt <- frec_tota_filt %>% group_by(LINEA,Mes) %>% summarise(promedio_linea= mean(intervalo_despacho))


ggplot(frec_tota_filt, mapping = aes(x=as.factor(Mes),y=promedio_linea,group=LINEA))+geom_line(aes(colour = LINEA))
