#----#librerias----

library(dplyr)
library(funModeling)
library(Rcpp)
library(ggplot2)
library(plotly)

#---#carga de datos----

trenes.despachados <- read.csv2("formaciones-despachadas.csv",stringsAsFactors = FALSE)
subte.estado.flota <- read.csv("estado-de-flota.csv", sep=";")

str(trenes.despachados)
summary(trenes.despachados)
df_status(trenes.despachados)

head(as.Date(trenes.despachados$X...FR1_FECHA,format = "%d/%m/%Y"))

trenes.despachados$FR1_FECHA <- as.Date(trenes.despachados$X...FR1_FECHA,format = "%d/%m/%Y")

#----#analisis estado flota----

subte.estado.flota$anio_mes <-  subte.estado.flota$ANIO*100+subte.estado.flota$X...MES

df_status(subte.estado.flota)

ggplot(data = subte.estado.flota,mapping = aes(x=as.factor(anio_mes),y=EN_SERVICIO,group=1))+geom_line()

CANT_SERV <- subte.estado.flota %>%                    # take the data.frame "data"
  filter(!is.na(EN_SERVICIO)) %>%
  group_by(anio_mes,LINEA) %>%          # Then, with the filtered data, group it by "anio_mes,LINEA"
  summarise(Unique_Elements = sum(EN_SERVICIO))   # Now summarise with unique elements per group

ggplot(data = CANT_SERV,mapping = aes(x=anio_mes,y=Unique_Elements,group=LINEA))+geom_line(aes(colour = LINEA))


CANT_CORRECTIVO <- subte.estado.flota %>%                    # take the data.frame "data"
  filter(!is.na(CORRECTIVO)) %>%
  group_by(anio_mes,LINEA) %>%          # Then, with the filtered data, group it by "anio_mes,LINEA"
  summarise(Correctivo = sum(CORRECTIVO))

ggplot(data = CANT_CORRECTIVO,mapping = aes(x=as.factor(anio_mes),y=Correctivo,group=LINEA))+geom_line(aes(colour = LINEA))+xlab("Año-Mes")

Cant_por_linea <- subte.estado.flota %>%                    # take the data.frame "data"
  group_by(anio_mes,LINEA) %>%          # Then, with the filtered data, group it by "anio_mes,LINEA"
  summarise(Correctivo = sum(CORRECTIVO,na.rm=TRUE),En_servicio=sum(EN_SERVICIO,na.rm=TRUE),Reserva=sum(RESERVA,na.rm=TRUE),RG=sum(RG,na.rm=TRUE),RP=sum(RP,na.rm=TRUE),Judicial=sum(JUDICIAL,na.rm=TRUE),Alistamiento=sum(ALISTAMIENTO,na.rm=TRUE))

p <- ggplot(data = CANT_CORRECTIVO,mapping = aes(x=as.factor(anio_mes),y=Correctivo,group=LINEA))+geom_line(aes(colour = LINEA))

ggplotly(p)



