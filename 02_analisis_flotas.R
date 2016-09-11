----#librerias----

library(dplyr)
library(funModeling)
library(Rcpp)
library(ggplot2)
library(plotly)


---#analisis---
  
  
  df_status(subte.estado.flota)

ggplot(data = subte.estado.flota,mapping = aes(x=anio_mes,y=EN_SERVICIO,group=1))+geom_line()

CANT_SERV <- subte.estado.flota %>%                    # take the data.frame "data"
  filter(!is.na(EN_SERVICIO)) %>%
  group_by(anio_mes,LINEA) %>%          # Then, with the filtered data, group it by "bb"
  summarise(Unique_Elements = sum(EN_SERVICIO))   # Now summarise with unique elements per group

servicio <- ggplot(data = CANT_SERV,mapping = aes(x=anio_mes,y=Unique_Elements,group=LINEA))+geom_line(aes(colour = LINEA))


CANT_CORRECTIVO <- subte.estado.flota %>%                    # take the data.frame "data"
  filter(!is.na(CORRECTIVO)) %>%
  group_by(anio_mes,LINEA) %>%          # Then, with the filtered data, group it by "bb"
  summarise(Correctivo = sum(CORRECTIVO))

correctivo <- ggplot(data = CANT_CORRECTIVO,mapping = aes(x=anio_mes,y=Correctivo,group=LINEA))+geom_line(aes(colour = LINEA))

ggplotly(servicio)
