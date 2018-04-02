#----#librerias----

library(dplyr)
library(funModeling)
library(Rcpp)
library(ggplot2)
library(plotly)
library(lubridate)
library(readr)
library(zoo)
library(scales)

#------analisis de datos------

molinetes_2013 <- molinetes_2013_junio_diciembre %>% mutate(mes= months(FECHA)) %>%select(mes,DESDE,HASTA,LINEA,PAX_PAGO,PAX_PASES_PAGOS,PAX_FRANQ,PAX_TOTAL) %>% 
  group_by(mes,DESDE,HASTA,LINEA) %>% summarise_all(sum,na.rm = TRUE) %>% filter(DESDE>='05:00:00')

flota <- estado.de.flota %>% filter(MES==8 & AÑO %in% c(2016,2017)) %>% select(AÑO,LINEA,EN_SERVICIO) %>% group_by(AÑO,LINEA) %>% summarise_all(sum,na.rm=TRUE)

ggplot(data = flota,aes(x=LINEA,y=EN_SERVICIO,fill=factor(AÑO)))+geom_bar(stat = 'identity', position=position_dodge())


molinetes_historico_2018 <- read_delim("molinetes_historico_2018.csv", 
                                       ";", escape_double = FALSE, col_types = cols(FECHA = col_date(format = "%d/%m/%Y")), 
                                       trim_ws = TRUE)

molinetes_2018 <- molinetes_historico_2018 %>% mutate(mes= months(FECHA)) %>%select(mes,DESDE,HASTA,LINEA,PAX_PAGOS,PAX_PASES_PAGOS,PAX_FRANQ,TOTAL) %>% 
  group_by(mes,DESDE,HASTA,LINEA) %>% summarise_all(sum,na.rm = TRUE) %>% filter(DESDE>='05:00:00')

molinetes_historico_2017 <- read_delim("molinetes_historico_2017.csv", 
                                       ";", escape_double = FALSE, col_types = cols(FECHA = col_date(format = "%d/%m/%Y")), 
                                       trim_ws = TRUE)

molinetes_2017 <- molinetes_historico_2017 %>% mutate(mes= months(FECHA)) %>%select(mes,DESDE,HASTA,LINEA,PAX_PAGOS,PAX_PASES_PAGOS,PAX_FRANQ,TOTAL) %>% 
  group_by(mes,DESDE,HASTA,LINEA) %>% summarise_all(sum,na.rm = TRUE) %>% filter(DESDE>='05:00:00')

remove(molinetes_historico_2017)
remove(molinetes_historico_2018)

levels(factor(molinetes_2017$mes))
levels(factor(molinetes_2017$mes,levels(factor(molinetes_2017$mes))[c(5,4,8,1,9,7,6,2,12,11,10,3)]))

molinetes_2017$mes <- factor(molinetes_2017$mes,levels(factor(molinetes_2017$mes))[c(5,4,8,1,9,7,6,2,12,11,10,3)])

molinetes_analisis <- rbind(molinetes_2017,molinetes_2018)

ggplot(molinetes_2018,aes(x=HASTA,y=TOTAL,color=LINEA)) + geom_line() + geom_vline(xintercept =hms('05:30:00'),color='red') + facet_grid(mes~.)

ggplot(molinetes_2017,aes(x=HASTA,y=TOTAL,color=LINEA)) + geom_line() + geom_vline(xintercept =hms('05:30:00'),color='red')+ facet_grid(mes~.)


#-----usuarios por horario extendido-----

horario_5am <- molinetes_2017 %>% filter(hour(HASTA) <= hour(hms('05:45:00')) & minute(HASTA)<=(minute(hms('05:30:00'))) & hour(HASTA) > hour(hms('00:00:00')))

horario_5am <- as.data.frame(horario_5am) %>% select(mes,LINEA,TOTAL)%>% group_by(mes,LINEA) %>% summarise(TOTAL=sum(TOTAL,na.rm = TRUE))

horario_5am <- horario_5am %>% filter(mes !='December')

horario_11pm_2017 <- molinetes_2017 %>% filter(hour(DESDE) >= hour(hms('23:45:00'))) %>% ungroup() %>% select(mes,LINEA,TOTAL)%>% group_by(mes,LINEA) %>% summarise(TOTAL=sum(TOTAL,na.rm = TRUE))

sum_2017 <- rbind(horario_5am,horario_11pm_2017)

sum_2017$anio <- 2017

horario_11pm_2018 <- molinetes_2018 %>% filter(hour(DESDE) >= hour(hms('23:45:00'))) %>% ungroup() %>% select(mes,LINEA,TOTAL)%>% group_by(mes,LINEA) %>% summarise(TOTAL=sum(TOTAL,na.rm = TRUE))

horario_11pm_2018$anio <- 2018

viajes_cambio_horario <- rbind(sum_2017,horario_11pm_2018)

viajes_cambio_horario$mes_anio <- zoo::as.yearmon(paste(viajes_cambio_horario$mes,viajes_cambio_horario$anio),'%B%Y')

ggplot(sum_2017,aes(x=mes,y=TOTAL,fill=LINEA)) + geom_bar(stat = "identity",position = "dodge")

ggplot(viajes_cambio_horario,aes(x=as.factor(mes_anio),y=TOTAL,fill=LINEA)) + geom_bar(stat = "identity",position = "dodge")   +
  xlab('Mes')


total_viajes <- viajes_cambio_horario %>% ungroup() %>% select(mes_anio,TOTAL) %>% group_by(mes_anio) %>% summarise(TOTAL=sum(TOTAL))

ts.plot(total_viajes)