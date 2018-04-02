library(readxl)
library(dplyr)
library(reshape2)
library(ggplot2)
library(stringr)
library(ggthemes)

options(scipen = 999)

Desagregado_por_Hora_para_Clarin_1_ <- read_excel("~/Downloads/Desagregado por Hora para Clarin (1).xlsx", 
                                                     sheet = "Pax x Hora", range = "B3:G117")

lineas <- c(rep('A',19),rep('B',19),rep('C',19),rep('D',19),rep('E',19),rep('H',19))

subte_planilla <- Desagregado_por_Hora_para_Clarin_1_

subte_planilla$linea <- lineas

colnames(subte_planilla)[1] <- 'horario' 

subte_planilla <- subte_planilla %>% filter(horario != 'Total')

plot(x= subte_planilla$horario,y=subte_planilla$`2013`)

ggplot(data = subte_planilla[subte_planilla$linea=='A',],aes(horario,`2013`,group = 1))+geom_point()+geom_line(color='red')

#transformar columnas a filas

subte_planilla$horario <- trimws(subte_planilla$horario)
graficos_planilla <- reshape2::melt(as.data.frame(subte_planilla), id=c('horario','linea'))

graficos_planilla$horario <-  as.numeric(gsub("([0-9]*).*","\\1",graficos_planilla$horario))


ggplot(graficos_planilla,aes(x=horario,y=value,group=variable,color=variable))+geom_line()+facet_grid(linea ~.)+theme_economist()

ggplot(graficos_planilla[graficos_planilla$variable %in% c(2016,2017),],aes(x=horario,y=value,group=variable,color=variable))+geom_line()+facet_grid(linea ~.)+
  scale_x_continuous(breaks = c(6,8,10,12,14,16,18,20,22))+theme_economist()

