----#librerias----

library(dplyr)
library(funModeling)
library(Rcpp)
library(ggplot2)
library(plotly)

----#analisis----

Cor_flota <- subte.estado.flota %>%                    # take the data.frame "data"
  group_by(Anio_Mes=as.factor(anio_mes),LINEA,FLOTA) %>%          # Then, with the filtered data, group it by "anio_mes,LINEA"
  summarise(Correctivo = sum(CORRECTIVO,na.rm=TRUE),En_servicio=sum(EN_SERVICIO,na.rm=TRUE),Reserva=sum(RESERVA,na.rm=TRUE),RG=sum(RG,na.rm=TRUE),RP=sum(RP,na.rm=TRUE),Judicial=sum(JUDICIAL,na.rm=TRUE),Alistamiento=sum(ALISTAMIENTO,na.rm=TRUE))

cor_plot <- ggplot(data = Cor_flota,mapping = aes(x=Anio_Mes,y=Correctivo,group=FLOTA))+geom_line(aes(colour = paste(LINEA,FLOTA)))+ylab("En Correctivo")+theme(text = element_text(size=15),axis.text.x = element_text(angle=90, vjust=1),strip.background = element_rect(colour="red", fill="#CCCCFF")) +xlab("Año-Mes")+facet_grid(LINEA~.)+ scale_colour_manual(values=c(1:22))

cor_plot

