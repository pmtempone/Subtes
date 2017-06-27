###Cuántas interrupciones de servicio hay en la red de subte, general y por línea.

----#librerias----

library(ggplot2)
library(dplyr)
library(plyr)

----#analisis-----
trenes.despachados <- read.csv2("trenes-despachados.csv")
str(trenes.despachados)

head(trenes.despachados)

trenes.despachados$FR1_FECHA <-  as.POSIXct(as.character(trenes.despachados$X...FR1_FECHA),format="%d/%m/%Y")
trenes.despachados$FR1_SALC1 <- factor(substr(as.character(trenes.despachados$FR1_SALC1),1,2))
trenes.despachados$FR1_SALC2 <- factor(substr(as.character(trenes.despachados$FR1_SALC2),1,2))

trenes_cab1 <- trenes.despachados[,c(1:6,7,8,11,13,14,15,17)]
trenes_cab2 <- trenes.despachados[,c(1:6,9,10,12,13,14,16,18)]



trenes_desp <- trenes.despachados %>% mutate(interrupcion_cab1=ifelse(FR1_CAUC1!="",1,0),interrupcion_cab2=ifelse(FR1_CAUC2!="",1,0),sin_int_cab1=ifelse(FR1_CAUC1=="",1,0),sin_int_cab2=ifelse(FR1_CAUC2=="",1,0)) 

trenes_desp <- trenes_desp %>% mutate(interrupciones=(interrupcion_cab1+interrupcion_cab2),sin_int=(sin_int_cab1+sin_int_cab2))

interrupciones_x_mes <- trenes_desp %>% select(FR1_FECHA,FR1_LINEA,interrupciones) %>% group_by(FR1_FECHA,FR1_LINEA) %>% dplyr::summarise(Interrupciones = sum(interrupciones,na.rm=TRUE))

f <- ggplotly(ggplot(data = interrupciones_x_mes,mapping = aes(x=FR1_FECHA,y=Interrupciones,fill=FR1_LINEA))+geom_line(aes(colour = FR1_LINEA))+xlab('Fecha')+labs(colour='LINEA'))


sum(interrupciones_x_mes$Interrupciones)

interrupciones_x_mes <- interrupciones_x_mes %>% filter(FR1_LINEA!="P")
interrupciones_promedio <- interrupciones_x_mes %>% mutate(Mes=format(FR1_FECHA,"%Y-%m"))%>% ungroup() %>% select (Mes,FR1_LINEA,Interrupciones) %>% group_by(Mes,FR1_LINEA) %>% dplyr::summarise(mean_interrupciones=mean(Interrupciones))

tail(format(interrupciones_x_mes$FR1_FECHA,"%Y-%m")) 

trenes_desp_sept <- interrupciones_x_mes %>% filter(FR1_FECHA>="2016-09-01") %>% filter(FR1_LINEA!="P") %>% summarise(prom=mean(Interrupciones))
mean(interrupciones_x_mes$Interrupciones[interrupciones_x_mes$FR1_FECHA>="2016-09-01"])
interrupcione_mes_total <- interrupciones_x_mes %>% mutate(Mes=format(FR1_FECHA,"%Y-%m"))%>% ungroup() %>% select (Mes,Interrupciones) %>% group_by(Mes) %>% dplyr::summarise(mean_interrupciones=mean(Interrupciones)) %>% ungroup()

ggplot(data = interrupcione_mes_total,mapping = aes(x=Mes,y=mean_interrupciones))+geom_line()+
  xlab("") + ylab("Daily Views")

qplot(data = interrupcione_mes_total,x=Mes,y=mean_interrupciones,geom = "line")

