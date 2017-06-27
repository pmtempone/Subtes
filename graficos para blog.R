p <- ggplotly(ggplot(data = CANT_SERV,mapping = aes(x=as.factor(anio_mes),y=Unique_Elements,group=LINEA))+geom_line(aes(colour = LINEA))+ylab("En Servicio")+theme(text = element_text(size=15),
                                                                                                                                                     axis.text.x = element_text(angle=90, vjust=1)) +xlab(""))
Sys.setenv("plotly_username"="pmtempone")
Sys.setenv("plotly_api_key"="5KKzs9V2ZIC5Gm2tQSqP")

api_create(p)

library(plotly)
library(dplyr)
library(reshape2)
servicio_lineaA <- CANT_SERV %>% filter(LINEA=='A')

melt_servicio <- melt(CANT_SERV,value.name = "value")

lineas_servicio <- data.frame(anio_mes = factor(unique(CANT_SERV$anio_mes)),
                              linea_a = CANT_SERV$Unique_Elements[CANT_SERV$LINEA=='A'],
                              linea_b = CANT_SERV$Unique_Elements[CANT_SERV$LINEA=='B'],
                              linea_c = CANT_SERV$Unique_Elements[CANT_SERV$LINEA=='C'],
                              linea_d = CANT_SERV$Unique_Elements[CANT_SERV$LINEA=='D'],
                              linea_e = CANT_SERV$Unique_Elements[CANT_SERV$LINEA=='E'],
                              linea_h = CANT_SERV$Unique_Elements[CANT_SERV$LINEA=='H'],
                              linea_p = CANT_SERV$Unique_Elements[CANT_SERV$LINEA=='P'])


p = plot_ly(data=lineas_servicio, x = ~ anio_mes, y = ~ linea_a, name = 'A', type = 'scatter', mode = 'lines') %>%
     add_trace(y = ~lineas_servicio$linea_b, name = 'B', mode = 'lines') %>%
     add_trace(y = ~lineas_servicio$linea_c, name = 'C', mode = 'lines') %>%
     add_trace(y = ~lineas_servicio$linea_d, name = 'D', mode = 'lines') %>%
     add_trace(y = ~lineas_servicio$linea_e, name = 'E', mode = 'lines') %>%
     add_trace(y = ~lineas_servicio$linea_h, name = 'H', mode = 'lines') %>%
     add_trace(y = ~lineas_servicio$linea_p, name = 'P', mode = 'lines') %>%
     layout(xaxis = list(title = "Mes"),yaxis = list(title="cantidad en servicio"))
  
  

linea_correctivo <- dcast(CANT_CORRECTIVO, anio_mes ~ LINEA)

linea_correctivo[is.na(linea_correctivo)] <- 0

q = plot_ly(data=linea_correctivo, x = ~ factor(anio_mes), y = ~ A, name = 'A', type = 'scatter', mode = 'lines') %>%
  add_trace(y = ~linea_correctivo$B, name = 'B', mode = 'lines') %>%
  add_trace(y = ~linea_correctivo$C, name = 'C', mode = 'lines') %>%
  add_trace(y = ~linea_correctivo$D, name = 'D', mode = 'lines') %>%
  add_trace(y = ~linea_correctivo$E, name = 'E', mode = 'lines') %>%
  add_trace(y = ~linea_correctivo$H, name = 'H', mode = 'lines') %>%
  add_trace(y = ~linea_correctivo$P, name = 'P', mode = 'lines') %>%
  layout(xaxis = list(title = "Mes"),yaxis = list(title="Correctivos"))
q
api_create(p)
api_create(q)
api_create(f)
