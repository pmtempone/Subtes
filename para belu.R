library(dplyr)
library(plotly)
library(ggplot2)

resumido_belu <- subte.estado.flota %>% select(anio_mes,LINEA,EN_SERVICIO,CORRECTIVO,RG,RP) %>% dplyr::group_by(anio_mes,LINEA) %>%
                  dplyr::summarise(servicio=sum(EN_SERVICIO,na.rm = TRUE),correctivo=sum(CORRECTIVO,na.rm = TRUE),revision_gral=sum(RG,na.rm = TRUE),revision_part=sum(RP,na.rm = TRUE)) %>% 
                  mutate(correctivo_sobretotal = correctivo/(correctivo+servicio))
                  

resumido_belu[is.na(resumido_belu)] <- 0

resumido_belu$fecha <- as.Date(paste(as.character(resumido_belu$anio_mes),'01',sep = ""),format = '%Y%m%d')

ggplot(resumido_belu,aes(x=fecha,y=correctivo_sobretotal,colour=LINEA)) + geom_line()

ggplotly(ggplot(resumido_belu,aes(x=fecha,y=revision_gral,colour=LINEA)) + geom_line())

ggplotly(ggplot(resumido_belu,aes(x=fecha,y=revision_part,colour=LINEA)) + geom_line())

library(xlsx)

write.table(resumido_belu,"resumido_belu.csv",sep = ";",dec = ",",row.names = FALSE)
