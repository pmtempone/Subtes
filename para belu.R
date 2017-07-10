library(dplyr)
library(ggplot2)

resumido_belu <- subte.estado.flota %>% select(anio_mes,LINEA,EN_SERVICIO,CORRECTIVO) %>% dplyr::group_by(anio_mes,LINEA) %>%
                  dplyr::summarise(servicio=sum(EN_SERVICIO,na.rm = TRUE),correctivo=sum(CORRECTIVO,na.rm = TRUE)) %>% 
                  mutate(correctivo_sobretotal = correctivo/(correctivo+servicio))
                  

resumido_belu[is.na(resumido_belu)] <- 0

resumido_belu$fecha <- as.Date(paste(as.character(resumido_belu$anio_mes),'01',sep = ""),format = '%Y%m%d')

ggplot(resumido_belu,aes(x=fecha,y=correctivo_sobretotal,colour=LINEA)) + geom_line()
