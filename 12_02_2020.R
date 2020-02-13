library(dplyr)
library(ggplot2)
library(ggthemes)
library(extrafont)
#Si es la primera vez que se usa el paquete extrafont, hay que ejecutar font_import() tras cargarlo

confirmados <- readr::read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/time_series/time_series_2019-ncov-Confirmed.csv")

muertes <- readr::read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/time_series/time_series_2019-ncov-Deaths.csv")

recuperados <- readr::read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/time_series/time_series_2019-ncov-Recovered.csv")

#Transformamos el dataset de confirmaciones para manejarlo con el tidyverse
conf2 <- reshape2::melt(confirmados, id.vars = colnames(confirmados)[1:4])

#Las fechas son un dolor de cabeza en este dataset porque aparecen en un formato muy incomodo. Este codigo me ha servido para lo que necesitaba:
conf2$Fechas <- as.POSIXct(paste("0",as.character(conf2$variable),sep=""), format = "%m/%e/%y %H:%M")

#Empezamos a crear el dataset que usaremos para el grafico. Vamos ejecutando los siguientes pasos
dat_graf <- conf2 %>%                                                       #Del dataset conf2...
  filter(`Province/State` == "Hubei") %>%                                   #...quedate solo con las observaciones de Hubei...
  mutate(cifra_dif = c(NA, diff(value))) %>%                                #...calcula la diferencia en confirmados entre una actualizacion y otra....
  filter(cifra_dif>50) %>%                                                  #...quita aquellas cuya diferencia sea menor de 50 confirmados...
  mutate(cifra_dif = c(NA, diff(value)),fechas_dif = c(NA, diff(Fechas)))   #...y cuando quites esas actualizaciones poco utiles, calcula otra vez la diferencia.

#Hay una actualizacion que esta como partida en dos. Nos sale que en dos horas se confirman tantos casos como en las 40 horas anteriores.
#Juntamos ambas actualizaciones para que el numero de confirmaciones por hora sea coherente.
dat_graf[16,8:9] <- dat_graf[16,8:9]+dat_graf[17,8:9]
dat_graf <- dat_graf[-17,]

dat_graf %>%
  filter(cifra_dif/fechas_dif < 500) %>% #IMPORTANTE: este argumento sirve para quitar la ultima actualizacion con el nuevo criterio de diagnostico
                                         #Si quieres que aparezca, ponle la almohadilla delante para comentarlo y que no se ejecute
  ggplot(aes(x = Fechas, y = cifra_dif/fechas_dif)) +    #Al hacer cifra_dif/fechas_dif sacamos las confirmaciones por hora
  geom_point(size = 3) + geom_line(size = 1.05) + 
  stat_smooth(se = F, size = 1.1) +
  scale_x_datetime(date_breaks = "2 days", date_labels = "%d/%m/%y") +
  labs(title = "Número medio de casos confirmados por hora del COVID-19 en la región de\nHubei (China) a 11-02-2020",
       subtitle = "Fuente: Centro para Ciencia de Sistemas e Ingeniería (CCSE) de la Universidad John Hopkins\n#DatosDeMiercoles de @R4DS_es, semana 12-02-2020",
       x = "Fecha", y = "Casos confirmados por hora transcurrida",
       caption = "@Picanumeros") +
  theme_bw(base_size = 16) + 
  theme(text = element_text(family = "Liberation Sans"),
        #panel.background = element_rect(fill = "#ffffcc"),
        #panel.grid = element_line(colour = "#919191", alpha =.5),
        plot.background = element_rect(fill = "#BFD5E3")) #+
  #LAS LINEAS QUE AQUI DEBAJO SE ADJUNTAN SON PARA INTRODUCIR LA FLECHA INDICADORA DEL CAMBIO EN EL CRITERIO DE DIAGNOSTICO
  # geom_segment(x = as.POSIXct("08/02/2020 01:00", format = "%d/%M/%Y %H:%M"), 
  #         xend = as.POSIXct("12/02/2020 18:00", format = "%d/%M/%Y %H:%M"),
  #         y = 300, yend = 580, arrow = arrow(), size = 1.05, col = "red") + 
  # annotate("text",
  #          x = as.POSIXct("06/02/2020 01:00", format = "%d/%M/%Y %H:%M"),
  #          y = 300, label = "Cambio de criterio\nen el diagnóstico", size = 5,
  #          col = "red", family = "Franklin Gothic Demi")

#Para el grafico de muertes, seguimos los mismos pasos grosso modo
fall2 <- reshape2::melt(muertes, id.vars = colnames(muertes)[1:4])
fall2$Fechas <- as.POSIXct(paste("0",as.character(fall2$variable),sep=""), format = "%m/%e/%y %H:%M")

dat_graf <- fall2 %>%
  filter(`Province/State` == "Hubei") %>%
  mutate(cifra_dif = c(NA, diff(value))) %>%
  filter(cifra_dif>20) %>%
  mutate(cifra_dif = c(NA, diff(value)),fechas_dif = c(NA, diff(Fechas)))

dat_graf[11,8:9] <- dat_graf[11,8:9]+dat_graf[12,8:9]
dat_graf <- dat_graf[-12,]

dat_graf %>%
  filter(cifra_dif/fechas_dif < 7.5) %>%
  ggplot(aes(x = Fechas, y = cifra_dif/fechas_dif)) +
  geom_point(size = 3) + geom_line(size = 1.05) + 
  stat_smooth(se = F, size = 1.1) +
  scale_x_datetime(date_breaks = "2 days", date_labels = "%d/%m/%y") +
  labs(title = "Número medio de muertes por hora causadas por el COVID-19 en la región de\nHubei (China) a 13-02-2020 18:00 GMT",
       subtitle = "Fuente: Centro para Ciencia de Sistemas e Ingeniería (CCSE) de la Universidad John Hopkins\n#DatosDeMiercoles de @R4DS_es, semana 12-02-2020",
       x = "Fecha", y = "Muertes por hora transcurrida",
       caption = "@Picanumeros") +
  theme_bw(base_size = 16) + 
  theme(text = element_text(family = "Liberation Sans"),
        #panel.background = element_rect(fill = "#ffffcc"),
        #panel.grid = element_line(colour = "#919191", alpha =.5),
         plot.background = element_rect(fill = "#BFD5E3"))
