library(dplyr)
library(ggplot2)
library(ggthemes)
library(extrafont)
#Si es la primera vez que se usa el paquete extrafont, hay que ejecutar font_import() tras cargarlo

confirmados <- readr::read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Confirmed.csv")

muertes <- readr::read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Deaths.csv")

recuperados <- readr::read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Recovered.csv")

#Transformamos el dataset de confirmaciones para manejarlo con el tidyverse
conf2 <- reshape2::melt(confirmados, id.vars = colnames(confirmados)[1:4])

#Las fechas son un dolor de cabeza en este dataset porque aparecen en un formato muy incomodo. Este codigo me ha servido para lo que necesitaba:
conf2$Fechas <- as.POSIXct(paste("0",as.character(conf2$variable),sep=""), format = "%m/%e/%y")

#Empezamos a crear el dataset que usaremos para el grafico. Vamos ejecutando los siguientes pasos
dat_graf <- conf2 %>%                                                       #Del dataset conf2...
  filter(`Province/State` == "Hubei") %>%                                   #...quedate solo con las observaciones de Hubei...
  mutate(cifra_dif = c(NA, diff(value))) %>%                                #...calcula la diferencia en confirmados entre una actualizacion y otra....
  filter(cifra_dif>50) %>%                                                  #...quita aquellas cuya diferencia sea menor de 50 confirmados...
  mutate(cifra_dif = c(NA, diff(value)),fechas_dif = c(NA, diff(Fechas)))   #...y cuando quites esas actualizaciones poco utiles, calcula otra vez la diferencia.

dat_graf %>%
  mutate(aconte = factor(ifelse(Fechas < as.POSIXct("2020-02-12"), 1,
                        ifelse(Fechas > as.POSIXct("2020-02-15"), 1, 2)))
        ) %>%
  filter(aconte == 1) %>% #IMPORTANTE: este argumento sirve para quitar la ultima actualizacion con el nuevo criterio de diagnostico
                                         #Si quieres que aparezca, ponle la almohadilla delante para comentarlo y que no se ejecute
  ggplot(aes(x = Fechas, y = cifra_dif/fechas_dif)) +    #Al hacer cifra_dif/fechas_dif sacamos las confirmaciones por hora
               , group = aconte
             )) +
  geom_point(size = 3) + geom_line(size = 1.05) +
  stat_smooth(se = F, size = 1.1) +
  scale_x_datetime(date_breaks = "2 days", date_labels = "%d/%m") +
  labs(title = "Número medio de casos confirmados por día del COVID-19 en la región de\nHubei (China) a 25-02-2020",
       subtitle = "Fuente: Centro para Ciencia de Sistemas e Ingeniería (CCSE) de la Universidad John Hopkins\n#DatosDeMiercoles de @R4DS_es, semana 12-02-2020",
       x = "Fecha", y = "Casos confirmados por día transcurrido",
       caption = "Descontados datos de días 13 y 14 de febrero por presentar valores atípicos debido al cambio en el método de diagnóstico\n@Picanumeros") +
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
fall2$Fechas <- as.POSIXct(paste("0",as.character(fall2$variable),sep=""), format = "%m/%e/%y")

fall_graf <- fall2 %>%
  filter(`Province/State` == "Hubei") %>%
  mutate(cifra_dif = c(NA, diff(value))) %>%
  filter(cifra_dif>0) %>%
  mutate(cifra_dif = c(NA, diff(value)),fechas_dif = c(NA, diff(Fechas)))

fall_graf %>%
  ggplot(aes(x = Fechas, y = cifra_dif/fechas_dif)) +
  geom_point(size = 3) + geom_line(size = 1.05) +
  stat_smooth(se = F, size = 1.1) +
  scale_x_datetime(date_breaks = "2 days", date_labels = "%d/%m") +
  labs(title = "Número medio de muertes por día causadas por el COVID-19 en la región de\nHubei (China) a 24-02-2020",
       subtitle = "Fuente: Centro para Ciencia de Sistemas e Ingeniería (CCSE) de la Universidad John Hopkins\n#DatosDeMiercoles de @R4DS_es, semana 12-02-2020",
       x = "Fecha", y = "Muertes por día transcurrido",
       caption = "@Picanumeros") +
  theme_bw(base_size = 16) +
  theme(text = element_text(family = "Liberation Sans"),
        #panel.background = element_rect(fill = "#ffffcc"),
        #panel.grid = element_line(colour = "#919191", alpha =.5),
         plot.background = element_rect(fill = "#BFD5E3"))

#### DATOS COREA DEL SUR E ITALIA ####

korea <- conf2 %>%
  filter(`Country/Region` == "South Korea") %>%  #PARA HACERLO CON ITALIA EL CODIGO ES EL MISMO, PERO CAMBIANDO
                                                 #"South Korea" POR "Italy" EN ESTA MISMA LINEA
  mutate(cifra_dif = c(NA, diff(value))) %>%
  mutate(cifra_dif = c(NA, diff(value)),fechas_dif = c(NA, diff(Fechas)))

#Por si te apetece hacer un grafico con la evolucion de los ultimos 5 dias, aunque va a servir de poco
korea[29:34,] %>%  
  ggplot(aes(x = Fechas, y = cifra_dif/fechas_dif
                      #, group = aconte
)) +
  geom_point(size = 3) + geom_line(size = 1.05) +
  stat_smooth(se = F, size = 1.1) +
  scale_x_datetime(date_breaks = "2 days", date_labels = "%d/%m") +
  labs(title = "Número medio de casos confirmados por día del COVID-19 en Corea del Sur a 25-02-2020",
       subtitle = "Fuente: Centro para Ciencia de Sistemas e Ingeniería (CCSE) de la Universidad John Hopkins\n#DatosDeMiercoles de @R4DS_es, semana 12-02-2020",
       x = "Fecha", y = "Casos confirmados por día transcurrido",
       caption = "@Picanumeros") +
  theme_bw(base_size = 16) +
  theme(text = element_text(family = "Liberation Sans"),
        #panel.background = element_rect(fill = "#ffffcc"),
        #panel.grid = element_line(colour = "#919191", alpha =.5),
        plot.background = element_rect(fill = "#BFD5E3"))

#### GRÁFICOS DE LA TASA DE LETALIDAD (CASE FATALITY RATE) ####

#Creamos las bases de datos con los casos confirmados y fallecimientos de la misma forma que antes
conf2 <- reshape2::melt(confirmados, id.vars = colnames(confirmados)[1:4])
conf2$Fechas <- as.POSIXct(paste("0",as.character(conf2$variable),sep=""), format = "%m/%e/%y")

dat_graf <- conf2 %>%
  filter(`Province/State` == "Hubei") %>%
  mutate(cifra_dif = c(NA, diff(value))) %>%
  filter(cifra_dif>50) %>%
  mutate(cifra_dif = c(NA, diff(value)),fechas_dif = c(NA, diff(Fechas)))

fall2 <- reshape2::melt(muertes, id.vars = colnames(muertes)[1:4])
fall2$Fechas <- as.POSIXct(paste("0",as.character(fall2$variable),sep=""), format = "%m/%e/%y")

fall_graf <- fall2 %>%
  filter(`Province/State` == "Hubei") %>%
  mutate(cifra_dif = c(NA, diff(value))) %>%
  filter(cifra_dif>0) %>%
  mutate(cifra_dif = c(NA, diff(value)),fechas_dif = c(NA, diff(Fechas)))

recu2 <- reshape2::melt(recuperados, id.vars = colnames(recuperados)[1:4])
recu2$Fechas <- as.POSIXct(paste("0",as.character(recu2$variable),sep=""), format = "%m/%e/%y")

#Creamos ahora un dataset para las recuperaciones de manera analoga, pero al final le metemos un right_join para que nos junte con el dataset de casos
recu_graf <- recu2 %>%
  filter(`Province/State` == "Hubei") %>%
  mutate(cifra_dif = c(NA, diff(value))) %>%
  filter(cifra_dif>0) %>%
  mutate(cifra_dif = c(NA, diff(value)),fechas_dif = c(NA, diff(Fechas))) %>%
  right_join(dat_graf, by = "variable")

#Y despues juntamos tambien el de fallecimientos con un left_join
recu_graf <- recu_graf %>% left_join(fall_graf, by = "variable")

#Dado que los reportes de casos y fallecimientos llegaban desacompasados, conviene hacer unos arreglos antes de graficar
recu_graf[28,"cifra_dif.x"] <- recu_graf[27,"cifra_dif.x"] + recu_graf[28,"cifra_dif.x"]
recu_graf[27,"cifra_dif.x"] <- NA
recu_graf[27,"fechas_dif.x"] <- NA
recu_graf[28,"fechas_dif.x"] <- 2

recu_graf[28,"cifra_dif.y"] <- recu_graf[27,"cifra_dif.y"] + recu_graf[28,"cifra_dif.y"]
recu_graf[27,"cifra_dif.y"] <- NA
recu_graf[27,"fechas_dif.y"] <- NA
recu_graf[28,"fechas_dif.y"] <- 2

recu_graf[19,"cifra_dif.x"] <- recu_graf[19,"cifra_dif.x"] + 47
recu_graf[19,"fechas_dif.x"] <- 2
recu_graf[6,"cifra_dif.x"] <- recu_graf[6,"cifra_dif.x"] + 8
recu_graf[6,"fechas_dif.x"] <- 2

#El codigo de los graficos es analogo practicamente a los anteriores tambien
#Solo hay que tener en cuenta lo que significa cada variable
#     - value = nº acumulado de fallecidos hasta un dia concreto
#     - value.y = nº acumulado de casos confirmados hasta un dia concreto
#     - value.x = nº acumulado de recuperaciones hasta un dia concreto
recu_graf %>% ggplot(aes(x = Fechas, y = value*100/value.y)) + 
  geom_point(size = 3) + geom_line(size = 1.05) +
  stat_smooth(se = F, size = 1.1) +
  scale_x_datetime(date_breaks = "2 days", date_labels = "%d/%m") +
  labs(title = "Evolución de la tasa de letalidad bruta (CFR) del COVID-19\n(nº fallecimientos/nº casos) en la región de Hubei (China) a 03-03-2020",
       subtitle = "Fuente: Centro para Ciencia de Sistemas e Ingeniería (CSSE) de la Universidad Johns Hopkins\n#DatosDeMiercoles de @R4DS_es, semana 12-02-2020",
       x = "Fecha", y = "Tasa de letalidad bruta (%)",
       caption = "@Picanumeros") +
  theme_bw(base_size = 16) +
  theme(text = element_text(family = "Liberation Sans"),
        plot.background = element_rect(fill = "#BFD5E3"))

recu_graf %>% ggplot(aes(x = Fechas, y = value*100/(value + value.x))) + 
  geom_point(size = 3) + geom_line(size = 1.05) +
  stat_smooth(se = F, size = 1.1) +
  scale_x_datetime(date_breaks = "2 days", date_labels = "%d/%m") +
  labs(title = "Evolución de la tasa de letalidad bruta (CFR) del COVID-19\n[nº fallecimientos/(nº fallecimientos + nº recuperaciones)] en la región de Hubei (China) a 03-03-2020",
       subtitle = "Fuente: Centro para Ciencia de Sistemas e Ingeniería (CSSE) de la Universidad Johns Hopkins\n#DatosDeMiercoles de @R4DS_es, semana 12-02-2020",
       x = "Fecha", y = "Tasa de letalidad bruta (%)",
       caption = "@Picanumeros") +
  theme_bw(base_size = 16) +
  theme(text = element_text(family = "Liberation Sans"),
        plot.background = element_rect(fill = "#BFD5E3"))
