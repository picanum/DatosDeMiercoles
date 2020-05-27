library(tidyverse)
library(extrafont)    #Estos dos paquetes son casi de la familia
devtools::install_github("hrbrmstr/waffle")
library(waffle)       #Este es el paquete que usaremos para los waffles, hay que instalarlo directamente de GitHub
library(rvest)        #Este paquete lo usaremos para scrapear tablas de una pagina web
library(RColorBrewer) 
library(hrbrthemes)   #Y estos dos para embellecer los graficos

#Leemos la web donde se encuentran las tabulaciones del barometro en HTML, y posteriormente rescatamos las tablas
web <- read_html("http://cis.es/cis/export/sites/default/-Archivos/Marginales/3260_3279/3273/es3273mar.html")
tablas <- html_table(web)

#En los elementos 51, 52 y 54 de la lista estan las tablas que nos interesan, en concreto:
#51: En general, ¿cree Ud. que aún queda mucho, bastante, poco o ningún camino que recorrer para lograr la igualdad de derechos y oportunidades entre hombres y mujeres?
#52: ¿Cree Ud. que la violencia de género es actualmente un problema preocupante para la sociedad española?
#54: ¿Ud. cree que con la actual ley contra la violencia de género es suficiente para enfrentarse a este problema o habría que hacer más cosas?

#Seleccionamos las tablas y le quitamos la fila de los totales
tab1 <- tablas[[51]][-8,]
tab2 <- tablas[[52]][-5,]
tab3 <- tablas[[54]][-5,]

#Unimos las tres tablas
tab <- rbind(tab1,tab2,tab3)

#Le damos una columna para poder dividirlo en facetas
tab$facet <- rep(1:3, c(7,4,4))

#Pasamos la columna de los datos numericos a un formato legible como tal en R
tab[,2] <- as.numeric(as.character(str_replace(tab[,2],",","\\.")))

#Dado que la pregunta B22a depende del flujo de B22, la multiplicaremos por el total de gente que ha pasado por dicho flujo
tab[12:15,2] <- tab[12:15,2]*0.93

#Redondeamos los resultados para que podamos usar los waffles
tab[,2] <- round(tab[,2])

#Creamos un nuevo factor ahora para colorear los cuadros de los waffles y que tengan una leyenda comun
tab$ley <- paste0(rep(c("1) ","2) ","3) "),
                      c(7,4,4)), tab[,1])
tab$ley[c(6,10,14)] <- "N.S."
tab$ley <- factor(tab$ley,
                  levels = c("1) Queda mucho camino por recorrer",
                             "1) Queda bastante camino por recorrer",
                             "1) Queda regular camino por recorrer",
                             "1) Queda poco camino por recorrer",
                             "1) No queda ningún camino por recorrer",
                             "2) Cree que es un problema preocupante",
                             "2) Cree que no supone un problema",
                             "3) Cree que con la actual ley es suficiente",
                             "3) Cree que habría que hacer más cosas",
                             "N.S."))

#En este vector almacenaremos los titulos de las facetas (siempre hay que meterlo en un as_labeller)
titulos <- as_labeller(c(
  "1" = "El 81% de la ciudadanía española considera que aún\nqueda mucho o bastante camino por recorrer\npara lograr la igualdad.",
  "2" = "El 93% cree que la violencia de género\nes un problema preocupante...",
  "3" = "... y de ell@s, el 76% cree que habría que hacer más\npara enfrentarse al problema además de la\nactual ley contra la violencia de género."
))

#geom_waffle requiere el argumento 'values', lo demas ya es opcional aunque esencial para que sean legibles
ggplot(tab, aes(fill = ley, values = X2)) +
  geom_waffle(color = "white", size=1.125, nrows = 6) +
  facet_wrap(~ facet, strip.position = "bottom",    #He tenido que poner los titulos de las facetas abajo para que las letras se vieran bien
             labeller = titulos) + 
  scale_fill_brewer("", palette = "Paired") +
  theme_void(base_size = 16) +
  theme_enhance_waffle() +
  theme(legend.position = "bottom",
        text = element_text(family = "Bahnschrift"),
        strip.text = element_text(vjust = 1.05)) +  #Ademas de eso, he tenido que mover los titulos dentro de cada faceta
  labs(title = "Posiciones de la sociedad española ante la igualdad y la violencia de género",
       subtitle = "Fuente: Barómetro de Febrero 2020 del CIS (preguntas B22, B23 y B23a). Desafío #30díasdegráficos con R de @R4DS_es, día 16.",
       caption = "@Picanumeros") +
  guides(fill=guide_legend(nrow=5,byrow=F))

ggsave("dia16.png", dpi = 300)
