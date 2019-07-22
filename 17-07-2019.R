library(dplyr)
library(ggplot2)
library(ggthemes)

dat <- readr::read_csv("https://raw.githubusercontent.com/cienciadedatos/datos-de-miercoles/master/datos/2019/2019-07-17/resultados_cmff.csv")

partidos <- dat %>% group_by(anio, id_partido_anio) %>% summarise(goles_partido = sum(goles))

estads <- partidos %>% group_by(anio) %>% 
  summarise(media_goles = mean(goles_partido),
            mediana_goles = median(goles_partido),
            desv_goles = sd(goles_partido)) %>%
  mutate(Etapa = ifelse(anio < 2010, "2007 o antes", "Después de 2007"))

goles_x_etapa <- round(aggregate(media_goles ~ Etapa, estads, FUN=mean)[,2],2)

ggplot(estads, aes(x=anio, y=media_goles)) +                  #Datos: evol_goles, con el año en el eje X y los promedios en el Y
  geom_point(size=3) + geom_line() +                              #Comandos para poner los puntos y la línea
  #geom_text(size = 2.75, nudge_y = 0.15 ) +
  stat_smooth(method="lm", se=F, formula= y ~ 1, aes(col=Etapa))+ #Añadimos una línea que indique la media de goles en las dos etapas
  theme_fivethirtyeight(base_size = 18) +                         #Usamos el tema de FiveThirtyEight para el gráfico
  scale_x_continuous(breaks=seq(1991,2019,by=4))+                #Indicamos que los cortes del eje X han de ir de 4 en 4 años
  scale_colour_manual(values = fivethirtyeight_pal()(2)) +        #Señalamos los colores que queremos para indicar las medias por etapa
  annotate("text", x=c(1997,2015), y=c(3.575,2.7), size = 6,     #Con annotate() introduciremos el texto para indicar el promedio numérico para cada etapa
           label= paste("Media = ",goles_x_etapa," goles",sep=""), col=fivethirtyeight_pal()(2)) +
  labs(title = "Evolución del promedio de goles por partido\nen Copas del Mundo de fútbol, 1991-2019",
       subtitle = "Fuente: data.world, recopilado en el repositorio de Github\nde @R4DS_es (#DatosDeMiercoles semana 17-07-2019)",
       caption = "@Picanumeros")                                  #Por último, títulos y subtítulos del gráfico
