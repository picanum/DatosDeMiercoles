install.packages("readr") #Para cargar los datos
library(ggplot2)          #Para el gráfico
library(ggthemes)         #Para acceder a temas gráficos que embellezcan la visualización

#Cargamos los datos
partidos_fifa_copa_mundial_procesado <- readr::read_delim("https://raw.githubusercontent.com/cienciadedatos/datos-de-miercoles/master/datos/2019/2019-04-10/partidos.txt",delim = "\t")

#Reparamos los datos de goles del Mundial de 1934 (en la fuente original sólo tiene goles hasta el descanso)
partidos_fifa_copa_mundial_procesado$equipo_1_final[which(partidos_fifa_copa_mundial_procesado$anio==1934)]<-
  c(3,3,5,3,4,3,7,2,3,2,1,2,1,1,3,3,2)
partidos_fifa_copa_mundial_procesado$equipo_2_final[which(partidos_fifa_copa_mundial_procesado$anio==1934)]<-
  c(2,2,2,1,2,2,1,1,2,1,1,1,0,0,1,2,1)

#Obtenemos la media de goles por partido para cada Copa del Mundo con aggregate()
evol_goles<-aggregate(equipo_1_final + equipo_2_final ~ anio, partidos_fifa_copa_mundial_procesado, FUN=mean)

#Añadimos una nueva variable que indique si el Mundial se celebró en 1958 o antes, o después de 1958
evol_goles$Etapa <- ifelse(evol_goles$anio <= 1958, "1958 o antes", "Después de 1958")

#Cambiamos el nombre a la variable que indica la media de goles por partido para que sea más manejable
colnames(evol_goles)[2]<-"media_goles"

#Obtenemos el promedio de goles por partido para las dos etapas descritas anteriormente (<= 1958 y >1958)
goles_x_etapa <- round(aggregate(media_goles ~ Etapa, evol_goles, FUN=mean)[,2],2)


ggplot(evol_goles, aes(x=anio, y=media_goles)) +                  #Datos: evol_goles, con el año en el eje X y los promedios en el Y
  geom_point(size=2) + geom_line() +                              #Comandos para poner los puntos y la línea
  #geom_text(size = 2.75, nudge_y = 0.15 ) +
  stat_smooth(method="lm", se=F, formula= y ~ 1, aes(col=Etapa))+ #Añadimos una línea que indique la media de goles en las dos etapas
  theme_fivethirtyeight(base_size = 14) +                         #Usamos el tema de FiveThirtyEight para el gráfico
  scale_x_continuous(breaks=seq(1930,2020,by=10))+                #Indicamos que los cortes del eje X han de ir de 10 en 10 años
  scale_colour_manual(values = fivethirtyeight_pal()(2)) +        #Señalamos los colores que queremos para indicar las medias por etapa
  annotate("text", x=c(1944,1990), y=c(3.8, 2.5),                #Con annotate() introduciremos el texto para indicar el promedio numérico para cada etapa
           label= paste("Media = ",goles_x_etapa," goles",sep=""), col=fivethirtyeight_pal()(2)) +
  labs(title = "Evolución del promedio de goles por partido\nen Copas del Mundo de fútbol, 1930-2018",
       subtitle = "Fuente: Open Public Domain Football Data, recopilado en el repositorio\nde Github de @R4DS_es (#DatosDeMiercoles semana 10-04-2019)",
       caption = "@Picanumeros")                                  #Por último, títulos y subtítulos del gráfico

