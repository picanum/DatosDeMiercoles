library(ggplot2)      #Paquete base para los gráficos
library(ggthemes)     #Para acceder a temas gráficos que embellezcan la visualización
library(ggrepel)      #Para añadir al gráfico etiquetas que no se solapen

#Cargamos los datos
tiempo_pantalla <- readr::read_csv("https://raw.githubusercontent.com/cienciadedatos/datos-de-miercoles/master/datos/2019/2019-04-17/tiempo_pantalla.csv")

#Obtenemos la media de episodios y de minutos de pantalla que aparecen los personajes
media_ep <- mean(tiempo_pantalla$episodios, na.rm=T)
media_mp <- mean(tiempo_pantalla$minutos_pantalla)

#Creamos el vector que recoja los nombres de los personajes que:
# - Tengan alta presencia en minutos de pantalla
# - Tengan más minutos en pantalla que la media y menos episodios que la media
nombres_graf <- tiempo_pantalla$nombre
nombres_graf[-which(
  (tiempo_pantalla$minutos_pantalla > 95) | 
    (tiempo_pantalla$minutos_pantalla >= media_mp &
       tiempo_pantalla$episodios <= media_ep)
)]<-NA

#VISUALIZACIÓN FINAL:

ggplot(tiempo_pantalla, aes(x=minutos_pantalla, y=episodios, label=nombres_graf))+ #Eje X minutos, eje Y episodios
  geom_point(size=2, col="blue")+   #Añadimos los puntos
  stat_smooth(se=F)+    #Añadimos la línea de regresión; usamos el modelo LOESS que ofrece ggplot por defecto 
                        #para mayor flexibilidad
  geom_hline(yintercept = media_ep, col="red")+   #Añadimos una línea que marque la media de episodios
  annotate("text", x=100, y = media_ep - 2.5,     #junto con su anotación
           label = paste("Media: ",round(media_ep,2)," episodios", sep=""), col="red")+
  geom_vline(xintercept = media_mp, col="red")+   #Ídem para la media de minutos de pantalla
  annotate("text", x=media_mp + 20, y = 45, 
           label = paste("Media:\n",round(media_mp,2)," minutos", sep=""), col="red")+
  theme_fivethirtyeight(base_size=14)+theme(axis.title = element_text()) + #Usamos el tema de FiveThirtyEight para el gráfico
  xlab("Minutos de pantalla") + ylab("Número de episodios")+    #Añadimos los títulos de los ejes
  labs(title="Relación entre minutos de pantalla y episodios en personajes de Juego de Tronos",
       subtitle="Fuente: data.world, recopilado en el repositorio de Github de @R4DS_es (#DatosDeMiercoles semana 17-04-2019)",
       caption="@Picanumeros")+
  geom_text_repel()   #Añadimos los nombres de los personajes que hemos elegido anteriormente
