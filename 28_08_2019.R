library(ggplot2)
library(dplyr)
library(gganimate)
library(viridis)
library(ggmap)
library(osmdata)

estaciones <- readr::read_csv("https://raw.githubusercontent.com/cienciadedatos/datos-de-miercoles/master/datos/2019/2019-08-28/estaciones.csv")

bicicletas <- readr::read_csv("https://raw.githubusercontent.com/cienciadedatos/datos-de-miercoles/master/datos/2019/2019-08-28/bicicletas.csv")

#Sacamos una variable que indique la hora de medición
bicicletas$hora <- substr(bicicletas$tiempo,12,19)
  
#En la siguiente línea crearemos el dataset con el que haremos el gráfico. Los pasos son:
#- Unimos a partir de la llave id_estacion ambos datasets
#- Agrupamos por hora y estación
#- Obtenemos una media de todas las variables, para cada estación y en cada hora de todas en las que se hicieron mediciones
dat_graf <- bicicletas %>% left_join(estaciones, "id_estacion") %>% group_by(hora, id_estacion) %>% summarise_all(list(~mean), na.rm=T) 
  
#En la primera línea del ggplot usaremos get_map para obtener el mapa de Buenos Aires
#Después, añadimos las estaciones con geom_point y a partir de ahí todo funciona como en un gganimate normal
g <- ggmap(get_map(getbb("Buenos Aires, Argentina"),maptype = "toner-background")) +
  geom_point(data = dat_graf, aes(x=lon, y = lat, col=bicis_disponibles), size = 6, alpha = .6) + 
  scale_colour_viridis(name = "Núm. bicis\ndisponibles\nen la estación\n(promedio)",option = "plasma") +
  transition_states(hora) + 
  geom_text(data = dat_graf, aes(x = -58.375, y = -34.55, label = hora), size = 10) +
  labs(title = "Número medio de bicicletas disponibles en las estaciones\nde Buenos Aires a las {closest_state} horas", 
                                 subtitle = "Fuente: Ciudad de Buenos Aires, datos recopilados\nen el GitHub de @R4DS_es (#DatosDeMiercoles semana 28-08-2019)",
                                 caption = "@Picanumeros") + 
  theme_void(base_size = 16)

#OJO: el gif tiene más de 100 frames por lo que, en condiciones normales, gganimate nos lo cortaría a la mitad (más o menos)
#Para evitar esto, tenemos que sacar el gif con la función animate especificando el número de frames (2 por cada hora)
animate(g, nframes = 2 * length(unique(bicicletas$hora_num)), height = 700, width = 700)
