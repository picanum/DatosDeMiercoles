library(viridis)  #Para acceder a la paleta de colores
library(ggplot2)  #Para el gráfico
library(maps)     #Para cargar el mapamundi

#Cargamos los datos
capitulos_rladies <- readr::read_csv("https://raw.githubusercontent.com/cienciadedatos/datos-de-miercoles/master/datos/2019/2019-06-26/capitulos_rladies.csv")
eventos_rladies <- readr::read_csv("https://raw.githubusercontent.com/cienciadedatos/datos-de-miercoles/master/datos/2019/2019-06-26/eventos_rladies.csv")
world <- map_data("world")

#A graficar!
ggplot() +  #Inicializamos el ggplot
  geom_polygon(data = world, aes(x = long, y = lat, group = group), fill = "#C0C0C0") +   #Metemos el mapamundi con geom_polygon y el objeto que hemos cargado
  geom_point(data = capitulos_rladies,      #Metemos los puntos de cada comunidad con geom_point, de forma que el tamaño y el color dependa del número de miembros
             aes(x = longitud, y = latitud, size = miembros, col = miembros), alpha = .75) + 
             theme_void(base_size = 16) +   #Tema vacío
  scale_color_viridis_c(name = "Nº miembros") + #Ponemos título a ambas escalas (color y tamaño)
  scale_size_continuous(name = "Nº miembros") +
  geom_label_repel(data = capitulos_rladies,    #Etiquetamos aquellas comunidades que tengan más de 1000 miembros a 25 de junio
                                            aes(x = longitud, y = latitud, 
                                                label = ifelse(miembros > 1000, ciudad, NA)),
                                            alpha = 0.6,      #Hacemos que las etiquetas sean un poco transparentes para que no tapen ninguna comunidad
                                            label.padding=.2, #Fijamos el tamaño de las etiquetas
                                            seed = 1) +       #Fijamos una semilla para que el "anti-overlay" deje las etiquetas siempre en la misma posición
  labs(title = "Comunidades R-Ladies en el mundo y número de miembros",
       subtitle = "Fuente: paquete 'meetupr' (De Queiroz et al., 2018). Datos disponible en el Github de @R4DS_es (#DatosDeMiercoles semana 26-06-2019)",
       caption = "@Picanumeros")
