library(ggplot2)    #Para hacer los gráficos
library(ggthemes)   #Para disponer de temas (themes) con los que embellecer los gráficos
library(gganimate)  #Para animar los gráficos

#Cargamos los datos
gapminder <- readr::read_csv("https://raw.githubusercontent.com/cienciadedatos/datos-de-miercoles/master/datos/2019/2019-04-24/gapminder_es.csv")

#Y sin más dilación, nos lanzamos con el gráfico:

g <- ggplot(gapminder, aes(x = esperanza_de_vida, y=pib_per_capita, 
                        col = log(poblacion), size = log(poblacion))) + geom_point() +  #Datos: X = esp. vida, Y = PIB, tamaño y color = log(población)
  theme_fivethirtyeight(base_size=24) + theme(axis.title = element_text()) +            #Usamos el tema de FiveThirtyEight con títulos para los ejes
  scale_y_log10() +   #Usamos una escala logarítmica en base 10 para el eje Y (el de PIB per cápita) 
  scale_color_continuous(name = "log(Población)")+
  scale_size_continuous(name = "log(Población)")+   #Personalizamos un poco las leyendas de tamaño y color de los puntos
  facet_wrap(~continente) +       #Haremos un gráfico animado para cada uno de los continentes metiéndole como argumento a facet_wrap la variable continente
  labs(title = "Esperanza de vida y PIB per cápita en el año {round(frame_time)}",    #El año visible en el título irá cambiando a medida que la animación avance a lo largo de la variable anio
       subtitle = "Fuente: paquete 'gapminder' de Jenny Bryan (2017) traducido al español y accesible en el Github de @R4DS_es (#DatosDeMiercoles semana 24-04-2019)",
       caption = "@Picanumeros",
       x = "Esperanza de vida (años)", y = "PIB per cápita") +    #Metemos el resto de nombres
  transition_time(anio) +   #Le decimos a gganimate que recorra cada año de la variable anio y que haga un gráfico como el descrito anteriormente para cada uno de ellos
  ease_aes("linear")        #Le decimos también que haga una transición lineal entre un año y otro

animate(g, width= 1500, height=1050)  #Animamos el gif con una resolución medianamente en condiciones...
anim_save("gapminder.gif")            #... y lo guardamos en el PC
