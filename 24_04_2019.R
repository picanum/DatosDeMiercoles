library(ggplot2)    #Para hacer los gráficos
library(ggthemes)   #Para disponer de temas (themes) con los que embellecer los gráficos
library(gganimate)  #Para animar los gráficos
library(dplyr)      #Para tratamiento de datos
library(viridis)    #Para los colores del joyplot

#Cargamos los datos
gapminder <- readr::read_csv("https://raw.githubusercontent.com/cienciadedatos/datos-de-miercoles/master/datos/2019/2019-04-24/gapminder_es.csv")

#Código para la animación ACTUALIZADO (30-04-2019)

g <- ggplot(gapminder, aes(x = esperanza_de_vida, y=pib_per_capita, 
                        col = poblacion, size = poblacion)) + geom_point(alpha = .5) +  #Datos: X = esp. vida, Y = PIB, tamaño y color = log(población)
  theme_fivethirtyeight(base_size=24) + theme(axis.title = element_text()) +            #Usamos el tema de FiveThirtyEight con títulos para los ejes
  scale_y_log10() +   #Usamos una escala logarítmica en base 10 para el eje Y (el de PIB per cápita) 
  scale_color_continuous(name = "Población")+
  scale_size_continuous(name = "Población")+   #Personalizamos un poco las leyendas de tamaño y color de los puntos
  facet_wrap(~continente) +       #Haremos un gráfico animado para cada uno de los continentes metiéndole como argumento a facet_wrap la variable continente
  labs(title = "Esperanza de vida y PIB per cápita en el año {round(frame_time)}",    #El año visible en el título irá cambiando a medida que la animación avance a lo largo de la variable anio
       subtitle = "Fuente: paquete 'gapminder' de Jenny Bryan (2017) traducido al español y accesible en el Github de @R4DS_es (#DatosDeMiercoles semana 24-04-2019)",
       caption = "@Picanumeros",
       x = "Esperanza de vida (años)", y = "PIB per cápita") +    #Metemos el resto de nombres
  transition_time(anio) +   #Le decimos a gganimate que recorra cada año de la variable anio y que haga un gráfico como el descrito anteriormente para cada uno de ellos
  ease_aes("linear")        #Le decimos también que haga una transición lineal entre un año y otro

animate(g, width= 1500, height=1050)  #Animamos el gif con una resolución medianamente en condiciones...
anim_save("gapminder.gif")            #... y lo guardamos en el PC

#Código para los joyplots

#En estas dos líneas haremos una variable de tipo factor para que los años evolucionen de arriba a abajo en los joyplots
anios <- levels(as.factor(gapminder$anio))
gapminder <- gapminder %>% mutate(anio_f = factor(anio, levels = rev(anios)))

ggplot(gapminder, aes(x = esperanza_de_vida, y = anio_f, fill = ..x..)) +   #Datos: x, relleno = esperanza de vida, y = año
  geom_density_ridges_gradient(scale = 3, rel_min_height = 0.01) +          #Metemos el joyplot con una escala para la densidad
                                                                            #que no entorpezca la visión y una altura mínima que
                                                                            #permita visualizar outliers
  scale_fill_viridis(name = "Esperanza de Vida", option = "C") +            #Con el paquete viridis tenemos acceso a una gama de
                                                                            #colores con los que embellecer los joyplots
  labs(x = "Esperanza de vida en años",
       y = "Año",
       caption = "@Picanumeros",
       title = "Evolución de la densidad de la esperanza de vida en el mundo",
       subtitle = "Fuente: paquete 'gapminder' de Jenny Bryan (2017) traducido al español\ny accesible en el Github de @R4DS_es (#DatosDeMiercoles semana 24-04-2019)")+
  theme_economist_white()     #Para no repetirme mucho, esta vez uso el tema de The Economist en blanco

#En vista de que hay una mixtura de distribuciones, vamos a separar los joyplots por continentes para verla mejor

gapminder %>% filter(continente != "Oceania") %>%     #Hay que quitar Oceanía de en medio ya que al tener solo dos países no podemos
                                                      #obtener densidades :(
ggplot(aes(x = esperanza_de_vida, y = anio_f, fill = ..x..)) + 
  facet_wrap(~continente)+        #El código es igual que el de antes EXCEPTO POR ESTA LÍNEA donde metemos el facet_wrap
  geom_density_ridges_gradient(scale = 3, rel_min_height = 0.01) +
  scale_fill_viridis(name = "Esperanza de Vida", option = "C") +
  labs(x = "Esperanza de vida en años",
       y = "Año",
       caption = "@Picanumeros",
       title = "Evolución de la densidad de la esperanza de vida en el mundo",
       subtitle = "Fuente: paquete 'gapminder' de Jenny Bryan (2017) traducido al español\ny accesible en el Github de @R4DS_es (#DatosDeMiercoles semana 24-04-2019)")+
  theme_economist_white()
