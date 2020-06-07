library(tidyverse)
library(extrafont)
library(gganimate)

ps <- seq(0.05,0.9,by=0.025)    #Generamos el grid de valores para el parámetro p

#Realizamos las simulaciones de las binomiales con n = 30 y p = p y las Poisson con lambda = 30*p
datos<-data.frame(p=rep(ps,each=10000),
                  media_pois = rep(ps*30, each=10000))
dens <- c()
dens_pois <- c()

#Generamos (y almacenamos) 10000 resultados de una binomial con n = 30 y p = i, donde i recorre todos los valores de ps
for(i in as.numeric(levels(as.factor(datos$p)))) dens <- c(dens, rbinom(10000,30,i)) 
datos$dens <- dens

#Generamos (y almacenamos) 10000 resultados de una Poisson con lambda = i, donde i recorre todos los valores de 30*ps
for(i in as.numeric(levels(as.factor(datos$media_pois)))) dens_pois <- c(dens_pois, rpois(10000,i))
datos$dens_pois <- dens_pois

#Representaremos los diagramas de barras de la frecuencia con la que se ha dado cada valor en las realizaciones de los experimentos
animate(
  ggplot(datos, aes(x=dens)) + 
    geom_bar(alpha=.8, stat = "count", fill = "grey25") +
  geom_bar(aes(x = dens_pois), fill = "red", alpha=.4, stat = "count") +
  theme_minimal(base_size = 24) + 
  theme(text = element_text(family = "Source Sans Pro"),
    axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())+
  labs(title = 'Densidad de probabilidad de una binomial con n = 30 y p = {closest_state} (negro)\nDensidad de probabilidad de una Poisson con lambda = {as.numeric(closest_state)*30} (rojo)\nA igual número de ensayos (si son suficientes), cuanto menor sea\nla prob. de éxito, más se aproximan ambas distribuciones',
       subtitle = "Datos obtenidos a partir de 10.000 simulaciones para cada valor de p.\nDesafío #30díasdegráficos con R de @R4DS_es, día 27.",
       y = "Densidad de probabilidad",
       caption="@Picanumeros")+
  annotate("text", x = 25, y = 3200, size = 8, 
           label = "Negro = binomial", family = "Source Sans Pro") +
  annotate("text", x = 25, y = 3000, size = 8, 
             label = "Rojo = Poisson", col = "red", family = "Source Sans Pro") +
  scale_y_continuous(breaks = seq(0,5000, by = 400),
                     labels = seq(0,0.5, by = 0.04)) +
  #A partir de esta línea empiezan los comandos de gganimate
  transition_states(p
                    , transition_length = 3, state_length = 2
  ) + 
  ease_aes('sine-in-out'),
  #Estos dos últimos comandos son los de animate(). En Twitter, lo ideal es que los gifs sean de 1280x1080, por bruto que parezca.
  height = 1280, width = 1080)

anim_save("dia27.gif")
