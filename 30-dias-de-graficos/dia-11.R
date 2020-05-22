library(tidyverse)  #Para el procesamiento de datos
library(extrafont)  #Para utilizar las fuentes de Windows en los ggplot

#Este es el script para simular las partidas. Tarda unos minutejos, asi que paciencia. Supongo que utilizando apply's se podra acelerar.
datos <- data.frame(v = NA, prob_vic = NA, prop = NA)
for(v in 1:10){     #Sea v = 1, 2, ..., 10 el numero de monedas que se ganan (por moneda apostada) cada vez que se gana un juego
  props <- c()
  esperanza <- c()
  for(p in seq(0.01, 0.99, by = 0.01)){       #Sea p \in (0, 1) la probabilidad de ganar el juego y 1-p la probabilidad de perderlo.
    aguante <- c()                            #En este vector anotaremos el resultado de cada simulacion (arruinado o no al turno nº100)
    for(i in 1:1000){                         #Simulamos 1000 partidas en las que jugaremos una y otra vez.
      x <- 1                                  #x = numero de monedas con las que contamos en nuestro presupuesto. De entrada, x = 1
      k <- 1                                  #Valor que servira para ir iterando dentro del while
      sit <- c()                              #En este vector iremos "apuntando" cuanto presupuesto nos queda en cada turno
      while(x > 0 & k < 101){                 #Este seria el bucle que va jugando las partidas. Se detiene al llegar a 100 o cuando el jugador se arruina
        x <- x + sample(c(v,-1),1, prob = c(p, 1-p))
        sit[k] <- x
        k <- k + 1
      }
      aguante[i] <- ifelse(length(sit)==100, 1, 0)  #Apuntamos el resultado de la partida en el vector (1 si no esta arruinado, 0 en otro caso)
    }
    esperanza[as.character(p)] <- 1 - (1-p)         #Anotamos tambien la esperanza del juego en la partida en cuestion
    props[as.character(p)] <- sum(aguante==1)/length(aguante)   #Anotamos el numero de partidas en las que el jugador NO se ha arruinado
  }
  datos <- rbind(datos, 
                 data.frame(v = rep(v, length(seq(0.01, 0.99, by = 0.01))),
                            prob_vic = seq(0.01, 0.99, by = 0.01),
                            prop = props ))
}

#Quitamos la primera fila, donde es todo NULL
datos <- datos[-1,]

#El ggplot no tiene nada de particular. Para hacerlo en forma de heatmap se añade el argumento geom_tile()
ggplot(datos, aes(x = v, y = prob_vic, fill = (1-prop)*100)) + geom_tile() +
  scale_fill_viridis_c(name = "% de partidas\narruinadas\nal 100º turno",
                       option = "magma", begin = 1, end = 0) + 
  theme_minimal(base_size = 16) + 
  theme(text = element_text(family = "Source Sans Pro")) +
  labs(x = "Ganancia neta por moneda jugada en caso de victoria/éxito en el juego",
       y = "Probabilidad de victoria/éxito", title = "The Gambler's Ruin: proporción de partidas a un juego con resultado dicotómico (éxito o fracaso) arruinadas antes\ndel turno nº 100, con un presupuesto inicial de 1 moneda y apostando 1 moneda cada vez que juega.",
       subtitle = "Desafío #30díasdegráficos con R de @R4DS_es, día 11.", 
       caption = "1000 simulaciones para cada par (probabilidad, ganancia) | @Picanumeros") + 
  scale_x_continuous(breaks = 1:10, 
                     labels = c("1 mda. por\nmda. jugada",paste(2:10, " mdas. por\nmda. jugada", sep = "")), expand = c(0,0)) + 
  scale_y_continuous(breaks=seq(0,1,by=0.1), expand = c(0.005,0.005))

ggsave("dia11.png", dpi = 300,
       width = 13.6, height = 8)
