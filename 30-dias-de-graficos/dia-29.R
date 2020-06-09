library(tidyverse)      #Preprocesamiento
library(ggthemes)       #Uso de temáticas embellecedoras para los ggplots
library(GGally)         #Contiene la función ggparcoord que utilizaremos para graficar las coordenadas paralelas

#Los datos están descargados de Kaggle, dejo aquí el enlace:
#https://www.kaggle.com/stefanoleone992/fifa-20-complete-player-dataset?select=players_20.csv

#He seleccionado solo jugadores de Madrid y Barça, pero puede interesar seleccionar p. ej. toda la Primera División
#Dejo aquí el vector entero para ahorrar trabajo a quien quiera hacerlo
clubes <- c("Real Madrid", "FC Barcelona"
            # , "Atlético Madrid", "Valencia CF",
            # "Athletic Club de Bilbao", "Villarreal CF", "Sevilla FC", "Real Betis",
            # "Real Sociedad", "Getafe CF", "Granada CF", "CA Osasuna",
            # "Levante UD", "Deportivo Alavés", "Real Valladolid CF", "SD Eibar",
            # "RC Celta", "RCD Mallorca", "CD Leganés", "RCD Espanyol"
            )

#Cargamos y seleccionamos solo los clubes anteriormente mencionados y los jugadores de nacionalidad española
#(hay bastante disparidad en las relaciones entre variables según el país)
dat <- read.csv("players_20.csv", encoding = "UTF-8")
dat <- dat[which(dat$club %in% clubes),]
dat <- dat[which(dat$nationality == "Spain"),]

#Vamos a tomar la primera posición que aparezca dentro de las posiciones en las que puede jugar un jugador.
#Lo hacemos con str_split, quedándonos con lo que haya antes de la primera coma.
dat$posiciones <- sapply(1:nrow(dat),
                         function(i) str_split(dat$player_positions, ", ")[[i]][1])

#A partir de ahí convertimos a las cuatro posiciones básicas: portero, defensa, centrocampista y atacante
#Lo hacemos "traduciendo" las posiciones del inglés
dat$posiciones[which(str_sub(dat$posiciones,-1,-1)=="K")] <- "Portero"
dat$posiciones[which(str_sub(dat$posiciones,-1,-1)=="M")] <- "Centrocampista"
dat$posiciones[which(str_sub(dat$posiciones,-1,-1)=="B")] <- "Defensa"
dat$posiciones[which(str_sub(dat$posiciones,-1,-1) %in% c("F","T","W"))] <- "Atacante"

#Creamos la variable que mida el IMC...
dat %>% mutate(bmi = weight_kg/(height_cm/100)^2) %>%
  select(age, overall, bmi, height_cm, posiciones) %>%
  #Y aplicamos ggparcoord, que crea un ggplot pero con coordenadas paralelas, con la transparencia deseada para las líneas,
  #distintos tipos de estandarización... está muy bien. Sólo tenemos que dar los índices de las variables que queremos que vayan 
  #en las columnas del gráfico y el índice de la variable de agrupamiento si la hubiera (será la que dictamine los colores de las líneas)
  ggparcoord(columns = 1:4, groupColumn = 5, alphaLines = 1,
             scale = "uniminmax") +
  geom_line(size = 1.05) +              #Para cualquier otro retoque en las líneas, usamos geom_line() como habitualmente
  scale_x_discrete(labels = c("Edad",
                              "Media del jugador",
                              "Índice de masa corporal",
                              "Altura")) +
  scale_y_continuous(breaks = seq(0, 1, by = 0.1)) +
  scale_color_manual("Posición", values = c("red2", "orange", "dodgerblue", "darkgreen")) +
  theme_fivethirtyeight(base_size = 18) +
  geom_vline(xintercept = 1:4, linetype = "dashed", size = 1.1) +   #Esto no lo sabía: con un solo geom_vline puedes hacer todas las
                                                                    #líneas verticales que quieras, sólo con dar el vector de intercepts
  labs(title = "Edad, puntuación media, IMC y altura de los jugadores\nde nacionalidad española jugando en el Real Madrid\ny FC Barcelona en el FIFA 20",
       subtitle = "Variables estandarizadas en el rango [0, 1] para su comparabilidad.\n(0 = presenta el menor valor de entre todos los jugadores, 1 = presenta el mayor valor)",
       caption = "Fuente: Kaggle. Desafío #30díasdegráficos con R de @R4DS_es, día 29 | @Picanumeros")

ggsave("dia29.png", width = 12, height = 10, dpi = 300)
