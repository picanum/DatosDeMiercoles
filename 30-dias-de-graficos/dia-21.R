library(tidyverse)
library(extrafont)  #Estos dos paquetes son casi de la familia
library(rvest)      #El que usamos siempre para scrapear la web

#Vamos a obtener la intención directa de voto de las encuestas que estan recogidas en la Wikipedia en ingles
#Hacemos el procedimiento de siempre: leemos con read_html y rescatamos las tablas con html_table, con un fill = T si hiciera falta
url <-  "https://en.wikipedia.org/wiki/Opinion_polling_for_the_April_2019_Spanish_general_election"
web <- read_html(url)
tablas <- html_table(web, fill = T)

#La tabla que nos interesa es el tercer elemento de la lista. Hay que limpiarla un poco antes (quitar filas y columnas irrelevantes)
votodir <- tablas[[3]]
votodir <- votodir[-c(1:2,4,121,123),c(1:7,14)]
colnames(votodir) <- c("pollster","fecha","muestra","PP","PSOE","UP","Cs","VOX")

#La fecha hay que arreglarla un poco mas: primero nos quedamos con la fecha de cierre del trabajo de campo
#Despues sustituimos los nombres de los meses en ingles por su numero correspondiente para poder pasarlo a formato fecha
votodir$fecha <- sapply(1:nrow(votodir), 
                        function(i) str_split(votodir$fecha, "–")[[i]][length(str_split(votodir$fecha, "–")[[i]])])
votodir$fecha <- str_replace_all(votodir$fecha, "Jan", "01")
votodir$fecha <- str_replace_all(votodir$fecha, "Feb", "02")
votodir$fecha <- str_replace_all(votodir$fecha, "Mar", "03")
votodir$fecha <- str_replace_all(votodir$fecha, "Apr", "04")
votodir$fecha <- str_replace_all(votodir$fecha, "May", "05")
votodir$fecha <- str_replace_all(votodir$fecha, "Jun", "06")
votodir$fecha <- str_replace_all(votodir$fecha, "Jul", "07")
votodir$fecha <- str_replace_all(votodir$fecha, "Aug", "08")
votodir$fecha <- str_replace_all(votodir$fecha, "Sep", "09")
votodir$fecha <- str_replace_all(votodir$fecha, "Oct", "10")
votodir$fecha <- str_replace_all(votodir$fecha, "Nov", "11")
votodir$fecha <- str_replace_all(votodir$fecha, "Dec", "12")
votodir$fecha <- as.Date(tolower(votodir$fecha), format = "%e %m %Y")

votodir %>% mutate(Cs = as.numeric(as.character(Cs)),         #Pasamos a numericas las variables que deben serlo
                   VOX = as.numeric(as.character(VOX)),
                   muestra = as.numeric(as.character(str_replace_all(muestra, ",", "")))) %>%
  mutate(muestra = replace_na(muestra, 1)) %>%                #Las elecciones generales que se recogen en la tabla no tienen muestra
                                                              #Le asignamos un peso unitario (no tiene relevancia) para incluirlas en el grafico tambien
  pivot_longer(colnames(votodir)[-c(1:3)], names_to = "part", values_to = "voto") %>%   #Pasamos la tabla a formato largo
  group_by(fecha, part) %>% summarise(voto = weighted.mean(voto, muestra)) %>%    #Para un dia concreto, obtenemos la IDV calculando
                                                                                  #la media ponderada (por su n) de las encuestas surgidas ese dia
  ggplot(aes(x = fecha, y = voto, col = part)) + geom_point(alpha = 0.25) + 
  stat_smooth(method = "loess", span = 0.2, se = F, size = 1.15) +
  theme_bw(base_size = 16) +
  scale_color_manual(name = "Partido", values = c("orange", "dodgerblue", "red2", "purple", "green")) +
  scale_x_date(breaks = "2 months", date_labels = "%e %b\n%Y") +
  scale_y_continuous(breaks = seq(0, 30, by = 4)) +
  #A partir de aqui estarian las anotaciones
  geom_vline(xintercept = as.Date("2016-10-01")) +
  labs(x = "Fecha", y = "% de intención directa de voto al partido",
       title = "Evolución de la intención directa de voto observada en las encuestas a los principales\npartidos nacionales entre las elecciones generales de 2016 y las de abril de 2019",
       subtitle = paste0("Fuente: ", url, "\nDesafío #30díasdegráficos con R de @R4DS_es, día 21."),
       caption = "Curvas obtenidas con modelos LOESS con ventana 0.2 y polinomios de grado 2 | @Picanumeros") +
  annotate("text", x = as.Date("2016-10-06"), y = 5, 
           label = "Dimisión de Pedro Sánchez\ncomo secretario general\ndel PSOE", hjust = 0, family = "Source Sans Pro") +
  geom_vline(xintercept = as.Date("2017-10-01")) + 
  annotate("text", x = as.Date("2017-09-26"), y = 25, 
           label = "Referéndum del 1-O", hjust = 1, family = "Source Sans Pro") + 
  geom_vline(xintercept = as.Date("2018-06-01")) + 
  annotate("text", x = as.Date("2018-05-26"), y = 25, 
           label = "Moción de censura\ncontra Mariano Rajoy", hjust = 1, family = "Source Sans Pro") +
  geom_vline(xintercept = as.Date("2018-12-02")) + 
  annotate("text", x = as.Date("2018-11-27"), y = 5, 
           label = "Elecciones\nandaluzas", hjust = 1, family = "Source Sans Pro") +
  geom_vline(xintercept = as.Date("2019-02-15")) + 
  annotate("text", x = as.Date("2019-02-21"), y = 25, 
           label = "Convocatoria\nde elecciones\ngenerales", hjust = 0, family = "Source Sans Pro") +
  theme(text = element_text(family = "Source Sans Pro"))

ggsave("dia21.png", dpi = 300)
