library(tidyverse)  #Con cargar el tidyverse ya nos bastara esta vez

#Los datos proceden de la encuesta de empleo del tiempo del INE. La tabla de la que tomaremos los datos para el grafico la podemos descargar
#directamente del enlace de abajo, en formato .csv
url <- "https://www.ine.es/jaxi/files/_px/es/csv_bdsc/t25/e447/a2009-2010/p01/l0/1r1.csv_bdsc?nocab=1"
dat <- readr::read_csv2(url)

#Cambiamos los nombres que venian por defecto por otros algo mas manejables.
colnames(dat) <- c("activ","sexo","variable","value")

#Este uso del tiempo tiene un nombre un pelin largo, asi que vamos a meterle un salto de linea a mitad.
dat$activ[which(dat$activ == "9 Trayectos y empleo del tiempo no especificado")] <-
  "9 Trayectos y empleo\ndel tiempo no especificado"

#En esta pipeline esta el procedimiento para obtener el donut. Mas abajo esta la pipeline para obtener el diagrama de barras
dat %>% 
  filter(substr(activ,1,2) %in% paste0(0:9," ")) %>%                              #Nos quedamos con el nivel mas agregado de actividades
  pivot_wider(names_from = variable, values_from = value) %>%                     #Le hacemos un pivot_wider para que sea manejable con ggplot
  mutate(tiempo = (`DMD (horas)` + `DMD (minutos)`/60) * `Personas (%)`/100) %>%  #Importante: el tiempo dedicado a cada actividad
                                                                                  #por cada español/a lo vamos a obtener con una media
                                                                                  #ponderada. Es decir, el numero de horas dedicado de media
                                                                                  #a la actividad multiplicado por la proporcion de personas
                                                                                  #que la realizan. Se puede verificar que todo suma 24 al final.
  select(activ, sexo, tiempo) %>%                                 #Nos quedamos con las 3 variables que nos interesan...
  filter(sexo == "Ambos sexos") %>%                               #...y con datos sin distinguir por sexo (aunque distinguiendo es mas interesante)
  mutate(activ2 = substr(activ,3,nchar(activ))) %>%               #Para las etiquetas creamos una nueva variable sin los codigos de actividad
  ggplot(aes(x = 2, y = tiempo, fill = activ, label = activ)) +   #Para "abrir" el agujero del donut, es necesario que x = 2
                                                                  #Todo lo demas se pone como en un diagrama de barras normal
  geom_bar(stat = "identity", color = "white") +
  geom_text(aes(y = 24 - (cumsum(tiempo) - 0.5*tiempo), label = paste0(activ2,"\n(",round(tiempo,2)," horas)")),
            size = 3, family = "Bahnschrift") +                   #Las cuentas para situar las etiquetas en su sitio te pueden llevar por el
                                                                  #camino de la amargura, pero en fin. Con esto funciona siempre que no hagas facetas
  coord_polar(theta = "y", start = 0) +                           #El paso clave: a coordenadas polares
  xlim(c(0.5, 2.5)) +                                             #A partir de aqui todos los comandos son para mejorar la estetica
  scale_fill_brewer(palette = "Paired") +
  theme_void(base_size = 14) + 
  labs(title = "Un día en España: número medio de horas diarias dedicadas a cada\nactividad por cada ciudadan@ de España",
       subtitle = "Fuente: Encuesta de empleo del tiempo, 2009-2010. Instituto Nacional de Estadística (INE).\nElaboración propia con datos extraídos del sitio web: www.ine.es.\nDesafío #30díasdegráficos con R de @R4DS_es, día 6.",
       caption = "@Picanumeros") +
  theme(legend.position = "none",
        text = element_text(family = "Bahnschrift"),
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5)) 
ggsave("dia6.png",
       width = 9, height = 8,
       dpi = 300)

#Aqui el pipeline para hacer ese mismo grafico pero con diagramas de barras, sin que te complique tanto la vida el visualizar los datos
#Los pasos son mas o menos los mismos
dat %>% filter(substr(activ,1,2) %in% paste0(0:9," ")) %>%
  #filter(variable != "Personas (%)") %>%
  pivot_wider(names_from = variable, values_from = value) %>%
  mutate(tiempo = (`DMD (horas)` + `DMD (minutos)`/60) * `Personas (%)`/100) %>%
  select(activ, sexo, tiempo) %>%
  filter(sexo == "Ambos sexos") %>%
  mutate(activ2 = substr(activ,3,nchar(activ)),
         activ = as.factor(activ)) %>%
  mutate(activ = factor(activ, levels = levels(activ)[order(tiempo, decreasing = T)])) %>% #Aqui reordenamos las barras para ir de mayor a menor
  ggplot(aes(x = activ, y = tiempo, fill = activ, label = activ)) + 
  geom_bar(stat = "identity", 
           width = 0.75) +
  geom_text(aes(y = tiempo, label = paste0(activ2,"\n(",round(tiempo,2)," horas)")),
            size = 3, family = "Bahnschrift", nudge_y = 0.5) +
  scale_fill_brewer(palette = "Paired") +
  theme_void(base_size = 14) + 
  labs(title = "Un día en España: número medio de horas diarias dedicadas a cada\nactividad por cada ciudadan@ de España",
       subtitle = "Fuente: Encuesta de empleo del tiempo, 2009-2010. Instituto Nacional de Estadística (INE).\nElaboración propia con datos extraídos del sitio web: www.ine.es.\nDesafío #30díasdegráficos con R de @R4DS_es, día 6.",
       caption = "@Picanumeros") +
  theme(legend.position = "none",
        text = element_text(family = "Bahnschrift"),
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        plot.background = element_rect("#F0F0F0ff")) 
ggsave("dia6_bar.png",
       width = 14, height = 8,
       dpi = 300)
