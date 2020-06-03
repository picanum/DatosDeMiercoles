library(tidyverse)
library(extrafont)
library(rvest)      #Los 3 paquetes de siempre: para preprocesamiento de datos, uso de fuentes de Windows en ggplot y lectura de paginas web

#Leemos las tablas de la web donde estan almacenadas las tablas de la encuesta postelectoral de 2019
url <- "http://www.cis.es/cis/export/sites/default/-Archivos/Marginales/3260_3279/3269/es3269mar.html"
web <- read_html(url)
tablas <- html_table(web)

#Las tablas que nos interesan son la 58 y la 60 (esto lo he sacado inspeccionando visualmente, totalmente "analogico")
t1 <- tablas[[58]][-nrow(tablas[[58]]),]
t2 <- tablas[[60]][-nrow(tablas[[60]]),]

#Variable nueva para especificar el nivel del sunburst donde se va a encajar cada categoria
t1$nivel <- 1
t2$nivel <- 2

#Unimos ambas tablas
bind_rows(t1, t2) %>% 
  filter(X1 %in% c("N.C.", "N.S.") == F) %>%                                #Quitamos de enmedio los no sabe/no contesta
                                                                            #(aqui aportan poco -son muy pocos- y afean el grafico)
  mutate(X2 = as.numeric(as.character(str_replace(X2, ",", "\\.")))) %>%    #Cambiamos comas por puntos y pasamos a numerica
                                                                            #la columna donde estan los porcentajes
  mutate(X2 = ifelse(nivel == 2, X2*0.547/0.999, X2/0.999)) %>%   #Recalculamos los porcentajes de la tabla tras quitar los NS/NC
                                                                  #y para reflejar los porcentajes sobre muestra total (no condicionada)
  mutate(X1 = reorder(X1, X2)) %>%
  mutate(nivel = as.factor(nivel)) %>%
  ggplot(aes(x = nivel, y = X2, fill = X1)) +
  geom_col(width = 1, color = "gray90", size = 0.25, position = position_stack()) +
  geom_text_repel(aes(label = paste0(X1,"\n(", round(X2,1),"%)")), 
            size = 5, position = position_stack(vjust = 0.55)) +
  coord_polar(theta = "y") +
  scale_fill_brewer("", palette = "Blues") +
  theme_void(base_size = 16) +
  theme(legend.position = "none",
        text = element_text(family = "Liberation Sans"),
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        plot.caption = element_text(hjust = 0.5, vjust = 5)) +
  annotate("text", x = 2.4, y = 86, 
           label = "¿Tuvo Ud. conocimiento de\nlos resultados de alguna de\nlas encuestas o\nsondeos preelectorales\nque se hicieron sobre las\nelecciones generales\ndel 10 de noviembre?", 
           size = 4,
           family = "Liberation Sans") +
  annotate("text", x = 2.5, y = 12.5, 
           label = "¿Y en qué medida los tuvo\nen cuenta a la hora de decidir qué\niba a hacer en las elecciones\ndel 10 de noviembre: mucho,\nbastante, poco o nada?", 
           size = 4,
           family = "Liberation Sans") +
  labs(title = "Porcentaje de votantes con conocimiento de los resultados de las encuestas electorales\ny grado en el que los tuvieron en cuenta a la hora de decidir su voto\nen las elecciones generales de España de Noviembre de 2019",
       subtitle = "Fuente: Barómetro de Diciembre 2019 (postelectoral elecciones generales 2019) del CIS, preguntas B18 y B18a.\nDesafío #30díasdegráficos con R de @R4DS_es, día 23.",
       caption = "@Picanumeros")

ggsave("dia23.png", dpi = 300)
