library(tidyverse)  #Para todo el procesamiento de datos
library(extrafont)  #Para cambiar la tipografia del grafico
library(eurostat)   #Para extraer directamente los datos de la agencia Eurostat
#devtools::install_github("rensa/ggflags")
library(ggflags)    #Para añadir las banderas de cada pais al grafico

#En Eurostat, las cifras de camas hospitalarias se encuentran en el dataset de referencia hlth_rs_bdsrg
#Se puede buscar usando el comando search_eurostat y poniendo "beds" en la query
#Como ya sabemos cual es, basta con importarlo con get_eurostat, y usando un formato numerico para la variable tiempo (lo indicamos en time_format)
camas <- get_eurostat("hlth_rs_bdsrg", time_format = "num")


camas %>% 
  filter(geo %in% c("ES","IT","PT","EL")) %>%   #Nos quedamos con España, Italia, Portugal y Grecia
  filter(unit == "P_HTHAB") %>%                 #Nos quedamos con las cifras de camas por cada 100.000 habitantes
  filter(facility == "HBEDT") %>%               #Nos quedamos con las cifras de camas totales (se pueden elegir camas de psiquiatria, etc.)
  mutate(pais = ifelse(geo=="EL","gr",tolower(geo))) %>%  #Estos dos comandos los usaremos para añadir las banderas
  mutate(pais = ifelse(time == 2017, pais, NA)) %>%       #Hay que cambiar el codigo de Grecia y dejarlo todo en minuscula
                                                          #Despues, para que la bandera solo se muestre al final, dejamos estos codigos en NA
                                                          #salvo el correspondiente al ultimo año disponible en los datos (2017)
  mutate(nombres = factor(pais,                             #Con este mutate hacemos una variable que nos permitira añadir el nombre de cada pais
                          levels = c("gr","es","it","pt"),  #al final de cada serie temporal
                          labels = c("Grecia", "España", "Italia", "Portugal"))) %>%
  ggplot(aes(x = time, y = values, col = geo,       #Graficamos normalmente
             label = nombres)) + 
  geom_line(size = 1.1, alpha = .75) + 
  geom_flag(mapping = aes(x = time+1, y = values,   #En geom_flag damos un nuevo mapping donde sumaremos a la variable tiempo 1 unidad
                          country = pais),          #Esto se hace para que la bandera aparezca en el punto correspondiente a 2018, esto es,
            size = 15) +                            #un año despues del ultimo año disponible
  geom_text(nudge_x = 2.5) +                        #Al texto hay que añadirle un nudge_x para moverlo en el eje X y que no se pise con la bandera
  scale_color_manual(values = c("dodgerblue2","red","green4","darkred")) +
  scale_x_continuous(breaks = seq(1993, 2017, by = 2)) +
  scale_y_continuous(breaks = seq(300, 800, by = 50)) +
  labs(x = "Año", y = "Camas por cada 100.000 habitantes",
       title = "Evolución del número de camas hospitalarias disponibles\npor cada 100.000 habitantes en España, Italia, Portugal y Grecia",
       subtitle = "Fuente: Eurostat (dataset [hlth_rs_bdsrg]).\nDesafío #30díasdegráficos con R de @R4DS_es, día 2",
       caption = "@Picanumeros") +
  theme_bw(base_size = 18) +
  theme(legend.position = "none",
        text = element_text(family = "Verdana"),
        plot.background = element_rect("#F0F0F0ff"))

ggsave("dia2.png", dpi = 300)     #Guardamos en el disco duro con ggsave, que nos permite ajustar la resolucion del grafico en .png
