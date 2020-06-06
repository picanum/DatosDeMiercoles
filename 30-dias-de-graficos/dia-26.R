library(tidyverse)  #Preprocesamiento de datos
library(extrafont)  #Uso de fuentes de Windows en ggplot
library(eurostat)   #Acceder a las tablas de Eurostat
#devtools::install_github('haleyjeppson/ggmosaic')
library(ggmosaic)   #Paquete que usaremos para hacer el mosaico

#Descargamos el dataset de Eurostat
dat <- get_eurostat("ilc_li02", time_format = "num")

#Lo primero que haremos sera filtrar los datos que nos interesan: de los paises mas poblados (si no contamos a Turquia) en 2018,
#en miles de personas, para ambos generos y para las franjas de edad mas concretas a partir de los 6 años.
#Tomaremos tambien el umbral como el 60% de la mediana de ingresos.
p <- dat %>% filter(time == "2018" & 
                 geo %in% c("ES", "IT", "FR", "UK", "DE") &
                 unit == "THS_PER" &
                 age %in% c("Y6-11","Y12-17", "Y18-24", "Y25-49", "Y50-64", "Y65-74", "Y_GE75") &
                 indic_il == "LI_R_M60" &
                 sex == "T") %>%
  mutate(geo = factor(geo, levels = c("ES", "IT", "FR", "UK", "DE"),      #Renivelamos las variables geo y age para traducirlas al castellano
                      labels = c("España", "Italia", "Francia",
                                 "Reino Unido", "Alemania")),
         age = factor(age, 
                      levels = c("Y6-11","Y12-17", "Y18-24", "Y25-49",
                                 "Y50-64", "Y65-74", "Y_GE75"),
                      labels = c("6-11 años", "12-17 años", "18-24 años",
                                 "25-49 años", "50-64 años", "65-74 años", ">74 años"))) %>%
  ggplot() + geom_mosaic(aes(weight = values, x = product(geo, age),      #Para hacer el mosaico, necesitamos tres elementos en geom_mosaic:
                             fill = geo), na.rm = T) +                    #1) weight (para el tamaño de las cajas, aqui pondremos los miles de personas)
                                                                          #2) x (para especificar que variables iran en los ejes, hay que ponerlo dentro
                                                                          #de product() ya que se trata de una interseccion)
                                                                          #3) fill (para especificar el color de las cajas)
  labs(x = "Franja de edad", y = "País",
       title = "Personas en riesgo de pobreza (ingresos por debajo del 60% de la mediana)\nen 2018 por país y franja de edad (unidad: miles de personas)",
       subtitle = "Fuente: Eurostat [dataset ilc_li02]. Desafío #30díasdegráficos con R de @R4DS_es, día 26.",
       caption = "@Picanumeros") +
  scale_fill_manual(values = c("#D55E00",
                                "#009E73",
                                "#0072B2",
                                "#56B4E9",
                                "#F0E442")) +
  theme_minimal(base_size = 18) + 
  theme(text = element_text(family = "Liberation Sans"),
        legend.position = "none")

#Esta ultima parte copiada de https://stackoverflow.com/questions/50227916/adding-counts-to-ggmosaic-can-this-be-done-simpler
#para poder añadir texto dentro del grafico de manera sencilla
p + geom_text(data = ggplot_build(p)$data %>% as.data.frame() %>% filter(.wt > 0), 
             aes(x = (xmin + xmax)/2, 
                 y = (ymin + ymax)/2, 
                 label = .wt), family = "Liberation Sans")

ggsave("dia26.png", dpi = 300, width = 14, height = 8)
