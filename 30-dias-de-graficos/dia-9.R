library(tidyverse)  #Para procesamiento de datos
library(extrafont)  #Para usar fuentes de Windows en los ggplots

#Vamos a emplear datos de la encuesta del uso de las TIC en los hogares que lleva a cabo el INE cada año
#Al igual que en anteriores ocasiones, extraemos los datos de tablas del INE mediante un hipervinculo
#Puedes acceder a el en el boton "Descarga" de arriba a la derecha cuando estas preparando una consulta, y elegir el formato que mas te guste
#Es una opcion que requiere mas preprocesamiento que hacerlo a traves de la API pero permite descargarlo directamente
dat <- read.csv("https://ine.es/jaxi/files/_px/es/csv_bdsc/t25/p450/base_2011/a2019/l0/04003.csv_bdsc?nocab=1", 
                sep = ";", dec=",", na.strings = "..", encoding = "UTF-8")  #Ojo con el encoding!

#Cambiamos el nombre de las columnas por otros mas manejables
colnames(dat) <- c("pob","dem","freq","total")


dat <- dat %>% 
  filter(pob == "Total de personas (16 a 74 años)") %>%     #Nos interesa el total de la poblacion, asi lo indicamos en este filter
  filter(substr(dem,1,4) == "Edad") %>%                     #Nos quedamos con los estratos demograficos de edad
  filter(freq != "Total") %>%                               #Quitamos los totales (solo sirven para mezclar datos absolutos con relativos)
  mutate(dem = str_sub(dem, 9, -1),                         #Acortamos las etiquetas de los estratos demograficos...
         total = as.numeric(str_replace(total, ",", ".")))  #... y pasamos el formato de la columna con las cifras a numerico

#En el dataset no estan los porcentajes de quienes no han usado nunca internet. Tenemos que calcularlos nosotros mismos
#Lo hacemos restando a 100 la suma en cada uno de los estratos de edad
dat2 <- data.frame(aggregate(total ~ dem, dat, FUN = function(x) 100 - sum(x, na.rm=T)),
           freq = rep("No han utilizado Internet nunca", 6))

#Juntamos ambos data.frames
dat <- rbind(dat[,c("dem", "total", "freq")], dat2)

dat %>%
  mutate(freq = factor(freq,            #Cambiamos el orden y las etiquetas del factor "frecuencia de uso"
                       levels = c("Utilizan internet varias veces al día",
                                  "Utilizan internet diariamente (al menos 5 días por semana) pero no varias veces al día",
                                  "Utilizan internet todas las semanas pero no diariamente",
                                  "Han utilizado Internet hace más de 1 semana y menos de 3 meses",
                                  "Han utilizado internet hace más de 3 meses y menos de 1 año",
                                  "Han utilizado Internet hace más de 1 año",
                                  "No han utilizado Internet nunca")[7:1],
                       labels = c("Utilizan internet varias veces al día",
                                  "Utilizan internet diariamente (al menos 5 días por semana)\npero no varias veces al día",
                                  "Utilizan internet todas las semanas pero no diariamente",
                                  "Han utilizado internet hace más de 1 semana y menos de 3 meses",
                                  "Han utilizado internet hace más de 3 meses y menos de 1 año",
                                  "Han utilizado internet hace más de 1 año",
                                  "No han utilizado internet nunca")[7:1]
  )) %>%
ggplot(aes(x = dem, y = total, fill = freq, group = freq)) +  #Importante: la variable que se apila tiene que estar tanto en fill como en group
  geom_area(col = "black", alpha = .75) +                     #Con geom_area apilamos las frecuencias
  theme_minimal(base_size = 18) +
  theme(legend.position = "bottom",
        panel.grid = element_line(colour = "grey75", linetype = "dashed"),
        plot.background = element_rect("#F0F0F0ff"),
        text = element_text(family = "Source Sans Pro")) +
  labs(x = "Grupo de edad", 
       y = "Porcentaje (%) de gente de cada frecuencia",
       title = "Frecuencia de uso de internet por grupos de edad en España (2019)",
       subtitle = "Fuente: Encuesta sobre equipamiento y uso de tecnologías de información y comunicación en los hogares, INE.\nElaboración propia con datos extraídos del sitio web: www.ine.es. Desafío #30díasdegráficos con R de @R4DS_es, día 9.") +
  scale_y_continuous(breaks = seq(0, 100, by = 10)) +
  scale_x_discrete(expand = c(0.04,0.04)) +
  guides(fill=guide_legend(nrow=4,byrow=TRUE)) +              #Truquejo para poner la leyenda en varias lineas
  scale_fill_brewer("Frecuencia\nde uso",
                    palette = "Paired")

#Y ya estaria
ggsave("dia9.png", width = 13.6, height = 10, dpi = 300)

