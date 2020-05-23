library(tidyverse)  #Para el procesamiento de datos
library(extrafont)  #Para poder usar las fuentes de Windows en los ggplots

#Cargamos los datos de bases de cotizacion de la Encuesta de insercion laboral de egresados universitarios del INE,
dat <- read.csv("https://ine.es/jaxi/files/_px/es/csv_bdsc/t13/p100/2014/p03/l0/12010.csv_bdsc?nocab=1",
                sep=";", dec = ",", encoding = "UTF-8", na.strings = "..")
                
#Quitamos el punto para los miles y pasamos a numerico donde hace falta
dat[,5] <- as.numeric(as.character(str_replace(dat[,5],"\\.","")))

#Ponemos nombres de columnas mas manejables
colnames(dat) <- c("gen","tit","quint","anio","total")

#Cargamos los errores de muestreo de la encuesta
cvs <- read.csv("https://ine.es/jaxi/files/_px/es/csv_bdsc/t13/p100/2014/p05/l0/01002.csv_bdsc?nocab=1",
                sep=";", dec = ",", encoding = "UTF-8", na.strings = "..")

colnames(cvs) <- c("gen","tit","sitlab","total")

#Nos quedamos con aquellas titulaciones donde el Coeficiente de Variacion de la estimacion de % de egresad@s que trabaja
#es menor del 10% en ambos generos
cvs <- cvs %>% filter(gen != "Ambos sexos" & sitlab == "Trabajando") %>%
  filter(total < 10) %>% count(tit) %>% filter(n == 2)

dat %>% filter(gen != "Ambos sexos") %>%      #Comenzamos filtrando las clases que no nos hacen falta
  drop_na("total") %>%                        #Tambien quitamos las carreras para las que no haya datos
  filter(quint != "Total afiliados a SS por cuenta ajena") %>%
  filter(anio == 2014) %>%
  filter(tit %in% cvs$tit) %>%                #Filtramos segun CV tal como dijimos
  mutate(quint.num = as.numeric(as.character(factor(quint, labels = c(4, NA, 1, 5, 2, 3))))) %>%  #Pasamos quintiles a numerico
  filter(quint != "No consta" & tit != "Otros grados") %>%    #Quitamos cifras correspondientes a gente que no declara quintil
                                                              #y la titulacion "otros grados"
  mutate(tit = ifelse(tit=="Total", "TOTAL", as.character(tit))) %>%  #Ponemos "total" (global de todas las titulaciones) en mayusculas
  group_by(gen, tit) %>%                                      #Calculamos ahora el quintil promedio por titulacion y genero
  summarise(media = sum(total * quint.num)/sum(total)) %>%
  pivot_wider(id_cols = "tit", names_from = "gen", values_from = "media") %>%  #En las siguientes lineas, calcularemos las diferencias
                                                                               #(calculando el cociente de medias entre generos)
                                                                               #para luego usarlas para ordenar los lollipops
  mutate(dif = Hombres/Mujeres) %>%
  pivot_longer(cols = c("Hombres","Mujeres"), names_to = "gen", values_to = "media") %>%
  mutate(tit = reorder(tit, dif)) %>%       #Listo! Reordenamos las categorias segun diferencias
  ggplot(aes(x = media, y = tit, group = tit, col = gen)) +       #Importante: para que salga un lollipop hay que poner en "group" la 
                                                                  #variable categorica (en este caso la titulacion)
  geom_line(col = "black") + geom_point(size = 5, alpha = .9) +   #Asi, solo tendremos que añadir geom_line() y geom_point() y sale automatico
  theme_bw(base_size = 14) + 
  theme(legend.position = "bottom",
        text = element_text(family = "Verdana")) +
  scale_color_manual(values = c("#4D6D93FF","#DCCA2CFF"), name = "Género") +
  labs(x = "Quintil promedio en el que se situaban l@s egresad@s de la titulación\nen las bases de cotización de la Seguridad Social\n(1 = primer quintil, menor remuneración; 5 = quinto quintil, mayor remuneración)",
       y = "", caption = "Sólo incluidas titulaciones para las que el Coef. Variación en la estimación del % de ocupados era menor del 10%. | @Picanumeros",
       title = "Diferencias entre géneros del promedio del quintil de bases de cotización a la Seguridad Social\nen el que se sitúan l@s egresad@s de una titulación del curso 2009-10 en 2014 en España\n(entre quienes trabajaban por cuenta ajena ese año)",
       subtitle = "Fuente: Encuesta de inserción laboral de titulados universitarios 2014, INE. Elaboración propia con\ninformación extraída del sitio web: www.ine.es. Desafío #30díasdegráficos con R de @R4DS_es, día 12.") + 
  scale_x_continuous(breaks = seq(1, 5, by = 0.5)) +
  geom_hline(yintercept = 9.5, linetype = "dashed") + 
  annotate("text", x = 2.5, y = 10, label = "Los hombres están en quintiles más altos que las mujeres") +
  annotate("text", x = 2.5, y = 9, label = "Las mujeres están en quintiles más altos que los hombres")

#Y ya estaria. Guardamos en dimensiones bien grandes, ya que hay muchas titulaciones.
ggsave("dia12.png", width = 16, height = 18, dpi = 300)
