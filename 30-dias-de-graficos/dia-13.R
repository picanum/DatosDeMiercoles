library(tidyverse)
library(extrafont)  #Estos dos paquetes son casi como de la familia
library(eurostat)   #Para cargar datos con la API de Eurostat
library(ggrepel)    #Para que las etiquetas no se solapen
library(ggthemes)   #Para usar nuevos themes

#Sacamos primero las series de paro y elegimos porcentaje, edad, pais y genero
paro <- get_eurostat("lfst_r_lfu3rt", time_format = "num") %>%
  filter(unit == "PC" & age == "Y15-74" & geo == "ES" & sex == "T")
  
#Para prestaciones seleccionamos el subsidio de desempleo total (full unemployment benefits), y las unidades euros de 2010 por habitante
prestacion <- get_eurostat("spr_exp_fun", time_format = "num") %>%
  filter(spdep == "STTCPFULLUNEMP" & unit == "EUR_HAB_KP10" &
           geo == "ES")

paro %>% left_join(prestacion, by = "time") %>%                         #Juntamos ambos datasets
  drop_na() %>% arrange(time) %>%
  mutate(gob = ifelse(time >= 2004 & time <= 2011, "PSOE", "PP")) %>%   #Añadimos variable que denota que partido estuvo en el gobierno
  ggplot(aes(x = values.x,  #Tasa de paro
             y = values.y,  #Prestacion
             label = time,  #Añadimos a cada punto el año
             col = gob,     #Importante: para que al colorear por partido no nos separe la serie en dos, hay que poner group = 1
                            #Asi indicamos que todas las observaciones son del mismo grupo
             group = 1)) + 
  geom_path(size = 0.75) +  #Añadimos una linea con el recorrido a lo largo de los años con geom_path
  geom_point(size = 3) + geom_label_repel() + 
  theme_igray(base_size = 16) +
  scale_color_manual(values = c("dodgerblue","red2"),
                     name = "Partido en\nel gobierno") + 
  scale_x_continuous(breaks = seq(8, 26, by = 4)) +
  theme(text = element_text(family = "Source Sans Pro")) +
  labs(x = "Tasa de paro en ambos sexos (15-74 años, %)",
       y = "Subsidio por desempleo total\nen euros de 2010 por habitante",
       title = "Evolución de la tasa de paro y el subsidio por desempleo total\npor habitante en España entre 1999 y 2017 (Eurostat)",
       subtitle = "Desafío #30díasdegráficos con R de @R4DS_es, día 13.",
       caption = "Datasets [lfst_r_lfu3rt] para tasa de paro y [spr_exp_fun] (gasto 'Full unemployment benefits') para subsidio | @Picanumeros")

ggsave("dia13.png", dpi = 300)
