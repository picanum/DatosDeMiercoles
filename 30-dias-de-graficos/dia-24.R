library(tidyverse)    #Procesamiento
library(ggthemes)     #Embellecimiento
library(sf)           #Mapas
library(RColorBrewer) #Embellecimiento

#Cargamos el mapa desde gadm.org
dat <- readRDS(url("https://biogeo.ucdavis.edu/data/gadm3.6/Rsf/gadm36_ESP_2_sf.rds"))

#Al igual que en otras ocasiones, he tenido que hacer la exportacion de forma manual, copiando la tabla en un Excel, arreglandola y de ahi a R
#La tabla procede del siguiente enlace:
#https://www.juntadeandalucia.es/institutodeestadisticaycartografia/badea/operaciones/consulta/anual/20531?CodOper=b3_2034&codConsulta=20531

datos_and <- read.xlsx("Libro1.xlsx", sheetIndex = 1, encoding = "UTF-8")

#Para que nos hagamos una idea, el dataset es este:
datos_and
#prov Mediano    Brecha
# 1 Almería   10572 0.1696383
# 2   Cádiz   11578 0.5230217
# 3 Córdoba   10189 0.5581395
# 4 Granada   10875 0.4199139
# 5  Huelva    8014 1.0242233
# 6    Jaén    9053 0.4910578
# 7  Málaga   12788 0.3511632
# 8 Sevilla   11879 0.4501922

#Unimos mapa y dataset
dat_graf <- merge(datos_and, dat, by.x = "prov", by.y = "NAME_2")

#Y graficamos sin ninguna historia, quiza el detalle importante es meter dat_graf dentro de un st_as_sf()
#para asi poder usar geom_sf
ggplot(st_as_sf(dat_graf)) + 
  geom_sf(aes(fill = Mediano), col = "black") + 
  geom_sf_label(aes(label = round(Mediano,2)), col = "black", size = 5) + 
  theme_fivethirtyeight(base_size = 11) + theme(legend.position = "bottom") + 
  scale_fill_gradientn(name = "Euros", colors = brewer.pal(9, "Blues")[9:1],
                       breaks = c(10000, 12000)) + 
  theme(panel.grid = element_line()) + 
  labs(title = "Salario bruto anual mediano por provincias de Andalucía (2018)", 
       subtitle = paste0("Fuente: Instituto de Estadística y Cartografía de Andalucía.\nExplotación de la Muestra Continua de Vidas Laborales con Datos Fiscales de la Seguridad Social\nDesafío #30díasdegráficos en R de @R4DS_es, día 24."),
       caption = "@Picanumeros"
       )

ggsave("dia24_1.png", dpi = 300)
       
ggplot(st_as_sf(dat_graf)) + 
  geom_sf(aes(fill = Brecha*100), col = "black") + 
  geom_sf_label(aes(label = round(Brecha*100,2)), col = "black", size = 5) + 
  theme_fivethirtyeight(base_size = 11) + theme(legend.position = "bottom") + 
  scale_fill_gradientn(name = "Euros", colors = brewer.pal(9, "Blues"),
                       breaks = c(25, 50, 75, 100)) + 
  theme(panel.grid = element_line()) + 
  labs(title = "Brecha salarial (cociente hombres/mujeres en %)\nentre salarios brutos anuales medianos en Andalucía (2018)", 
       subtitle = paste0("Fuente: Instituto de Estadística y Cartografía de Andalucía.\nExplotación de la Muestra Continua de Vidas Laborales con Datos Fiscales de la Seguridad Social\nDesafío #30díasdegráficos en R de @R4DS_es, día 24."),
       caption = "@Picanumeros"
  )

ggsave("dia24_2.png", dpi = 300)
