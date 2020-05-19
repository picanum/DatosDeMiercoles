library(tidyverse)      #Para el procesamiento de datos
library(extrafont)      #Para usar fuentes de Windows en los graficos
library(rnaturalearth)  #Para poder descargar el mapamundi y usarlo en el ggplot

#En esta ocasion hay que descargar los datos manualmente haciendo una consulta en la web del IGN y descargando el correspondiente csv
#Se puede acceder a la query que he realizado (longitudes 10ºW-4ºE y latitudes 34ºN-44ºN) en el siguiente enlace:
#https://www.ign.es/web/ign/portal/sis-catalogo-terremotos/-/catalogo-terremotos/searchTerremoto?latMin=34&latMax=44&longMin=-10&longMax=4&startDate=01/01/2019&endDate=31/12/2019&selIntensidad=N&selMagnitud=N&intMin=&intMax=&magMin=&magMax=&selProf=N&profMin=&profMax=&fases=no&cond=#

#Descargamos los datos en R:
dat <- read.csv("csv_terr.csv", sep = ";")

#Guardamos el mapamundi cargado con ne_countries en un objeto sf
world <- ne_countries(scale = "medium", returnclass = "sf")

#El ggplot no tiene demasiada historia
ggplot(data = world) + geom_sf(col = "red") + 
  ylim(c(34, 44)) + xlim(c(-10,4)) + 
  stat_density_2d(data = dat, aes(x = Longitud, y = Latitud, fill = after_stat(level)),   #Con stat_density_2d hacemos los diagramas de contorno
                  geom = "polygon", alpha = .5) +                                         #Para rellenarlos segun densidad de terremotos,
                                                                                          #introduzco fill = after_stat(level)
                                                                                          #Posteriormente especifico que me de poligonos
  geom_point(data = dat, aes(x = Longitud, y = Latitud), alpha = .05) +
  scale_fill_viridis_c(name = "Densidad") +
  theme_minimal(base_size = 16) +
  labs(title = "Diagramas de contorno de la densidad de terremotos\nen la Península Ibérica durante el año 2019",
       subtitle = "Fuente: Catálogo de terremotos del Instituto Geográfico Nacional (IGN).\nDesafío #30díasdegráficos con R de @R4DS_es, día 8.",
       caption = "@Picanumeros") +
  theme(text = element_text(family = "Source Sans Pro"))
  
#Y con esto ya estaría
ggsave("dia8.png", dpi = 300, height = 12, width = 12)
