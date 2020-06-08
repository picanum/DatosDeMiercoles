library(tidyverse)      #Preprocesamiento de datos
#install.packages("circlize")
library(circlize)       #Paquete necesario para hacer el gráfico de cuerdas
library(RColorBrewer)   #Para la paleta de colores

#Al igual que el día 20, cargamos los datos de la tabla del INE desde este enlace:
dat <- read.csv("https://www.ine.es/jaxiT3/files/t/es/csv_bdsc/24447.csv?nocab=1",
                sep = ";", encoding = "UTF-8")
colnames(dat) <- c("gen","destino","origen","periodo","total")

andalucia <- c("Almería", "Granada", "Jaén", "Málaga",
               "Córdoba", "Sevilla", "Cádiz", "Huelva")

#El preprocesamiento también es el mismo que el del día 20, sólo que ahora nos quedamos únicamente con las provincias andaluzas
#(se puede hacer para toda España pero la interpretación es más complicada)
grafo <- dat %>% filter(gen == "Ambos sexos" & 
                          destino != "Total Nacional" &
                          origen != "Total Nacional" &
                          periodo == "2019S1") %>%
  mutate(destino = str_sub(destino, 4, -1),
         origen = str_sub(origen, 4, -1),
         total = as.numeric(as.character(str_remove(total, "\\.")))) %>%
  select(destino, origen, total) %>%
  mutate(total = ifelse(is.na(total), 0, total)) %>%
  filter(origen %in% andalucia,
         destino %in% andalucia)

#Seleccionamos la paleta "Paired", la almacenamos en un vector, y a cada color le damos el nombre de la provincia que lo llevará
grid.col = brewer.pal(8, "Paired")
names(grid.col) = andalucia

#Con la función chordDiagram hacemos el gráfico. Nótese que no usa ggplot sino el formato base de R, por lo que hay que hilar un poco
#más fino para añadirle todos los detalles.
chordDiagram(grafo, grid.col = grid.col)
title(main = "Migraciones entre provincias andaluzas en el primer semestre de 2019")
text(x = 0, y = 1.035, "Desafío #30díasdegráficos con R de @R4DS_es, día 28.")
text(x = 0, y = -1.05, "Fuente: Estadística de migraciones, INE. Elaboración propia con datos extraídos del sitio web www.ine.es. | @Picanumeros")

#Lo guardé en PDF a través de RStudio y después lo importé a .png con Inkscape
