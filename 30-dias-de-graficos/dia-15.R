library(tidyverse)
library(extrafont)  #Estos dos paquetes son casi como de la familia
library(tidytext)   
library(tm)
library(SnowballC)
library(fastmatch)  #Estos cuatro paquetes son para la mineria de texto
library(ggdendro)   #Y este ultimo para dibujar el dendrograma

#Recupero mi funcion para hacer preprocesamiento de datos textuales en castellano, que no es perfecta pero aqui ha cumplido bien
preprocesamiento <- function(x) removeWords(removePunctuation(removeNumbers(tolower(x))),stopwords("spanish"))

#Cargamos los datos de recetas tipicas asturianas que nos brinda el Ayuntamiento de Gijon y que aparecen en el portal de datos abiertos del Gobierno
dat <- read.csv("http://datos.gijon.es/doc/turismo/recetas.csv", sep = ",",
                encoding = "UTF-8")

#Nos vamos a centrar en la variable ingredientes. Primero hacemos el preprocesamiento de su texto
dat$ingredientes <- preprocesamiento(dat$ingredientes)

#Despues vamos a quedarnos solo con los terminos mas frecuentes
palabras <- dat$ingredientes %>% tidy() %>% mutate(linea = 1:n()) %>% unnest_tokens(word, x) %>%
  count(word) %>% filter(n > 5) %>% select(word)

#De dichos terminos retiraremos la terminologia de cantidades y unidades de peso 
palabras <- as.data.frame(palabras)
palabras <- palabras[-which(palabras[,1] %in%
                              c("botella", "copa", "cucharada", "cucharadas", 
                                "cucharadita", "diente", "dientes", "grandes", 
                                "ingredientes", "litro", "medianas", "pizca", 
                                "soperas", "vaso")),]

palabras <- palabras[-which(palabras %in%
                              c("dl","gr","kg","l","ml"))]

#Hacemos la DocumentTermMatrix manualmente, con un for. Si, es feo y anticuado, pero funciona en tres lineas
for(i in palabras){
  dat[,i] <- str_count(dat$ingredientes, i)
}

#Quitamos dos platos que aparecen repetidos en la lista y otro cuyos ingredientes no se han parseado bien
dat <- dat[-c(3,4,38),]

#Turno de la variable que denota en que categoria se situa el plato (carne, arroces, pescados, etc.)
#Aqui el preprocesamiento es sencillo, ya que los terminos que nos interesan estan situados entre comas
dat$cat <- sapply(1:nrow(dat), function(i) str_split(dat$categorias, ",")[[i]][1])
dat$cat[which(dat$cat == "Turismo")] <- 
  sapply(which(dat$cat == "Turismo"), function(i) str_split(dat$categorias, ",")[[i]][2])
dat$cat[which(dat$cat == " 2013")] <- "Sin categoría"
dat$cat <- str_trim(dat$cat)

#Pasamos al fin al analisis. Calculamos las distancias de Jaccard (usando el argumento "binary" en la funcion dist) entre ingredientes
distancias <- dist(dat[,palabras], "binary")

#Introducimos las distancias en hclust y ya tenemos el analisis cluster jerarquico hecho
dendro <- hclust(distancias)

#Damos nombre a las etiquetas que han salido
dendro$labels <- dat$nombre

#Pasamos el objeto a dendro_data para poder trabajar con el en ggplot
dendro <- dendro_data(as.dendrogram(dendro))

#Hacemos otro data.frame para las etiquetas del dendrograma. Es importante seguir estos pasos (primero usar label() y luego unir 
#la tabla que salga con la nuestra) ya que de lo contrario se pueden liar las categorias de las etiquetas en el grafico.
etiquetas <- label(dendro) %>% left_join(dat %>% select(nombre, cat), by = c("label" = "nombre"))

#Graficamos
ggplot(segment(dendro)) + 
  geom_segment(aes(x = x, y = -y, xend = xend, yend = -yend)) +           #Para hacer que el texto este alineado con el final del dendrograma:
  geom_text(data = etiquetas, mapping = aes(label = label, x = x, y = y,  #1) invertir y e yend; 2) añadir hjust = 0 en el geom_text
                                                col = cat), hjust = 0,
            family = "Georgia") + 
  theme_void(base_size = 16) + 
  scale_y_continuous(limits = c(-1, 1)) + coord_flip() +
  theme(legend.position = "bottom", 
        text = element_text(family = "Georgia")) + 
  scale_color_brewer("Categoría", palette = "Paired", direction = -1) +
  labs(title = "Análisis clúster jerárquico de recetas asturianas\nsegún similitudes (distancia de Jaccard) entre sus ingredientes",
       subtitle = "Fuente: Ayuntamiento de Gijón, datos.gob.es (https://datos.gob.es/en/catalogo/l01330241-recetas)\nDesafío #30díasdegráficos con R de @R4DS_es, día 15. | @Picanumeros")

#Y ya estaria
ggsave("dia15.png", dpi = 300, width = 10.2, height = 14)
