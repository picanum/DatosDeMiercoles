library(tidyverse)    #Para el procesamiento de datos
library(extrafont)    #Para usar tipografias de Windows en los graficos
#remotes::install_github("wilkelab/ggtext")
library(ggtext)       #Nos permitira usar las funcionalidades de RMarkdown dentro de los textos del grafico

#El famoso cuarteto del paper de F. J. Anscombe de 1973 esta disponible en el paquete datasets, que es uno de los paquetes base
#Tan solo tenemos que invocarlo con la funcion data() y esperar unos segundos (o ejecutar el comando anscombe acto seguido)
data(anscombe)

#Con este comando vamos a designar los titulos de cada una de las cuatro facetas
#Usaremos paste0 ya que nos interesa concatenar los resultados de los estadisticos descriptivos
nombres <- paste0(c("**Un conjunto relativamente normal**",   #Notese que para enfatizar encerramos el texto entre **
                    "**Una relación curvilínea**",            #Este es el comando que se usa en RMarkdown para ello
                    "**Una dependencia funcional perfecta... salvo por un outlier**",
                    "**Todos los datos en un punto... salvo por un outlier**"),
  "\n\nMedia de X = ", colMeans(anscombe[,1:4]),              #Ahora debemos poner dos \n en lugar de uno, ya que en RMarkdown se requiere doble espaciado
                                                              #Escribimos el texto y despues añadimos el vector numerico. Asi sucesivamente.
                 ", Media de Y = ", round(colMeans(anscombe[,5:8]),5),
                 ",\n\nVarianza de X = ", apply(anscombe[,1:4],2,var) * (nrow(anscombe)-1)/nrow(anscombe),
                 ", Varianza de Y = ", round(apply(anscombe[,5:8],2,var) * (nrow(anscombe)-1)/nrow(anscombe),5),
                 ",\n\nCorrelación de Pearson = ", round(diag(cor(anscombe)[1:4,5:8]), 5))

#Este paso tan simple es muy importante: para poder usar estos titulos en las facetas, debemos hacer que los nombres del vector
#coincidan con los de la variable que delimita cada una de las facetas. Como vamos a hacer que esta variable tome los valores 1, 2, 3 y 4
#(uno para cada dataset del cuarteto), le damos esos nombres al vector.
names(nombres) <- 1:4

#Creamos un data.frame nuevo con las siguientes columnas:
#- X, donde pondremos los valores de la variable X para cada uno de los cuatro datasets, todos seguidos (con unlist(c()) es posible)
#- Y, donde haremos lo mismo para la variable Y
#- dataset, que sera el identificador del dataset para cada una de las instancias. Como hemos puesto los datos de los cuatro datasets seguidos,
#proporcionaremos el identificador con la funcion rep(), que repetira cada uno de ellos tantas veces como filas tenga el dataset (11)
data.frame(x = unlist(c(anscombe[,1:4])), y = unlist(c(anscombe[,5:8])), 
           dataset = rep(1:4, each = 11)) %>%
  ggplot(aes(x = x, y = y)) + 
  geom_point(size = 4, col = "red") +                     #Añadimos los puntos y los ponemos en rojo
  stat_smooth(method = "lm", se = F, col = "orange") +    #La recta de regresión en naranja
  facet_wrap(~dataset, #scales = "free",                  #Con facet_wrap dividimos el grafico en tantas facetas como clases tenga la variable dataset
                                                          #Lo que hace ggplot entonces es tratar cada uno de los subconjuntos definidos por las clases como un dataset aparte
             labeller = as_labeller(nombres)) +           #Y con labeller le asignaremos los titulos que hemos definido antes a cada faceta
                                                          #Importante: el vector de titulos debe ir siempre dentro de la funcion as_labeller()
  theme_classic(base_size = 16) +                         #Un tema planito que no distraiga mucho
  scale_x_continuous(breaks = seq(4,20,by=2)) +
  labs(title = "El famoso cuarteto de Anscombe: 4 datasets bidimensionales\ncon medias, varianzas y covarianzas (casi) iguales",
       subtitle = "Fuente: Anscombe, F. J. (1973). Graphs in statistical analysis. The american statistician, 27(1), 17-21.\nDisponible en R en el paquete {datasets}. Desafío #30díasdegráficos con R de @R4DS_es, día 4.",
       x = "X", y = "Y", caption = "@Picanumeros") +
  theme(text = element_text(family = "Tahoma"),           #Finalmente, en theme pondremos el element_text para todo el texto con estilo Tahoma...
        strip.text = element_markdown())                  #... y en strip.text, que nos permite manejar los titulos de las facetas,
                                                          #añadimos un element_markdown() para decirle a ggplot que los lea como si fuese un archivo RMarkdown

#Y con esto ya esta. Guardamos el grafico.
ggsave("dia4_2.png", 
       height = 10, width = 12, 
       dpi = 300)
        
