library(ggplot2)
library(gganimate)
library(dplyr)
library(stringr)
library(extrafont)

imdb <- readr::read_csv("https://raw.githubusercontent.com/cienciadedatos/datos-de-miercoles/master/datos/2020/2020-02-19/ranking_imdb.csv")

#Vamos a binarizar primero los generos de cada pelicula, un poco a lo bruto pero funciona igual
generos <- do.call(rbind.data.frame,str_split(imdb$genero, ", "))                 #Metemos los generos en un data.frame
vec_generos <- c(levels(generos[,1]), levels(generos[,2]), levels(generos[,3]))   #Hacemos un vector que contenga todos los generos observados
vec_generos <- vec_generos[-which(duplicated(vec_generos))]

#Con este for (perdon de antemano), generamos una columna booleana para cada genero que nos dice si la i-esima pelicula esta etiquetada en ese genero o no
for(i in vec_generos){
  generos[,i] <- apply(generos, 1, function(x) x[1] == i | x[2] == i | x[3] == i)
}

#Unimos esas nuevas columnas a la base original
imdb <- cbind(imdb, generos[,-c(1:3)])

#Creamos un nuevo data.frame llamado imbd (ojo al cambio de letras) para poder tener toda la informacion sobre generos en una columna
imbd <- reshape2::melt(imdb, id.vars = colnames(imdb)[1:9])

#Hacemos una tabla que nos diga cuantas peliculas de cada genero hubo en cada año y la convertimos a data.frame
#Es importante añadir el which(imbd$value == TRUE), ya que sirve para contar todas las etiquetas de genero que hay cada año
pelisxgeneroyanio <- data.frame(table(imbd$anio[which(imbd$value == TRUE)], 
                                                 imbd$variable[which(imbd$value == TRUE)]))

#Juntamos esos datos con los del numero de peliculas que hubo cada año en total, para asi poder relativizar las cifras
#(por ejemplo, que haya 10 peliculas de cine negro en un año es relevante si en ese año ha habido 15 peliculas. No lo es si ha habido 1000 peliculas)
pelisxgeneroyanio <- merge(pelisxgeneroyanio, data.frame(table(imdb$anio)), 
                           by = "Var1", all.x = T)

#Con estos datos podemos empezar a generar la animacion
g <- pelisxgeneroyanio %>% filter(as.numeric(as.character(Var1)) > 1929) %>%  #Quitamos los datos previos a 1930 por falta de muestra
  mutate(Var1 = factor(as.character(Var1))) %>%                               #Dado que Var1 (el año) es un factor, hay que rehacerlo para quitar los niveles (años) que ya no estan
  mutate(decada = cut(as.numeric(as.character(Var1)),                         #Con estas lineas creamos una nueva variable que registre la decada de la pelicula
                      breaks = seq(1930,2020,by=10),
                      labels = paste(c(1930,seq(1941,2011,by=10)), seq(1940,2020,by=10), sep=" - "),
                      include.lowest = T)) %>%
  mutate(Var2 = factor(Var2,                                                  #Con estas lineas reordenamos el factor genero para que salgan primero los generos mas prevalentes
                       levels = names(sort(table(imbd$variable[which(imbd$value==TRUE)]), decreasing = T)))) %>%
  group_by(Var2, decada) %>%                                                  #Con group_by y summarise calculamos los subtotales por decadas
  summarise(Freq.gen = sum(as.numeric(as.character(Freq.x))),
            Freq.tot = sum(as.numeric(as.character(Freq.y)))) %>%
  ggplot(aes(y = Freq.gen*100/Freq.tot,                                       #Y, al fin, graficamos!
             x = Var2)) +
  geom_bar(stat = "identity", fill = "#3B90FF") + transition_states(decada, transition_length = 10, state_length = 10) +
  labs(title = "Porcentaje de películas de IMDB en cada década\ncon cada etiqueta de género cinematográfico",
       subtitle = "Fuente: IMDB, disponible en Kaggle.\n#DatosDeMiercoles de @R4DS_es, semana 19-02-2020",
       x = "Género", y = "Porcentaje de películas con esa etiqueta",
       caption = "@Picanumeros") + coord_flip() + 
  geom_text(aes(x = 10, y = 50, label = decada), size = 10, family = "Liberation Sans") + 
  theme_classic(base_size = 14) + 
  theme(text = element_text(family = "Liberation Sans"),
        plot.background = element_rect(fill = "Light Gray"),
        panel.background = element_rect(fill = "Light Gray")
        )

animate(g, nframes=120, height = 480, width = 480)
