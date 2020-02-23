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
                       
#### BOXPLOT DE GANANCIAS POR GENERO CINEMATOGRAFICO ####

#Se obtienen los generos ordenados segun la mediana de las ganancias, para poder ordenar despues los boxplots
clases.ordenadas <- as.character(unlist(imbd %>% 
                                        filter(value == TRUE) %>% 
                                        group_by(variable) %>% 
                                        summarise(media = median(log(1+ganancias), na.rm=T)) %>% 
                                        arrange(media) %>% select(variable)))
                       
imbd %>% filter(value == TRUE) %>% 
    mutate(variable = factor(variable,
                             levels = clases.ordenadas)) %>% 
                       ggplot(aes(y = log(1+ganancias), x = variable)) +
    geom_boxplot(fill = "#ffcc66") + coord_flip() + 
                       labs(x = "Género", y = "Logaritmo neperiano de las ganancias en millones de dólares\n(calculado como ln(1 + ganancias))", 
                            title = "Boxplots de las ganancias (en millones de dólares, logaritmizadas)\nde las películas según etiqueta de género registradas en IMDB", 
                            subtitle = "Fuente: IMDB, disponible en Kaggle.\n#DatosDeMiercoles de @R4DS_es, semana 19-02-2020", 
                            caption = "@Picanumeros") + theme_minimal(base_size = 16)
                       
#### BOXPLOTS DE GANANCIAS POR GENERO CINEMATOGRAFICO POR DECADAS ####
library(patchwork)

#Hay que hacer los 9 gráficos por separado y despues juntarlos con la libreria patchwork.
#Ignoro si hay alguna forma mas corta de hacerlo.
g1 <- imbd %>% filter(anio > 1929) %>%
  mutate(decada = cut(anio, 
                      breaks = seq(1930,2020,by=10),
                      labels = paste(c(1930,seq(1941,2011,by=10)), seq(1940,2020,by=10), sep=" - "),
                      include.lowest = T)) %>% 
  filter(value == TRUE & decada == "1930 - 1940") %>% 
  mutate(variable = factor(variable,
                           levels = as.character(unlist(imbd %>% 
                                                          filter(value == TRUE & anio >= 1930 & anio <= 1940) %>% 
                                                          group_by(variable) %>% 
                                                          summarise(media = median(log(1+ganancias), na.rm=T)) %>% 
                                                          arrange(media) %>% select(variable)))
  )) %>% 
  ggplot(aes(y = log(1+ganancias), x = variable)) +
  geom_boxplot(fill = "#ffcc66") + coord_flip() + 
  labs(x = "Género", title = "1930 - 1940", y = ""
       ) + theme_minimal(base_size = 16)

g2 <- imbd %>% filter(anio > 1929) %>%
  mutate(decada = cut(anio, 
                      breaks = seq(1930,2020,by=10),
                      labels = paste(c(1930,seq(1941,2011,by=10)), seq(1940,2020,by=10), sep=" - "),
                      include.lowest = T)) %>% 
  filter(value == TRUE & decada == "1941 - 1950") %>% 
  mutate(variable = factor(variable,
                           levels = as.character(unlist(imbd %>% 
                                                          filter(value == TRUE & anio >= 1941 & anio <= 1950) %>% 
                                                          group_by(variable) %>% 
                                                          summarise(media = median(log(1+ganancias), na.rm=T)) %>% 
                                                          arrange(media) %>% select(variable)))
  )) %>% 
  ggplot(aes(y = log(1+ganancias), x = variable)) +
  geom_boxplot(fill = "#ffcc66") + coord_flip() + 
  labs(x = "Género", title = "1941 - 1950", y = ""
  ) + theme_minimal(base_size = 16)

g3 <- imbd %>% filter(anio > 1929) %>%
  mutate(decada = cut(anio, 
                      breaks = seq(1930,2020,by=10),
                      labels = paste(c(1930,seq(1941,2011,by=10)), seq(1940,2020,by=10), sep=" - "),
                      include.lowest = T)) %>% 
  filter(value == TRUE & decada == "1951 - 1960") %>% 
  mutate(variable = factor(variable,
                           levels = as.character(unlist(imbd %>% 
                                                          filter(value == TRUE & anio >= 1951 & anio <= 1960) %>% 
                                                          group_by(variable) %>% 
                                                          summarise(media = median(log(1+ganancias), na.rm=T)) %>% 
                                                          arrange(media) %>% select(variable)))
  )) %>% 
  ggplot(aes(y = log(1+ganancias), x = variable)) +
  geom_boxplot(fill = "#ffcc66") + coord_flip() + 
  labs(x = "Género", title = "1951 - 1960", y = ""
  ) + theme_minimal(base_size = 16)

g4 <- imbd %>% filter(anio > 1929) %>%
  mutate(decada = cut(anio, 
                      breaks = seq(1930,2020,by=10),
                      labels = paste(c(1930,seq(1941,2011,by=10)), seq(1940,2020,by=10), sep=" - "),
                      include.lowest = T)) %>% 
  filter(value == TRUE & decada == "1961 - 1970") %>% 
  mutate(variable = factor(variable,
                           levels = as.character(unlist(imbd %>% 
                                                          filter(value == TRUE & anio >= 1961 & anio <= 1970) %>% 
                                                          group_by(variable) %>% 
                                                          summarise(media = median(log(1+ganancias), na.rm=T)) %>% 
                                                          arrange(media) %>% select(variable)))
  )) %>% 
  ggplot(aes(y = log(1+ganancias), x = variable)) +
  geom_boxplot(fill = "#ffcc66") + coord_flip() + 
  labs(x = "Género", title = "1961 - 1970", y = ""
  ) + theme_minimal(base_size = 16)

g5 <- imbd %>% filter(anio > 1929) %>%
  mutate(decada = cut(anio, 
                      breaks = seq(1930,2020,by=10),
                      labels = paste(c(1930,seq(1941,2011,by=10)), seq(1940,2020,by=10), sep=" - "),
                      include.lowest = T)) %>% 
  filter(value == TRUE & decada == "1971 - 1980") %>% 
  mutate(variable = factor(variable,
                           levels = as.character(unlist(imbd %>% 
                                                          filter(value == TRUE & anio >= 1971 & anio <= 1980) %>% 
                                                          group_by(variable) %>% 
                                                          summarise(media = median(log(1+ganancias), na.rm=T)) %>% 
                                                          arrange(media) %>% select(variable)))
  )) %>% 
  ggplot(aes(y = log(1+ganancias), x = variable)) +
  geom_boxplot(fill = "#ffcc66") + coord_flip() + 
  labs(x = "Género", title = "1971 - 1980", y = ""
  ) + theme_minimal(base_size = 16)

g6 <- imbd %>% filter(anio > 1929) %>%
  mutate(decada = cut(anio, 
                      breaks = seq(1930,2020,by=10),
                      labels = paste(c(1930,seq(1941,2011,by=10)), seq(1940,2020,by=10), sep=" - "),
                      include.lowest = T)) %>% 
  filter(value == TRUE & decada == "1981 - 1990") %>% 
  mutate(variable = factor(variable,
                           levels = as.character(unlist(imbd %>% 
                                                          filter(value == TRUE & anio >= 1981 & anio <= 1990) %>% 
                                                          group_by(variable) %>% 
                                                          summarise(media = median(log(1+ganancias), na.rm=T)) %>% 
                                                          arrange(media) %>% select(variable)))
  )) %>% 
  ggplot(aes(y = log(1+ganancias), x = variable)) +
  geom_boxplot(fill = "#ffcc66") + coord_flip() + 
  labs(x = "Género", title = "1981 - 1990", y = ""
  ) + theme_minimal(base_size = 16)

g7 <- imbd %>% filter(anio > 1929) %>%
  mutate(decada = cut(anio, 
                      breaks = seq(1930,2020,by=10),
                      labels = paste(c(1930,seq(1941,2011,by=10)), seq(1940,2020,by=10), sep=" - "),
                      include.lowest = T)) %>% 
  filter(value == TRUE & decada == "1991 - 2000") %>% 
  mutate(variable = factor(variable,
                           levels = as.character(unlist(imbd %>% 
                                                          filter(value == TRUE & anio >= 1991 & anio <= 2000) %>% 
                                                          group_by(variable) %>% 
                                                          summarise(media = median(log(1+ganancias), na.rm=T)) %>% 
                                                          arrange(media) %>% select(variable)))
  )) %>% 
  ggplot(aes(y = log(1+ganancias), x = variable)) +
  geom_boxplot(fill = "#ffcc66") + coord_flip() + 
  labs(x = "Género", title = "1991 - 2000", y = ""
  ) + theme_minimal(base_size = 16)

g8 <- imbd %>% filter(anio > 1929) %>%
  mutate(decada = cut(anio, 
                      breaks = seq(1930,2020,by=10),
                      labels = paste(c(1930,seq(1941,2011,by=10)), seq(1940,2020,by=10), sep=" - "),
                      include.lowest = T)) %>% 
  filter(value == TRUE & decada == "2001 - 2010") %>% 
  mutate(variable = factor(variable,
                           levels = as.character(unlist(imbd %>% 
                                                          filter(value == TRUE & anio >= 2001 & anio <= 2010) %>% 
                                                          group_by(variable) %>% 
                                                          summarise(media = median(log(1+ganancias), na.rm=T)) %>% 
                                                          arrange(media) %>% select(variable)))
  )) %>% 
  ggplot(aes(y = log(1+ganancias), x = variable)) +
  geom_boxplot(fill = "#ffcc66") + coord_flip() + 
  labs(x = "Género", title = "2001 - 2010", y = "Logaritmo neperiano de las ganancias en millones de dólares\n(calculado como ln(1 + ganancias))"
  ) + theme_minimal(base_size = 16)

g9 <- imbd %>% filter(anio > 1929) %>%
  mutate(decada = cut(anio, 
                      breaks = seq(1930,2020,by=10),
                      labels = paste(c(1930,seq(1941,2011,by=10)), seq(1940,2020,by=10), sep=" - "),
                      include.lowest = T)) %>% 
  filter(value == TRUE & decada == "2011 - 2020") %>% 
  mutate(variable = factor(variable,
                           levels = as.character(unlist(imbd %>% 
                                                          filter(value == TRUE & anio >= 2011 & anio <= 2020) %>% 
                                                          group_by(variable) %>% 
                                                          summarise(media = median(log(1+ganancias), na.rm=T)) %>% 
                                                          arrange(media) %>% select(variable)))
  )) %>% 
  ggplot(aes(y = log(1+ganancias), x = variable)) +
  geom_boxplot(fill = "#ffcc66") + coord_flip() + 
  labs(x = "Género", title = "2011 - 2020", y = ""
  ) + theme_minimal(base_size = 16)

((g1 / g4 / g7) | (g2 / g5 / g8) | (g3 / g6 / g9)) + 
  plot_annotation(title = "Boxplots de las ganancias (en millones de dólares, logaritmizadas) de las películas\nsegún etiqueta de género y década de estreno registradas en IMDB",
                  subtitle = "Fuente: IMDB, disponible en Kaggle. #DatosDeMiercoles de @R4DS_es, semana 19-02-2020", caption = "@Picanumeros",
                  theme = theme(plot.title = element_text(size = 16),
                                plot.subtitle = element_text(size = 16),
                                plot.caption = element_text(size = 16)))

#### GRAFICOS ANALIZANDO ASOCIACION ENTRE STAR WARS Y ESTRENOS DE PELICULAS DE CIENCIA FICCION Y FANTASIA ####

pelisxgeneroyanio %>% 
  filter(Var2 %in% c("Ciencia Ficción")) %>%
  mutate(Prop = 
           as.numeric(as.character(Freq.x))/as.numeric(as.character(Freq.y))) %>%
  filter(as.numeric(as.character(Var1))> 1960) %>%
  ggplot(aes(x = as.numeric(as.character(Var1)), y = Prop*100)) + 
  geom_segment(aes(x = 1977, xend = 1977, y = 0, yend = 20), col = "red") + 
  geom_segment(aes(x = 1980, xend = 1980, y = 0, yend = 20), col = "red") + 
  geom_segment(aes(x = 1983, xend = 1983, y = 0, yend = 20), col = "red") + 
  geom_segment(aes(x = 1999, xend = 1999, y = 0, yend = 20), col = "red") + 
  geom_segment(aes(x = 2002, xend = 2002, y = 0, yend = 20), col = "red") + 
  geom_segment(aes(x = 2005, xend = 2005, y = 0, yend = 20), col = "red") + 
  geom_segment(aes(x = 2015, xend = 2015, y = 0, yend = 20), col = "red") + 
  geom_segment(aes(x = 2017, xend = 2017, y = 0, yend = 20), col = "red") + 
  annotate("text", x = 1975, y = 20.5, label = "Episodio IV", col = "red") +
  annotate("text", x = 1980, y = -0.5, label = "Episodio\nV", col = "red") +
  annotate("text", x = 1985, y = 20.5, label = "Episodio VI", col = "red") +
  annotate("text", x = 1997, y = 20.5, label = "Episodio I", col = "red") +
  annotate("text", x = 2002, y = -0.5, label = "Episodio\nII", col = "red") +
  annotate("text", x = 2007, y = 20.5, label = "Episodio III", col = "red") +
  annotate("text", x = 2013, y = 20.5, label = "Episodio VII", col = "red") +
  annotate("text", x = 2018, y = -0.5, label = "Episodio\nVIII", col = "red") +
  geom_line(size = 1.1) + theme_minimal(base_size = 16) + 
  labs(title = "Porcentaje de películas por año en IMDB con la etiqueta 'Ciencia Ficción' desde 1960\nEn rojo, estrenos de películas de las trilogías de Star Wars", 
       subtitle = "Fuente: IMDB, disponible en Kaggle.\n#DatosDeMiercoles de @R4DS_es, semana 19-02-2020", 
       x = "Año de estreno", y = "Porcentaje de películas de 'Ciencia Ficción' en ese año", caption = "@Picanumeros") + 
  scale_x_continuous(breaks = seq(1960, 2020, by = 10)) + stat_smooth(se = F)

pelisxgeneroyanio %>% 
  filter(Var2 %in% c("Fantasía")) %>%
  mutate(Prop = 
           as.numeric(as.character(Freq.x))/as.numeric(as.character(Freq.y))) %>%
  filter(as.numeric(as.character(Var1))> 1960) %>%
  ggplot(aes(x = as.numeric(as.character(Var1)), y = Prop*100)) + 
  geom_segment(aes(x = 1977, xend = 1977, y = 0, yend = 15), col = "red") + 
  geom_segment(aes(x = 1980, xend = 1980, y = 0, yend = 15), col = "red") + 
  geom_segment(aes(x = 1983, xend = 1983, y = 0, yend = 15), col = "red") + 
  geom_segment(aes(x = 1999, xend = 1999, y = 0, yend = 15), col = "red") + 
  geom_segment(aes(x = 2002, xend = 2002, y = 0, yend = 15), col = "red") + 
  geom_segment(aes(x = 2005, xend = 2005, y = 0, yend = 15), col = "red") + 
  geom_segment(aes(x = 2015, xend = 2015, y = 0, yend = 15), col = "red") + 
  geom_segment(aes(x = 2017, xend = 2017, y = 0, yend = 15), col = "red") + 
  annotate("text", x = 1975, y = 15.5, label = "Episodio IV", col = "red") +
  annotate("text", x = 1980, y = -0.5, label = "Episodio\nV", col = "red") +
  annotate("text", x = 1985, y = 15.5, label = "Episodio VI", col = "red") +
  annotate("text", x = 1997, y = 15.5, label = "Episodio I", col = "red") +
  annotate("text", x = 2002, y = -0.5, label = "Episodio\nII", col = "red") +
  annotate("text", x = 2007, y = 15.5, label = "Episodio III", col = "red") +
  annotate("text", x = 2013, y = 15.5, label = "Episodio VII", col = "red") +
  annotate("text", x = 2018, y = -0.5, label = "Episodio\nVIII", col = "red") +
  geom_line(size = 1.1) + theme_minimal(base_size = 16) + 
  labs(title = "Porcentaje de películas por año en IMDB con la etiqueta 'Fantasía' desde 1960\nEn rojo, estrenos de películas de las trilogías de Star Wars", 
       subtitle = "Fuente: IMDB, disponible en Kaggle.\n#DatosDeMiercoles de @R4DS_es, semana 19-02-2020", 
       x = "Año de estreno", y = "Porcentaje de películas de 'Fantasía' en ese año", caption = "@Picanumeros") + 
  scale_x_continuous(breaks = seq(1960, 2020, by = 10)) + stat_smooth(se = F)
