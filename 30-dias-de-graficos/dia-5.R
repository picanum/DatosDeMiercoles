library(tidyverse)  #Para el procesamiento de datos
library(extrafont)  #Para usar tipografias de Windows en los graficos
library(tm)         #Para el procesamiento de datos textuales
library(tidygraph)  #Para usar herramientas del tidyverse para grafos
library(ggraph)     #Para poder graficar los grafos (arcos)
library(tools)      #Para usar su comando toTitleCase(), con el que podemos poner un texto en las mayusculas de titulo

#Vamos a usar un .csv del GitHub https://github.com/melozap/Brooklyn-99-Pilot-Analysis donde están las frases junto a su autora o autor.
#Alternativamente podemos scrapear los dialogos de https://brooklyn99.fandom.com/wiki/Pilot_Transcript pero a la mitad dejan de designar
#quien dice cada frase :(
url <- "https://raw.githubusercontent.com/melozap/Brooklyn-99-Pilot-Analysis/master/brooklyn-99.csv"
texto <- readr::read_csv(url)
colnames(texto) <- c("autor","frase")

#Vamos a hacer, para cada personaje protagonista, una columna binaria que nos indique si son mencionados en la linea correspondiente.
#Notese que he puesto todas las formas en las que recuerdo que son mencionados los personajes.
#Lo hago con str_locate, separando cada una de las posibles menciones con el operador |
texto$AMY <- ifelse(is.na(str_locate(texto$frase, "Amy|Santiago")[,1])==F, 1, 0)
texto$JAKE <- ifelse(is.na(str_locate(texto$frase, "Jake|Peralta")[,1])==F, 1, 0)
texto$HOLT <- ifelse(is.na(str_locate(texto$frase, "Captain|Raymond|Holt")[,1])==F, 1, 0)
texto$TERRY <- ifelse(is.na(str_locate(texto$frase, "Sergeant|Terry|Jeffords")[,1])==F, 1, 0)
texto$CHARLES <- ifelse(is.na(str_locate(texto$frase, "Charles|Boyle")[,1])==F, 1, 0)
texto$GINA <- ifelse(is.na(str_locate(texto$frase, "Gina|Linetti")[,1])==F, 1, 0)
texto$ROSA <- ifelse(is.na(str_locate(texto$frase, "Rosa|Diaz")[,1])==F, 1, 0)
texto$HITCHCOCK <- ifelse(is.na(str_locate(texto$frase, "Hitchcock")[,1])==F, 1, 0)
texto$SCULLY <- ifelse(is.na(str_locate(texto$frase, "Scully")[,1])==F, 1, 0)

#Pongo una semilla para que siempre genere el mismo grafico de arco
set.seed(1)

texto %>% group_by(autor) %>%
  summarise_if(is.numeric, sum) %>%                                 #Suma todas las veces que alguien menciona a cada personaje
  pivot_longer(cols = colnames(texto)[3:11],                        #Lo paso a formato largo para aprovecharlo para el ggplot
               names_to = "variable", values_to = "value") %>%
  filter(value > 0) %>%                                             #Quito todas aquellas relaciones en las que no haya habido menciones
  mutate(autor = toTitleCase(tolower(as.character(autor))),         #Pasamos el nombre de cada personaje a mayusculas solo en la primera letra
    variable = toTitleCase(tolower(as.character(variable))),
    autor_ley = autor) %>%                                          #Esta variable es importante, nos permitira poner colores a cada arco luego
  as_tbl_graph(directed = T) %>%                                    #Lo pasamos a formato grafo (importante que sea dirigido, claro)
  ggraph(layout = "linear") +                                       #Empezamos el ggraph
  geom_edge_arc(aes(width = value, col = autor_ley), alpha = 1) +   #El grosor de cada arco segun el numero de menciones, y el color segun el autor
  scale_edge_colour_brewer('Quién menciona', type = "qual",         #Elijo una paleta de colores vivos pero respetuosos con el daltonismo
                           palette = "Paired") +
  scale_edge_width(name = "Nº menciones", breaks = c(1,3,5,7,9)) +  #Con este comando controlo lo referente al grosor de los arcos
  geom_node_label(aes(label = name),  repel = FALSE, family = "Impact") +   #Ponemos las labels en su lugar correspondiente (sin repeler)
                                                                            #y con tipografia Impact (la usada en los carteles de la serie)
  theme_graph() +                                                   #Usamos el tema predeterminado del paquete ggraph, aunque...
  theme(plot.background = element_rect("#F0F0F0ff")) +              #... le modificamos el color de fondo al gris de theme_fivethirtyeight
  labs(title = "Menciones entre protagonistas en el capítulo 1x01 (piloto) de Brooklyn Nine-Nine",
       subtitle = "Los arcos de arriba se leen de izq. a dcha. (p. ej. Charles menciona 1 vez a Gina), y los de abajo de dcha. a izda (p. ej. Gina menciona 1 vez a Amy)\nFuente: https://github.com/melozap/Brooklyn-99-Pilot-Analysis. Desafío #30díasdegráficos en R de @R4DS_es, día 5",
       caption = "@Picanumeros")

#Y listo
ggsave("dia-5.png", dpi = 300)
