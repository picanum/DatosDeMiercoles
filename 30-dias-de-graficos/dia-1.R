library(rvest)      #Para importar tablas desde paginas web
library(extrafont)  #Para usar otras fuentes en los graficos
#font_import()      #Si es la primera vez que usas extrafont, tienes que ejecutar esto primero
library(tidyverse)  #Para todo lo demas

#En este enlace de la Wikipedia estan disponibles los datos de los escaños conseguidos por el partido mayoritario
#en todas las elecciones españolas desde 1979
url <- "https://es.wikipedia.org/wiki/Elecciones_generales_de_Espa%C3%B1a"

#Leemos la web con read_html
web <- read_html(url)

#Obtenemos las tablas con html_table, cogemos la primera de todas (donde estan los datos que nos interesan)
#y seleccionamos las filas de 4 en 4, empezando por la 3. Ahi estan los datos de escaños
tabla <- html_table(web, fill = TRUE)[[1]][seq(3,55, by = 4),]

#Para obtener los datos del año de celebracion y el partido que consiguio los escaños, hay que indagar de nuevo en la tabla
#En la columna 1 se encuentra el año, mientras que en la 5 esta el partido. En ambos casos los datos estan en las filas 1, 5, 9, 13...
#asi que hay que recorrerlas de nuevo con seq
tabla[,1] <- html_table(web, fill = TRUE)[[1]][c(1,seq(5,53, by = 4)),1]
tabla[,3] <- html_table(web, fill = TRUE)[[1]][c(1,seq(5,53, by = 4)),5]

#Nos quedamos solo con las variables que nos interesan y las renombramos (esto se puede hacer tambien con pipelines)
tabla <- tabla[,1:3]
names(tabla) <- c("Elecciones","Dip","Partido")


tabla %>% 
  mutate(Dip = as.numeric(str_remove_all(Dip, "/350"))) %>%           #Quitamos el /350 que hay en todos los datos y lo pasamos a numerico
  ggplot(aes(x = Elecciones, y = Dip, fill = Partido, label = Dip)) + #Comenzamos el ggplot acto seguido
  geom_bar(stat = "identity", width = 0.75) +                         #Comando para las barras. Importante el stat = "identity"
                                                                      #para que ggplot use la altura de la variable Y como altura de las barras
  geom_text(nudge_y = 5, size = 5) +                                  #Comando para meter el numero de escaños encima de cada barra
  scale_fill_manual(name = "Partido con más diputados",               #Comando para darle color a las barras y nombre a la leyenda
                    values = c("dodgerblue","red2","green4")) +       #Procuramos darle a cada partido su color corporativo (PSOE rojo, PP azul, UCD verde)
  geom_vline(xintercept = 10.5, linetype = "dashed", size = 1.1) +    #Colocamos una linea que indique el inicio del periodo sin mayorias absolutas
  scale_y_continuous(limits = c(0, 210), expand = c(0,0)) +           #Importante el expand = c(0,0) si queremos que el gráfico empiece en 0
  theme_minimal(base_size = 18) +                                     #Tema minimal (para no sobrecargar) y acto seguido las labels
  labs(title = "Número de escaños conseguidos por el partido mayoritario\nen cada una de las elecciones al Congreso de los Diputados de España desde 1979",
       subtitle = paste0("Fuente: Wikipedia (",url,")\nDesafío #30díasdegráficos con R de @R4DS_es, día 1"),
       x = "Elecciones", y = "Número de escaños",
       caption = "@Picanumeros")+
  theme(legend.position = "bottom",
        text = element_text(family = "Source Sans Pro"),              #Empleamos la fuente Source Sans Pro (necesitamos el paquete extrafont)
        plot.background = element_rect("#FFFECD"),                    #Color de fondo del gráfico
        panel.grid = element_line("#D4D4D4"))                         #Color de las lineas de la malla que cuadricula el grafico
  
