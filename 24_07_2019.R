library(dplyr)
library(ggplot2)
library(ggthemes)
library(stringr)
library(patchwork)

apps <- readr::read_csv("https://raw.githubusercontent.com/cienciadedatos/datos-de-miercoles/master/datos/2019/2019-07-24/apps_googleplaystore.csv")

#Obtenemos una variable numérica para el tamaño de la app. Para ello hay que extraer los símbolos que representan los millones y los miles
apps$tamanio_num <- as.numeric(str_remove_all(str_remove_all(apps$tamanio, "M"),"k"))
apps$tamanio_num[which(!is.na(str_locate(apps$tamanio,"k")[,1]))] <- 
  apps$tamanio_num[which(!is.na(str_locate(apps$tamanio,"k")[,1]))]/1000

#Construimos el primer gráfico (básico) a partir del cual obtendremos la moda de cada distribución
g <- ggplot(apps, aes(x = calificacion, y = ..density.., fill = precio==0)) + 
  geom_density(alpha = .5) + scale_fill_viridis_d(name = "¿App gratuita?",
                                                  labels = c("No","Sí"))

#Los datos del ggplot que acabamos de crear los podemos extraer con ggplot_build y llamando al objeto data
datos_graf <- ggplot_build(g)$data[[1]]

#Sacamos las modas para cada uno de los dos grupos (delimitados por la variable de fill)
modas <- aggregate(density ~ fill, data = datos_graf, FUN = max)

#Buscamos y guardamos los puntos en el eje X donde se encuentran las modas
moda_no <- datos_graf$x[which(datos_graf$fill == modas[1,1] & 
                     datos_graf$density == modas[1,2])]
moda_si <- datos_graf$x[which(datos_graf$fill == modas[2,1] & 
                                datos_graf$density == modas[2,2])]

#Añadimos al gráfico todo lo que le faltaba
g1 <- g + geom_vline(xintercept = moda_no) + geom_vline(xintercept = moda_si) +
  scale_x_continuous(breaks=round(c(1,1.5,2,2.5,3,3.5,4,5, moda_no+0.05, moda_si-0.05),2),
                     labels=c(1,1.5,2,2.5,3,3.5,4,5,"Moda\n(No):\n4.51","Moda\n(Sí):\n4.39")) + #(Para representar las modas en el eje X hacemos un poco de trampa para que no se pisen :D)
  theme_classic(base_size = 14) + 
  theme(axis.ticks.x = element_blank(), legend.position = "top")+
  labs(title = "Densidad de frecuencia de las calificaciones a las apps en Google Play Store según su gratuituidad",
       subtitle = "Fuente: Kaggle, recopilado en el repositorio de Github de @R4DS_es (#DatosDeMiercoles semana 24-07-2019)",
       x = "Calificación", y = "Densidad\n(cuanto mayor, más frecuente)")

#SEGUNDO GRÁFICO (BOXPLOTS):

options(scipen=999) #Incluimos este comando para que no nos salte la notación científica en el eje X

g2 <- ggplot(apps, aes(x = factor(instalaciones/1000), 
                 y = tamanio_num,
                 fill = log(instalaciones+1))) + geom_boxplot() +
  labs(x = "Número de instalaciones (en miles) que ha sobrepasado la app",
       y = "Tamaño de la app (Megabytes)",
       title = "Diagramas de caja y bigotes del tamaño de las apps en Google Play Store según el nivel de descargas en el que se encuentran",
       subtitle = "Fuente: Kaggle, recopilado en el repositorio de Github de @R4DS_es (#DatosDeMiercoles semana 24-07-2019)")+
  theme_classic(base_size = 14) + 
  scale_fill_viridis_c(begin = 0.2) + theme(legend.position = "none")

#Representamos ambos gráficos con la ayuda del paquete patchwork
g1 / g2 + plot_layout(ncol = 1, heights = c(200,350))

