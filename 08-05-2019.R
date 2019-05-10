library(ggplot2)    #Para los gráficos
library(ggthemes)   #Para embellecer los gráficos
library(dplyr)      #Para el tratamiento de datos
library(patchwork)  #Para colocar varios gráficos en un mismo plot
library(viridis)    #Para acceder a una paleta de colores guay

#Cargamos los datos
datos_uip <- readr::read_csv("https://raw.githubusercontent.com/cienciadedatos/datos-de-miercoles/master/datos/2019/2019-05-08/datos_uip.csv")

#Función que cojo prestada de https://stackoverflow.com/questions/19876505/boxplot-show-the-value-of-mean aunque con un pequeño cambio
fun_mean <- function(x){
  return(data.frame(y=mean(x),
                    label=paste("Media =",round(mean(x,na.rm=T),2)))
         )
  }

#A graficar!

p1 <- datos_uip %>% filter(!is.na(cuota_genero)) %>%   #Nos quitamos aquellos parlamentos de los que no tengamos datos al completo
  ggplot(aes(x=cuota_genero, y=porcentaje_mujeres, fill=cuota_genero)) + 
  geom_boxplot()+ #geom_jitter() +    #No he usado geom_jitter para estos boxplots porque quedaba raro, aunque ahí lo dejo para quien quiera probar
  theme_minimal(base_size=13) +       #Usamos un tema discretito esta vez
  labs(x = "¿Existe cuota de género?", y = "Porcentaje (%) de mujeres") +
  theme(legend.position = "none")+    #No nos interesa que haya una leyenda para el color de relleno (ya que no es muy informativo); la quitamos
  stat_summary(fun.y = mean, geom="point",colour="darkred", size=3) +   #Estas dos funciones son las que meten la media en los boxplots
  stat_summary(fun.data = fun_mean, geom="text", vjust=-0.7) +          #Copiadas igualmente de https://stackoverflow.com/questions/19876505/boxplot-show-the-value-of-mean
  scale_fill_manual(values = viridis(2, begin = 0.5, end = 0.8))        #Metemos los colores de viridis

#Para el gráfico del tamaño del parlamento hay que quitar a China, que tiene un parlamento de 2975 integrantes que supone un outlier que dificulta la visualización
p2 <- datos_uip %>% filter(!is.na(cuota_genero)) %>% filter(iso_pais != "chn") %>%   
  ggplot(aes(x=cuota_genero, y=numero_integrantes, fill=cuota_genero)) + 
  geom_boxplot() + #geom_jitter() +
  theme_minimal(base_size=13) + labs(x = "¿Existe cuota de género?", y = "Número de integrantes\nde la cámara") +
  theme(legend.position = "none") +
  stat_summary(fun.y = mean, geom="point",colour="darkred", size=3) +
  stat_summary(fun.data = fun_mean, geom="text", vjust=-0.7) + 
  scale_fill_manual(values = viridis(2, begin = 0.5, end = 0.8))

p3 <- datos_uip %>% filter(!is.na(cuota_genero)) %>%
  ggplot(aes(x=cuota_genero, y=edad_elegibilidad, fill=cuota_genero)) + 
  geom_boxplot() + #geom_jitter() +
  theme_minimal(base_size=13) + labs(x = "¿Existe cuota de género?", y = "Edad mínima requerida\npara elegibilidad") +
  theme(legend.position = "none")+
  stat_summary(fun.y = mean, geom="point",colour="darkred", size=3) +
  stat_summary(fun.data = fun_mean, geom="text", vjust=-0.7) + 
  scale_fill_manual(values = viridis(2, begin = 0.5, end = 0.8))

p4 <- datos_uip %>% filter(!is.na(cuota_genero)) %>% 
  ggplot(aes(x=cuota_genero, y=integrante_mas_joven, fill=cuota_genero)) + 
  geom_boxplot() + #geom_jitter() +
  theme_minimal(base_size=13) + labs(x = "¿Existe cuota de género?", y = "Edad del integrante\nmás joven de la cámara") +
  theme(legend.position = "none") +
  stat_summary(fun.y = mean, geom="point",colour="darkred", size=3) +
  stat_summary(fun.data = fun_mean, geom="text", vjust=-0.7) + 
  scale_fill_manual(values = viridis(2, begin = 0.5, end = 0.8))

#Gracias a la librería patchwork podemos implementar varios gráficos en uno, no hay más que sumarlos
p1 + p2 + p3 + p4 + plot_layout(ncol = 2, nrow = 2) + 
  #Si además queremos meter títulos para la mezcla de plots al completo, usamos plot_annotation
  plot_annotation(title = "Influencia de la cuota de género en parlamentos mundiales sobre % de mujeres, número de integrantes y sus edades",
                  subtitle = "Fuente: datos abiertos de la Unión Interparlamentaria, accesibles en el Github de @R4DS_es (#DatosDeMiercoles semana 08-05-2019)",
       caption = "@Picanumeros",
       theme = theme(
         plot.title = element_text(size = 18),
         plot.subtitle = element_text(size = 12)
         )
       )
