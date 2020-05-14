library(tidyverse)  #Para preprocesamiento de datos
library(extrafont)  #Para añadir fuentes de Windows en los gráficos
library(WDI)        #Para descargar los datos del Banco Mundial
library(ggrepel)    #Para poder etiquetar sin solapamientos en los graficos

#Vamos a descargar datos de tres indicadores: poblacion total del pais, indice Gini (estimado por el Banco Mundial,
#en este enlace tienen mas info: http://iresearch.worldbank.org/PovcalNet/home.aspx) e ingresos fiscales (% del PIB)
dat <- WDI::WDI(indicator = c("SP.POP.TOTL",    #Poblacion total
                       "SI.POV.GINI",           #Indice Gini (Estimacion Banco Mundial)
                       "GC.TAX.TOTL.GD.ZS"),    #Ingresos fiscales (% del PIB)
         start = 2015, end = 2015,              #Elegimos 2015 puesto que es el año del que tenemos mas paises con datos disponibles
         extra = TRUE)                          #Con extra = TRUE añade a la query datos adicionales, incluida la region donde esta el pais

dat %>% 
  drop_na() %>%                         #Quitamos paises con datos faltantes
  filter(region != "Aggregates") %>%    #Quitamos datos agregados de paises de una region (ya que son filas no correspondientes a paises)
  ggplot(aes(x = GC.TAX.TOTL.GD.ZS,     #Empezamos a graficar. Ingresos en eje X, gini en eje Y, coloreamos segun region
             y = SI.POV.GINI, 
             col = region,
             label = ifelse(SP.POP.TOTL > 50e6, country, NA))) +  #En label registramos el nombre de todos los paises con >50M hab. (NA en otro caso)
  geom_point(aes(size = SP.POP.TOTL), alpha = .75) +  #Tamaño de los puntos ha de ser proporcional a la poblacion y transparencia del 75%
  geom_label_repel(size = 3, alpha = .75) +           #Añadimos etiquetas con los nombres de los paises como indicamos en la linea 20
  scale_size_continuous(range =                       #Muy importante. Con este argumento controlamos dos cosas:
                          c(3,                        #1) los limites en el tamaño de los puntos (yo puse de 3 a 50 para que se vieran todos bien)
                            50), guide = 'none') +    #2) que no salgan los indicadores de tamaño en la leyenda (ya que en este caso no aportan mucho)
  theme_bw(base_size = 15) +                          #Tema discreto, en blanco y negro
  labs(x = "Ingresos fiscales (% del PIB) del país",  #Añadimos las labels
                    y = "Índice de Gini",
                    title = "Relación entre ingresos fiscales (% del PIB) e índice de Gini (Estimación Banco Mundial)\npor países con datos disponibles en 2015. Tamaño proporcional a la población.",
       subtitle = "Fuente: Banco Mundial (indicadores SP.POP.TOTL, SI.POV.GINI y GC.TAX.TOTL.GD.ZS, 2015).\nDesafío #30díasdegráficos con R de @R4DS_es, día 3.",
       caption = "@Picanumeros") +
  theme(legend.position = "bottom", text = element_text(family = "Verdana")) +  #Que la leyenda vaya abajo y la tipografia sea Verdana
  guides(colour = guide_legend(nrow=2,byrow=TRUE,                               #Tambien muy importante. Con este comando hacemos:
                               override.aes = list(size=10))) +                 #1) que la leyenda vaya en 2 lineas en vez de 1
                                                                                #2) que los puntos de la leyenda tengan tamaño 10 
  scale_color_viridis_d(name = "Región",            #Usaremos la escala inferno (con limite en 0.8 para no saturar mucho con los claros)
                        option = "inferno",         #y traducimos los nombres de las regiones al español
                        begin = 0, end = 0.8,
                        labels = c("Asia Oriental y Pacífico",
                                   "Europa y Asia Central",
                                   "Latinoamérica y el Caribe",
                                   "Oriente Medio y Norte de África",
                                   "África Sub-sahariana"))

ggsave("dia3.png", dpi = 300, width = 12, height = 8)
