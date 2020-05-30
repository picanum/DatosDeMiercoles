library(tidyverse)
library(extrafont)
#install.packages("ggTimeSeries")
library(ggTimeSeries)
#devtools::install_github("hrbrmstr/streamgraph")
library(streamgraph)

#Una vez mas, para el grafico de hoy he hecho una consulta en la web del SIIU y la he copiado en Excel para luego llevarmela a R
#La consulta se puede hacer mediante el enlace: http://estadisticas.mecd.gob.es/EducaJaxiPx/Tabla.htm?path=/Universitaria/Series/Estudiantes/Egresados//l0/&file=SEGR201.px&type=pcaxis&L=0
dat <- read.table("clipboard", header=T, sep="\t")
colnames(dat) <- c("Curso", "Ciencias Sociales y Jurídicas",
                   "Ingeniería y Arquitectura",
                   "Artes y Humanidades",
                   "Ciencias de la Salud",
                   "Ciencias")

#Nos quedamos con el año en el que egresan en formato numerico y pasamos la tabla a formato largo
dat <- dat %>% mutate(Curso = str_trim(Curso),
                      curso_num = as.numeric(substr(Curso, 6, 9))) %>%
  pivot_longer(cols = colnames(dat)[-1], names_to = "campo", values_to = "total")

#Ahora hacemos el grafico con las herramientas de {streamgraph}. Para que nos entendamos, las analogias con ggplot serian:
#- key = fill
#- value = y
#- date = x
#En este caso le pongo el interactive = F ya que no me interesa un grafico interactivo, aunque si se quiere hacer un Shiny es muy interesante
streamgraph(data = dat, key = "campo", value = "total", date = "curso_num",
            interactive = F) %>% 
            #... y ahora tenemos que ir poniendo las etiquetas (la rama de enseñanza y los títulos -!!!-) con sg_annotate
            #Se supone que con sg_title() se puede poner el titulo al grafico pero yo no he conseguido hacer que funcione
            #La funcion toma los mismos argumentos que annotate() de ggplot
  sg_annotate(label = levels(factor(dat$campo)),
              x = rep(as.Date("1997-01-01"), 5),
              y = c(20, 38, 55, 125, 195)*1000, col = "black", size = 20) %>%
  sg_annotate(label = "Número de egresados de carreras universitarias en España, 
              por rama de enseñanza",
              x = as.Date("1985-01-01"), y = 234000,
              size = 26) %>%
  sg_annotate(label = "Fuente: Sistema Integrado de Información Universitaria (SIIU). Desafío #30díasdegráficos con R de @R4DS_es, día 19.",
              x = as.Date("1985-01-01"), y = 225000,
              size = 18) %>%
  sg_annotate(label = "@Picanumeros",
              x = as.Date("2012-01-01"), y = 0000,
              size = 18) %>%
              #Y ahora los argumentos para embellecer: sg_fill_brewer, que funciona como scale_fill_brewer,
              #y sg_axis_x como scale_x_continuous()
  sg_fill_brewer(palette = "Paired") %>%
  sg_axis_x(tick_interval = 2)
#Lo he guardado desde el visor de RStudio (pestaña "Viewer") con la opcion Export --> Save as image. 

#Ahora hacemos el mismo grafico pero con ggplot. Para ponerlo en forma de streamgraph tan solo hay que usar el argumento
#stat_steamgraph(), y todo lo demas lo configurareos como un ggplot normal.
ggplot(dat, aes(x = curso_num, y = total, fill = campo,
                label = campo)) +
  stat_steamgraph(alpha = .75) + theme_minimal(base_size = 16) +
  scale_y_continuous(breaks = 1e06) +
  scale_x_continuous(breaks = seq(1986, 2015, by = 2),
                     expand = c(0,0)) +
  scale_fill_viridis_d(begin = 0.2, end = 1) +
  labs(x = "Año", y = "",
       title = "Evolución del número de personas egresadas de carreras universitarias en España\n(Grado y 1er y 2º Ciclo) desde 1986 hasta 2015, según rama de enseñanza",
       subtitle = "Fuente: Sistema Integrado de Información Universitaria (SIIU), Secretaría General de Universidades.\nDesafío #30díasdegráficos con R de @R4DS_es, día 19.",
       caption = "@Picanumeros") +
  annotate("text", x = rep(2000, 5), size = 6,
           y = c(-80, -52, -35, -15, 45)*1000,
           label = levels(factor(dat$campo))[c(5, 2, 1, 3, 4)],
           family = "Liberation Sans") +
  theme(legend.position = "none",
        text = element_text(family = "Liberation Sans"))
        
ggsave("dia19_ggplot.png", dpi = 300)
