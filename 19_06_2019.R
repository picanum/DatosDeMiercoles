library(dplyr)
library(ggplot2)
library(ggalluvial)

leyes <- readr::read_csv("https://raw.githubusercontent.com/cienciadedatos/datos-de-miercoles/master/datos/2019/2019-06-19/leyes.csv")
cambios <- readr::read_csv("https://raw.githubusercontent.com/cienciadedatos/datos-de-miercoles/master/datos/2019/2019-06-19/cambios.csv")

cols <- colnames(leyes)[5:9]

#Cambiamos los niveles de las variables registrando la existencia de leyes con mutate_at
leyes <- leyes %>% mutate_at(cols, list(~factor(., levels = c(0, 1), labels = c("No", "Sí"))))

#Creamos el data.frame para el Alluvial Plot
dat <- as.data.frame(table(leyes[,5:9]))

#Ploteamos!
ggplot(dat, aes(y = Freq, 
                axis1 = proteccion_empleo,            #UN EJE POR CADA VARIABLE A REPRESENTAR
                axis2 = proteccion_derechos_amplios,
                axis3 = proteccion_contra_crimenes_odio,
                axis4 = proteccion_contra_incitacion_odio,
                axis5 = proteccion_constitucional)) +
  geom_alluvium(aes(fill = proteccion_empleo),
                width = 1/8, knot.pos = 0, reverse = FALSE) +
  scale_fill_viridis_d(option = "magma", begin = 0.25, end = 0.75) +
  guides(fill = FALSE) +
  geom_stratum(width = 1/8, reverse = FALSE, fill = "lightgrey") +
  theme_minimal(base_size = 14) +
  geom_text(stat = "stratum", label.strata = TRUE, reverse = FALSE) +
  scale_x_continuous(breaks = 1:5, 
                     labels = c("Protección\ndel empleo",
                                "Protección\nde derechos amplios",
                                "Protección contra\ncrímenes de odio",
                                "Protección contra\nincitación al odio",
                                "Protección\nconstitucional")
  ) +
  labs(title = "Protección legal sobre la orientación sexual: relaciones entre niveles de protección",
       subtitle = "Fuente: informe 'Homofobia de Estado' (ILGA, 2019). Datos disponibles en el Github de @R4DS_es\n(#DatosDeMiercoles semana 19-06-2019)",
       x = "¿Cuenta el país con esta protección legal?",
       y = "Nº de países",
       caption = "@Picanumeros")

#### Gráfico para el reconocimiento de derechos civiles ####

cols <- colnames(leyes)[10:13]

leyes <- leyes %>% mutate_at(cols, list(~factor(., levels = c(0, 1), labels = c("No", "Sí"))))

dat <- as.data.frame(table(leyes[,10:13]))

ggplot(dat, aes(y = Freq, 
                axis2 = reconocimiento_matrimonio, 
                axis1 = reconocimiento_union_civil,
                axis3 = reconocimiento_adopcion_conjunta,
                axis4 = reconocimiento_adopcion_segundo_padre)) +
  geom_alluvium(aes(fill = reconocimiento_union_civil),
                width = 1/8, knot.pos = 0, reverse = FALSE) +
  scale_fill_viridis_d(option = "magma", begin = 0.25, end = 0.75) +
  guides(fill = FALSE) +
  geom_stratum(width = 1/8, reverse = FALSE, fill = "lightgrey") +
  theme_minimal(base_size = 14) +
  geom_text(stat = "stratum", label.strata = TRUE, reverse = FALSE) +
  scale_x_continuous(breaks = 1:4, 
                     labels = c("Reconocimiento\nde la unión civil",
                                "Reconocimiento\ndel matrimonio",
                                "Reconocimiento\nde la adopción conjunta",
                                "Reconocimiento\nadopción como\n2º padre/2ª madre")
  ) +
  labs(title = "Protección legal sobre la orientación sexual: relaciones entre derechos reconocidos",
       subtitle = "Fuente: informe 'Homofobia de Estado' (ILGA, 2019). Datos disponibles en el Github de @R4DS_es\n(#DatosDeMiercoles semana 19-06-2019)",
       x = "¿Reconoce el país este derecho?",
       y = "Nº de países",
       caption = "@Picanumeros")
