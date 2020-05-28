library(tidyverse)
library(extrafont)  #Estos dos paquetes son casi de la familia
library(ggalluvial) #Este paquete servira para hacer el diagrama de Sankey

#Esta vez he tenido que hacer la importacion de forma completamente manual.
#Dicho de otro modo: he cogido la tabla B22R de la pagina http://www.cis.es/cis/export/sites/default/-Archivos/Marginales/3260_3279/3269/cru3269votog2019abr.html
#y la he arreglado en Excel. Una vez hecho, la importo con read.table("clipboard")
dat <- read.table("clipboard", header = T, sep = "\t", dec = ",")

dat %>% pivot_longer(cols = colnames(dat)[-1], names_to = "voto_abr",
                                            values_to = "value") %>%
  filter(voto_nov != "(N)" & voto_abr != "TOTAL") %>%   #Lo de eliminar la etiqueta 'TOTAL' es porque en Excel tambien conte los totales por filas
  mutate(voto_abr = as.character(voto_abr),
         voto_nov = as.character(voto_nov)) %>%
  mutate(voto_abr = ifelse(voto_abr %in% c("PSOE", "PP", "Ciudadanos", "UP...ECP",
                                           "VOX", "ERC", "No.votó", "No.recuerda", "N.C."),
                           voto_abr, "Otras opciones"),
         voto_nov = ifelse(voto_nov %in% c("PSOE", "PP", "Ciudadanos", "UP + ECP",
                                           "VOX", "ERC", "Abstención", "No recuerda", "N.C."),
                           voto_nov, "Otras opciones")) %>%
  group_by(voto_abr, voto_nov) %>% summarise(value = sum(value)) %>%
  ungroup() %>%
  mutate(voto_abr = str_replace_all(str_replace_all(voto_abr, "\\.", " "),
    "UP   ECP", "UP + ECP")) %>%
  mutate(voto_abr = factor(voto_abr,
                           levels = c("PSOE", "PP", "VOX",
                                      "UP + ECP", "Ciudadanos",
                                      "ERC", "Otras opciones",
                                      "No votó", "No recuerda",
                                      "N C ")),
         voto_nov = factor(voto_nov,
                           levels = c("PSOE", "PP", "VOX",
                                      "UP + ECP", "Ciudadanos",
                                      "ERC", "Otras opciones",
                                      "Abstención", "No recuerda",
                                      "N.C."))) %>%
  ggplot(aes(y = value, axis1 = factor(voto_abr), axis2 = voto_nov)) +
  geom_alluvium(aes(fill = voto_nov), width = 1/12) +
  geom_stratum(width = 1/12, fill = "grey90", color = "black") +
  geom_text(stat = "stratum", label.strata = TRUE,
            family = "Liberation Sans") +
  scale_fill_manual(name = "Voto en Noviembre", values = c("red2",
                               "dodgerblue",
                               "green",
                               "purple",
                               "orange",
                               "yellow",
                               "darkgreen",
                               "#777EB9",
                               "#FF00B3",
                               "black")) +
  theme_void(base_size = 18) +
  annotate("text", y = 5000, x = 1, label = "Voto en las\nelecciones del 28-A", family = "Liberation Sans", size = 5) +
  annotate("text", y = 5000, x = 2, label = "Voto en las\nelecciones del 10-N", family = "Liberation Sans", size = 5) +
  theme(legend.position = "bottom",
        text = element_text("Liberation Sans"),
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5)) +
  labs(title = "Diagrama de Sankey de las transferencias de voto entre partidos entre las elecciones\nal Congreso de los Diputados de España del 28 de Abril y del 10 de Noviembre de 2019",
       subtitle = "Fuente: Barómetro de Diciembre 2019 (Postelectoral Elecciones Generales 2019), CIS.\nDesafío #30díasdegráficos con R de @R4DS_es, día 17.",
       caption = "@Picanumeros")

ggsave("dia17.png", dpi = 300, width = 15.5,
       height = 14)
