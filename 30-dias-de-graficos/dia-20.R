library(tidyverse)
library(extrafont)
library(tidygraph)
library(ggraph)

dat <- read.csv("https://www.ine.es/jaxiT3/files/t/es/csv_bdsc/24447.csv?nocab=1",
                sep = ";", encoding = "UTF-8")
colnames(dat) <- c("gen","destino","origen","periodo","total")

grafo <- dat %>% filter(gen == "Ambos sexos" & 
                 destino != "Total Nacional" &
                 origen != "Total Nacional" &
                 periodo == "2019S1") %>%
  mutate(destino = str_sub(destino, 4, -1),
         origen = str_sub(origen, 4, -1),
         total = as.numeric(as.character(str_remove(total, "\\.")))) %>%
  select(destino, origen, total) %>%
  mutate(total = ifelse(is.na(total), 0, total),
         origen = ifelse(origen == "Santa Cruz de Tenerife", "SC Tenerife", origen),
         destino = ifelse(destino == "Santa Cruz de Tenerife", "SC Tenerife", destino)) %>%
  group_by(origen) %>%
  mutate(total = 100*total/sum(total)) %>%
  ungroup()

layout <- create_layout(as_tbl_graph(grafo %>% filter(total >= 5)), layout = 'fr', weights = grafo$total[which(grafo$total >= 5)])

ggraph(layout) +
  geom_edge_fan2(aes(width = total), alpha = 0.5) +   
  scale_edge_width(name = "Nº migraciones", range = c(0.5,4)) +  
  geom_node_point(size = 15) + 
  geom_node_label(aes(label = name),  repel = FALSE, family = "Liberation Sans") +   
  theme_graph() + theme(legend.position = "none", 
                        plot.background = element_rect("#F0F0F0ff")) +
  labs(title = "Red de migraciones interiores entre provincias españolas en el 1er semestre de 2019\n(contando únicamente aquellas que suponen el 5% o más de las emigraciones totales de una provincia)",
       subtitle = "Fuente: Estadística de migraciones, INE. Elaboración propia con datos extraídos del sitio web www.ine.es. Desafío #30díasdegráficos con R de @R4DS_es, día 20.",
       caption = "@Picanumeros")
ggsave("dia20_1.png", width = 14, height = 9, dpi = 300)


layout <- create_layout(as_tbl_graph(grafo), layout = 'fr', weights = grafo$total)

ggraph(layout) +
  geom_edge_fan2(aes(width = total), alpha = 0.5) +   
  scale_edge_width(name = "Nº migraciones", range = c(0.5,4)) +  
  geom_node_label(aes(label = name), repel = FALSE, family = "Liberation Sans") +   
  theme_graph() + theme(legend.position = "none", 
                        plot.background = element_rect("#F0F0F0ff")) +
  labs(title = "Red de migraciones interiores entre provincias españolas en el 1er semestre de 2019",
       subtitle = "Fuente: Estadística de migraciones, INE. Elaboración propia con datos extraídos del sitio web www.ine.es. Desafío #30díasdegráficos con R de @R4DS_es, día 20.",
       caption = "@Picanumeros")
ggsave("dia20_2.png", dpi = 300, height = 8*1.2, width = 13*1.2)
