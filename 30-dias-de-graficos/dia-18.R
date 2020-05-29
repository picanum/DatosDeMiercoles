library(robis)
library(tidyverse)
library(maps)
library(ggthemes)
library(ggforce)

world <- map_data("world")

molram <- occurrence("Orcinus orca")

atun <- occurrence("Thunnus thynnus")

variables_utiles<-c("date_year","decimalLongitude","decimalLatitude","originalScientificName")

ggplot() +
  geom_polygon(data = world, aes(x = long, y = lat, group = group), fill = "#808080") +
  coord_cartesian(xlim = c(-80,-45), ylim = c(30,55)) +
  geom_point(data = rbind(molram[which(!is.na(molram$date_year)),variables_utiles],
                          atun[which(!is.na(atun$date_year)),variables_utiles]
                          ) %>% filter(originalScientificName %in% c("Orcinus orca", "Thunnus thynnus") &
                                         date_year >= 2006),
             aes(x = decimalLongitude, 
                 y = decimalLatitude,
                 fill=originalScientificName), size=5,
             shape = 21, alpha = .75) + 
  scale_fill_viridis_d(name = "Nombre científico", begin = 0.8, end = 0.2) +
  geom_mark_hull(data = rbind(molram[which(!is.na(molram$date_year)),variables_utiles],
                       atun[which(!is.na(atun$date_year)),variables_utiles]
  ) %>% filter(originalScientificName %in% c("Orcinus orca", "Thunnus thynnus") &
                 decimalLongitude > -80 & decimalLongitude < -45 &
                 decimalLatitude > 30 & decimalLatitude < 55 &
                 date_year >= 2006),
  mapping = aes(x = decimalLongitude, 
      y = decimalLatitude,
      fill=originalScientificName,
      label = ifelse(originalScientificName == "Orcinus orca",
                     "Las orcas son depredadores naturales del atún",
                     "Los atunes tienen una gran presencia\nen la Costa Atlántica norteamericana"))) +
  theme_fivethirtyeight() +
  theme(text = element_text(size=16),
        legend.position = "bottom")+
  labs(title = 'Avistamientos de orcas y atunes desde el año 2006\nfrente a la Costa Atlántica de EEUU y Canadá', 
       subtitle = "Fuente: Ocean Biogeographic Information System (OBIS) a través del paquete {robis}\nDesafío #30díasdegráficos con R de @R4DS_es, día 18.",
       caption = "@Picanumeros")

ggsave("dia18.png", dpi = 300, width = 9, height = 8)
