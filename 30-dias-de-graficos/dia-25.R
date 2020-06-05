library(tidyverse)
library(extrafont)
library(tidytext)
library(ggthemes)

#Vamos a utilizar un dataset con todos los generos existentes en Spotify. Cada genero viene con el valor promedio para cada una de sus features.
#Hay que descargar primero el dataset de Kaggle y después cargarlo desde el PC. Se puede descargar desde el siguiente enlace:
#https://www.kaggle.com/yamaerenay/spotify-dataset-19212020-160k-tracks?select=data_by_genres.csv
dat <- read.csv("datasets_670134_1185459_data_by_genres.csv")

#Truco para que el hip-hop cuente como un género y no como dos
dat$genres <- str_replace_all(dat$genres, "hip hop", "hiphop")

#Seleccionamos los 10 términos más repetidos entre todos los géneros registrados
generos <- as.character(dat$genres) %>% tidy() %>%
  unnest_tokens(word, x) %>% count(word, sort = T) %>%
  top_n(10) %>% select(word)

generos <- unlist(c(generos))

for(i in 1:length(generos)){
  dat[,generos[i]] <- str_detect(dat$genres, generos[i])
}

dat %>% pivot_longer(cols = generos, 
                     names_to = "genero",
                     values_to = "value") %>%
  filter(value == T) %>%
  mutate(genero = factor(genero,
                         levels = levels(as.factor(genero))[
                           order(dat %>% pivot_longer(cols = generos, 
                                                      names_to = "genero",
                                                      values_to = "value") %>%
                                   filter(value == T) %>%
                                   group_by(genero) %>%
                                   summarise(media = mean(energy)) %>%
                                   select(media))
                         ])) %>%
  ggplot(aes(x = genero, y = energy, fill = genero)) +
  #geom_boxplot(width = .5, alpha = .5, outlier.size = 1) +
  geom_point(position = position_jitter(width = .2), alpha = .4) +
  geom_violin(alpha = .5, width = 1.3) + 
  scale_fill_brewer("", palette = "Paired") +
  theme_minimal(base_size = 16) +
  scale_y_continuous(breaks = seq(0, 1, by = 0.2)) +
  annotate("text", x = 9, y = 0.1, label = "@Picanumeros", size = 8, col = "grey") +
  labs(x = "Género musical", y = "Energía",
       title = "Distribución de la energía (intensidad, actividad) de las canciones de un\ndeterminado género musical",
       subtitle = "Fuente: Kaggle (obtenido originalmente de la API de Spotify). Desafío #30díasdegráficos con R de @R4DS_es.",
       caption = "Los puntos representan el valor medio de las canciones de un subgénero en cuyo nombre aparezca el del género principal.\n(P. ej. la energía media de las canciones de 'acid rock' representará un punto en el género 'rock')") +
  theme(legend.position = "none",
        text = element_text(family = "Microsoft Sans Serif"),
        plot.background = element_rect("#F0F0F0ff"))

ggsave("dia25.png", dpi = 300)
