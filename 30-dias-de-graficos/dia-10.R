library(tidyverse)
library(rvest)
library(tm)
library(tidytext)
library(wordcloud)
#install.packages("ghibli")
library(ghibli)

url <- "http://www.script-o-rama.com/movie_scripts/p/princess-mononoke-script-transcript-hayao.html"
web <- read_html(url)
texto <- html_text(web)
texto <- str_split(texto, "\n\n                   \n\n")[[1]]
texto <- str_replace(texto, "\n\n\n\n", "")
texto <- str_replace(texto, "\n\n", " ")
texto <- texto[-1]

AFINN <- get_sentiments("nrc")

texto %>% tidy() %>% mutate(linea = 1:n()) %>% unnest_tokens(word, x) %>%
  anti_join(stop_words) %>% count(word, sort = T) %>%
  filter(n > 5) %>%
  ggplot(aes(label = word, size = n, col = log(n))) + 
  geom_text_wordcloud_area() +
  scale_size_area(max_size = 24) +
  scale_color_ghibli_c("MononokeMedium") +
  theme_void(base_size = 18) +
  theme(plot.background = element_rect("#F5FDFF"),
        text = element_text(family = "Palatino Linotype")) +
  labs(title = "Wordcloud de términos (en inglés) más frecuentes (n > 5) en la película\n'La Princesa Mononoke' (1997), de Hayao Miyazaki",
       subtitle = "Fuente: http://www.script-o-rama.com/movie_scripts/p/princess-mononoke-script-transcript-hayao.html\nPaleta de colores obtenida del paquete {ghibli}. Desafío #30díasdegráficos con R de @R4DS_es, día 10.",
       caption = "@Picanumeros")
  # with(wordcloud(word, n, max.words = 100, scale = c(5,0.5),
  #                colors = ghibli_palettes$MononokeMedium))
ggsave("dia10_2.png", dpi = 300)

texto %>% tidy() %>% mutate(linea = 1:n()) %>% unnest_tokens(word, x) %>%
  anti_join(stop_words) %>%
  left_join(AFINN, by = "word") %>%
  group_by(sentiment) %>% count(word, sort = T) %>%
  top_n(10) %>%
  filter(sentiment != "NA") %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  filter(sentiment %in% c("surprise","joy","anticipation")==F) %>%
  ggplot(aes(x = word, y = n, fill = sentiment)) + geom_bar(stat = "identity") +
    facet_wrap(~sentiment, scales = "free_y") + coord_flip() +
  theme(legend.position = "none") +
  scale_fill_ghibli_d("MononokeMedium") +
  theme_bw(base_size = 16) +
  theme(legend.position = "none",
        text = element_text(family = "Palatino Linotype")) +
  labs(x = "Frecuencia absoluta del término", y = "Término",
       title = "Términos (en inglés) más frecuentes para cada sentimiento (según el NRC Emotion Lexicon)\nen la película 'La Princesa Mononoke' (1997), de Hayao Miyazaki",
       subtitle = "Fuente: http://www.script-o-rama.com/movie_scripts/p/princess-mononoke-script-transcript-hayao.html\nPaleta de colores obtenida del paquete {ghibli}. Desafío #30díasdegráficos con R de @R4DS_es, día 10.",
       caption = "@Picanumeros")
ggsave("dia10_3.png", dpi = 300, width = 12.5, height = 10)
