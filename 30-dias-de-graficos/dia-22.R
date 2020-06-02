library(tidyverse)
library(extrafont)    #Estos dos paquetes son casi como de la familia
library(rvest)        #El paquete habitual para leer paginas web
library(tidytext)     
library(tm)           #Estos dos paquetes para operar con datos textuales (el segundo tiene funcionalidades para tratar textos en español)
library(ggwordcloud)  #Paquete para hacer los wordclouds en ggplot

#Recupero mi funcion para hacer preprocesamiento de datos textuales en castellano, que no es perfecta pero aqui ha cumplido bien
preprocesamiento <- function(x) removeWords(removePunctuation(removeNumbers(tolower(x))),stopwords("spanish"))

#Vamos a sacar las transcripciones de los discursos de estos enlaces. Cada uno de ellos se almacenara en un elemento de una lista.
urls <- c("https://www.publico.es/espana/discurso-rey-2019-discurso-rey-nochebuena-2019.html",
          "https://www.lavanguardia.com/politica/20181224/453750347839/discurso-rey-navidad-2018-texto-integro.html",
          "https://www.elmundo.es/espana/2017/12/24/5a3fbfd9468aebec3c8b458f.html",
          "https://www.elmundo.es/espana/2016/12/25/585e9ccd46163fd13f8b45d6.html",
          "https://www.elmundo.es/espana/2015/12/24/567c33d246163f5b5b8b45f9.html",
          "https://elpais.com/politica/2014/12/24/actualidad/1419442224_270426.html")

texto <- list()

#Recorremos cada enlace con un for. Como se puede observar, se cuenta todo el discurso desde el saludo hasta la despedida
#(para la despedida he tenido que poner varios terminos ya que no siempre se despide de la misma manera)
#Despues le quito algunos restos de regex que hayan quedado, y acto seguido le paso la funcion de preprocesamiento
for(i in 1:length(urls)){
  web <- read_html(urls[i])
  temp <- html_text(web)
  texto[[i]] <- str_split(str_split(temp, "Buenas noches")[[1]][2],
                     "Buenas noches|próspero año|Boas Festas|Boas festas")[[1]][1]
  texto[[i]] <- str_replace_all(texto[[i]], "\\n", " ")
  texto[[i]] <- str_replace_all(texto[[i]], "\\t", " ")
  texto[[i]] <- str_replace_all(texto[[i]], "\\r", " ")
  texto[[i]] <- preprocesamiento(texto[[i]])
}

#Lo meto todo en un vector de tipo character
texto = as.character(unlist(c(texto)))

#Lo paso a tibble con un tidy() y ya empiezo a usar las herramientas de tidytext
texto %>% tidy() %>%
  mutate(anio = 2019:2014) %>%                  #Añado una variable con el año de discurso
  unnest_tokens(word, x) %>%                    #unnest_tokens para que cada fila sea una palabra
  group_by(anio) %>% count(word, sort = T) %>%  #Obtengo la cuenta de palabras usadas en cada discurso (frecuencias absolutas)
  mutate(fi = n/sum(n)) %>%                     #Obtengo las frecuencias relativas (% de veces que aparece sobre el total de palabras) 
  filter(n > 2) %>%                             #Me quedo con aquellos que aparezcan 3 o mas veces
  ungroup() %>%
  ggplot(aes(label = word, size = fi, col = log(n))) +        #Para el wordcloud necesito en el aesthetic los terminos label y size
  geom_text_wordcloud_area(family = "Liberation Sans") +      #Despues basta con pasar el comando geom_text_wordcloud_area()
  scale_size_area(max_size = 11) +                            #Aqui especifico el tamaño maximo de las palabras de cada wordcloud
  scale_color_viridis_c(begin = 0, end = 0.7) +               #Todo lo demas ya es lo habitual en un ggplot: facetas, labels y retoque del theme
                                                              #(en este caso para cambiar el tamaño de algunas labels)
  facet_wrap(~anio) + 
  theme_minimal(base_size = 16) +
  labs(title = "Wordclouds de los términos con más de 2 apariciones de todos los discursos de Navidad del Rey Felipe VI",
       subtitle = "A más grande el término, más veces fue mencionado en el discurso. Las frecuencias son relativas, por lo que la longitud del discurso no afecta.\nFuentes: publico.es, La Vanguardia, El Mundo, El País (ver enlaces abajo). Desafío #30díasdegráficos con R de @R4DS_es, día 22.",
       x = "@Picanumeros",
       caption = paste(c("Enlaces a las fuentes:", urls), collapse = "\n")) +
  theme(text = element_text(family = "Liberation Sans"),
        plot.caption = element_text(size = 9),
        strip.text = element_text(size = 18))

ggsave("dia22.png", dpi = 300, width = 14.5, height = 9)
