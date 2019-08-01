library(dplyr)
library(ggplot2)
library(ggthemes)
library(stringr)

dat <- readr::read_csv("https://raw.githubusercontent.com/cienciadedatos/datos-de-miercoles/master/datos/2019/2019-07-31/la_casa_de_papel.csv")

#Añadimos una variable que indique qué episodio es de todos (no sólo dentro de una temporada). Lo hacemos "a mano" mediante ifelse
dat$epi_temp <- dat$episodio + ifelse(dat$temporada == 1, 0, ifelse(dat$temporada == 2, 9, 15))

####CREACIÓN DE LA MATRIZ DE TÉRMINOS####
library(tm)
library(SnowballC)
library(fastmatch)

#Con esta función quitamos los paréntesis de los subtítulos ya que no forman parte del diálogo en sí
quitarparentesis <- function(x) gsub("\\s*\\([^\\)]+\\)","",as.character(x))

#Con esta función aplicamos un preprocesamiento basado en quitar palabras stoppers, signos de puntuación, números y ponerlo todo en minúsculas
preprocesamiento <- function(x) removeWords(removePunctuation(removeNumbers(tolower(x))),stopwords("spanish"))

dat$texto2 <- preprocesamiento(quitarparentesis(dat$texto))

#La función de preprocesamiento no quita los símbolos de apertura de exclamación ni de interrogación, así que los quitamos también
dat$texto2 <- str_remove_all(dat$texto2, "¡")
dat$texto2 <- str_remove_all(dat$texto2, "¿")

#Creamos la matriz de términos y rescatamos aquellos que se hayan mencionado más de 50 veces a lo largo de la serie
DTM1 <- DocumentTermMatrix(Corpus(VectorSource(t(dat$texto2))))
DTM1<-as.matrix(DTM1)
DTM1<-as.data.frame(DTM1)
DTM1<-DTM1[,which(apply(DTM1,2,sum) > 50)]

#Unimos la matriz de términos al data.frame original... y dejamos respirar un poco a la memoria RAM :')
dat <- data.frame(dat,DTM1)
rm("DTM1")
gc()

#En dat2 almacenaremos el número de veces que se repite un término en cada episodio
dat2 <- reshape2::melt(dat[,c(7,9:121)], id.vars = "epi_temp")
dat2 <- aggregate(value ~ epi_temp + variable, dat2, FUN=sum)

#Acto seguido sacamos la duración de cada episodio, y la unimos a dat2
tiempos <- aggregate(tiempo_salida ~ epi_temp, dat, FUN = function(x) max(x)/60)
dat2 <- merge(dat2, tiempos, by = "epi_temp")

#Y empezamos a graficar
dat2 %>% filter(variable %in% c("amor","vida","vamos")) %>% 
  ggplot(aes(x = epi_temp, y = value/as.numeric(tiempo_salida), col = variable)) + 
  geom_line() + geom_point() + stat_smooth( se = F) + 
  geom_vline(xintercept = 9.5) + geom_vline(xintercept = 15.5) +
  theme_bw(base_size = 14) + scale_color_discrete(name = "Término") +
  labs(title = "Evolución del nº de veces por minuto que se usa cada término por episodio de La Casa de Papel",
       subtitle = "Fuente: MySubs, recopilado en el Github de @R4DS_es (#DatosDeMiercoles semana 31-07-2019)",
       x = "Episodio", y = "Número de ocurrencias por minuto", caption = "@Picanumeros") + 
  ggplot2::annotate("text", label = "1ª TEMPORADA", x = 9.5/2, y = 1, size = 5) +
  ggplot2::annotate("text", label = "2ª TEMPORADA", x = (15.5 + 9.5)/2, y = 1, size = 5) +
  ggplot2::annotate("text", label = "3ª TEMPORADA", x = (24 + 15.5)/2, y = 1, size = 5)


dat2 %>% 
  filter(variable %in% c("cojones","coño", "joder",
                         "puta", "puto", "mierda","tío")) %>% 
  mutate(variable = 
           recode(variable, puta = "puto/a", puto = "puto/a")) %>%  #Esta línea la añadimos para tratar "puto" y "puta" como un solo término
  ggplot(aes(x = epi_temp, y = value/as.numeric(tiempo_salida), col = variable)) + 
  geom_line(alpha=.25, size = 1.05) + geom_point(alpha=.5) + 
  stat_smooth(se = F, size = 1.25) + geom_vline(xintercept = 9.5) + 
  geom_vline(xintercept = 15.5) + theme_bw(base_size = 14) + scale_color_discrete(name = "Término") +
  labs(title = "Evolución del nº de veces por minuto que se usa cada término por episodio de La Casa de Papel",
       subtitle = "Fuente: MySubs, recopilado en el Github de @R4DS_es (#DatosDeMiercoles semana 31-07-2019)",
        x = "Episodio", y = "Número de ocurrencias por minuto", caption = "@Picanumeros") + 
  ggplot2::annotate("text", label = "1ª TEMPORADA", x = 9.5/2, y = 0.375, size = 5) +
  ggplot2::annotate("text", label = "2ª TEMPORADA", x = (15.5 + 9.5)/2, y = 0.375, size = 5) +
  ggplot2::annotate("text", label = "3ª TEMPORADA", x = (24 + 15.5)/2, y = 0.375, size = 5)

#Mismo gráfico que antes pero con un facet_wrap para poder obtener una serie temporal independiente para cada término
dat2 %>% 
  filter(variable %in% c("cojones","coño", "joder",
                         "puta", "puto", "mierda","tío")) %>% 
  mutate(variable = 
           recode(variable, puta = "puto/a", puto = "puto/a")) %>%
  ggplot(aes(x = epi_temp, y = value/as.numeric(tiempo_salida), col = variable)) + 
  geom_line(alpha=.25, size = 1.05) + geom_point(alpha=.5) + 
  stat_smooth(se = F, size = 1.25) + geom_vline(xintercept = 9.5) + 
  geom_vline(xintercept = 15.5) + theme_bw(base_size = 14) + scale_color_discrete(name = "Término") +
  labs(title = "Evolución del nº de veces por minuto que se usa cada término por episodio de La Casa de Papel",
       subtitle = "Fuente: MySubs, recopilado en el Github de @R4DS_es (#DatosDeMiercoles semana 31-07-2019)",
       x = "Episodio", y = "Número de ocurrencias por minuto", caption = "@Picanumeros") + 
  ggplot2::annotate("text", label = "1ª TEMPORADA", x = 9.5/2, y = 0.375, size = 2.5) +
  ggplot2::annotate("text", label = "2ª TEMPORADA", x = (15.5 + 9.5)/2, y = 0.375, size = 2.5) +
  ggplot2::annotate("text", label = "3ª TEMPORADA", x = (24 + 15.5)/2, y = 0.375, size = 2.5) +
  facet_wrap(~variable) + theme(legend.position = "none")
