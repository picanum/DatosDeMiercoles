
library(corrplot)
library(dplyr)
library(reshape2)
library(viridis)

#Cargamos los datos (el .csv está creado guardando el objeto dataset_spotify resultante de ejecutar el código de @R4DS_es)
dat <- read.csv("dataset_spotify.csv") 

#### GRAFICO DE CORRELACIONES ####

M <- dat %>% filter(top_pais == "El Top 50 de España") %>% select(colnames(dat)[8:20]) %>% cor(method = "spearman")
colnames(M) <- c("Bailabilidad", "Energía", "Nota musical", "Volumen", "Modo",
                 "Hablado", "Acústico", "Instrumental", "En vivo", "Positividad",
                 "Tempo", "Duración", "Tiempo compás")
rownames(M) <- colnames(M)

corrplot(M, method = "color",
         type = "upper", order = "hclust", hclust.method = "complete", 
         addrect = 3, rect.col = "black", rect.lwd = 30, mar=c(1,0,2.5,0), 
         addCoef.col = "black", number.cex = 0.6, 
         title = "Correlación de Spearman entre los atributos de las\n50 canciones más escuchadas de Spotify en España")
text(1.5, 2.5, "Fuente: API de Spotify siguiendo script disponible\nen el Github de @R4DS_es\n(#DatosDeMiercoles semana 16-05-2019)")
title(xlab="@Picanumeros")

#### BOXPLOTS DEL TEMPO MEDIO ####

paises <- str_replace_all(dat$top_pais,"El Top 50 de la ","")
paises <- str_replace_all(paises, "El Top 50 de ","")
dat <- dat %>% mutate(pais = paises)
medianas_tempo <- aggregate(tempo ~ pais, dat, FUN = median)
dat$pais <- factor(dat$pais, levels = medianas_tempo$pais[order(medianas_tempo$tempo, decreasing=T)])

ggplot(dat, aes(x = pais, y = tempo)) + geom_boxplot(aes(fill = pais), col = "black") + 
  scale_fill_viridis(option = "plasma", direction = -1, discrete = T) + 
  theme_bw(base_size=14) + theme(legend.position="none") +
  coord_flip() + scale_y_continuous(breaks = seq(80,200, by = 20))+
  labs(title = "Diagramas de caja y bigotes del tempo promedio (pulsos/minuto)\nde las 50 canciones más escuchadas de Spotify en cada país",
       subtitle = "Fuente: API de Spotify siguiendo script disponible en el Github de @R4DS_es\n(#DatosDeMiercoles semana 16-05-2019)",
       y = "Tempo promedio (pulsos/minuto)", x ="", 
       caption = "@Picanumeros") 
