library(spotifyr)   #Para usar la API de Spotify
library(tidyverse)  #Para el procesamiento de datos
library(ggridges)   #Para introducir las ridgelines
library(extrafont)  #Para utilizar las fuentes de Windows en los graficos

#Lo primero que hay que hacer es obtener una clave para la API de Spotify, se puede hacer en https://developer.spotify.com/
#Una vez obtenidas las introducimos asi:
Sys.setenv(SPOTIFY_CLIENT_ID = "id_cliente")
Sys.setenv(SPOTIFY_CLIENT_SECRET = "id_cliente_secreta")

#Y con este comando accederemos a Spotify directamente
access_token <- get_spotify_access_token()

#Una busqueda para ver cual es la id de Muse en Spotify
View(search_spotify("Muse", "artist"))

#Una vez encontrada, obtenemos las id's de los albums
albums <- get_artist_albums("12Chz98pHFMPJEknJQMWvI", "album")$id

#Obtenemos la info de las canciones, en concreto: nombre (por si quieres analizarlas por separado), id, nombre del album y fecha
#get_album_tracks te devuelve un data.frame por album, asi que los obtenemos de forma iterativa con un lapply,
#y despues los juntamos todos en un solo data.frame con el comando do.call(rbind.data.frame, lista)
canciones <- do.call(rbind.data.frame,
                     lapply(1:length(albums),
                            function(i) data.frame(get_album_tracks(albums[i])[,c("name","id")],
                                                   album = get_album(albums[i])$name,
                                                   fecha = get_album(albums[i])$release_date)))

#Nos quedamos solo con los albums de estudio que hay en Spotify
canciones <- canciones %>%
  filter(album %in% c("Showbiz",
                      "Origin of Symmetry",
                      "Absolution",
                      "Black Holes and Revelations",
                      "The Resistance",
                      "The 2nd Law",
                      "Drones",
                      "Simulation Theory (Super Deluxe)"))

#Con get_track_audio_features obtendremos las caracteristicas musicales de aquellas canciones para las que proporcionamos id
#Hay que partir el vector en dos porque el maximo de canciones en una sola llamada es de 100
caract <- get_track_audio_features(canciones$id[1:100])
caract[101:131,] <- get_track_audio_features(canciones$id[101:131])

canciones %>% 
  left_join(caract, "id") %>%   #Unimos los dos datasets anteriores
  ggplot(aes(x = danceability,  #Graficamos: en el eje X, la bailabilidad, en el Y, los albums, y otra vez el X para dar color (en fill)
  y = album, 
  fill = ..x..)
  ) +
  geom_density_ridges_gradient(scale = 2) +   #Comando para introducir las ridgelines
  labs(x = "Índice 'danceability' (0 = lo menos bailable, 1 = lo más bailable)",
       y = "Álbum",
       title = "Se han vuelto bailongos: la evolución de la bailabilidad de\nlas canciones de Muse a través de sus álbums de estudio",
       subtitle = "Fuente: API de Spotify accedida a través del paquete {spotifyr}.\nDesafío #30díasdegráficos con R de @R4DS_es, día 7.",
       caption = "@Picanumeros") + 
  scale_x_continuous(breaks = seq(0,1,by = 0.2)) +
  theme_minimal(base_size = 14) + 
  geom_segment(aes(x = 0.2, xend = -0.3,          #Introducimos el segmento que nos lleva al texto donde se indica como funciona la densidad
               y = 9.25, yend = 9.25)) +
  annotate("text", x = -0.525, y = 9.25,          #Introducimos dicho texto
           family = "Copperplate Gothic Light",
           size = 3.5,
           label = "Cuanto más alta sea la campana,\nmás canciones tienen la bailabilidad\nindicada en el eje X en ese punto") +
  coord_cartesian(xlim = c(-0.2, 1.2), clip = "off") +    #Importante: para que el texto no nos estire el grafico, hay que especificar
                                                          #en coord_cartesian el comando clip = "off" y asi permitirle que salga del panel
  geom_text(data = canciones %>%                                  #Comando para introducir las medianas de cada ridgeline
                 left_join(caract, "id") %>%                      #Primero se obtiene un dataset con las medianas de bailabilidad de cada disco
                 group_by(album) %>%
                 summarise(mean = median(danceability)),
               aes(x = mean, y = c(1:2 + 1, 3 + 0.85,             #Yo he posicionado las etiquetas en Y un poco a ojo mediante ensayo-error
                                   4:5 + 1,                       #Picando un poco mas de codigo se puede optimizar de forma automatica
                                   6 + 0.85, 7 + 1, 8 + 1.25), 
                   label = paste0("Mediana = ",round(mean,2))),
            family = "Copperplate Gothic Light") +
  scale_fill_gradientn(colours = c("#f77f14", "#d32d03","darkorange",     #Con este comando introducimos una paleta de colores que recuerde
                                   "#0405d1", "#0daf28", "#c5c5c5")) +    #(vagamente) a la de la portada de The Resistance
  theme(text = element_text(family = "Copperplate Gothic Bold"),
        legend.position = "none",
        panel.grid = element_line("grey50",
                                  linetype = "dashed"),
        plot.background = element_rect("#F0F0F0ff"),
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))

#Y ya estaria
ggsave("dia7.png", dpi = 300, width = 12.5, height = 8)
