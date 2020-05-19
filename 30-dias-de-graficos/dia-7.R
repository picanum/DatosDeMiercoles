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
                            
#### ANEXO: RIDGELINES SOBRE FRANZ FERDINAND, ARCADE FIRE, RADIOHEAD Y LOS PLANETAS ####
                            
#Franz Ferdinand

View(spotifyr::search_spotify("Franz Ferdinand", "artist"))

albums <- spotifyr::get_artist_albums("0XNa1vTidXlvJ2gHSsRi4A", "album")$id
canciones <- do.call(rbind.data.frame,
        lapply(1:length(albums),
               function(i) data.frame(get_album_tracks(albums[i])[,c("id","name")],
           album = get_album(albums[i])$name,
           fecha = get_album(albums[i])$release_date)))

canciones <- canciones %>%
  filter(album %in% c("Franz Ferdinand",
                      "You Could Have It So Much Better",
                      "Tonight",
                      #"Blood",
                      "Right Thoughts, Right Words, Right Action",
                      "Always Ascending"))

canciones <- canciones[-which(duplicated(canciones$name)),]

caract <- get_track_audio_features(canciones$id)

canciones %>% left_join(caract, "id") %>%
  ggplot(aes(x = energy
             , y = album)) +
  geom_density_ridges(col = "#FFF6CE",
                               fill = "#FFE475",
                               alpha = .75) +
  scale_x_continuous(breaks = seq(0, 1, by = 0.2)) +
  theme_minimal(base_size = 14) +
  labs(x = "Energía (0 = lo menos enérgico, 1 = lo más enérgico)",
       y = "Álbum",
       subtitle = "Fuente: API de Spotify accedida a través del paquete {spotifyr}.\nDesafío #30díasdegráficos con R de @R4DS_es, día 7.",
       title = "I say, take me out! ¿Cómo ha evolucionado la energía\nde las canciones de Franz Ferdinand a través de sus álbums?",
       caption = "@Picanumeros") +
  theme(plot.background = element_rect("black"),
        panel.grid = element_line(colour = "#FFF6CE", linetype = "dashed"),
        text = element_text(colour = "#FFF6CE",
                            face = "bold",
                            family = "Century Gothic"),
        axis.text = element_text(colour = "#FFF6CE")) +
  geom_text(data = canciones %>% 
              left_join(caract, "id") %>% 
              group_by(album) %>% 
              summarise(mean = median(energy)),
            aes(x = mean, y = 1:5 + 0.4, 
                label = paste0("Mediana = ",round(mean,2))),
            family = "Century Gothic") +
  annotate("label", x = 0.45, y = 4.6, size = 3,
           label = "Esta colina menos energética\nes por 'Fade Together' y\n'Eleanor Put Your Boots On'",
           family = "Century Gothic") +
  annotate("label", x = 0.2, y = 3.35, size = 3,
           label = "Y esta otra por 'Katherine Kiss Me'",
           family = "Century Gothic")
ggsave("franz.png", dpi = 300, width = 11.2, height = 8)

#Los Planetas

View(spotifyr::search_spotify("Los Planetas", "artist"))

albums <- spotifyr::get_artist_albums("0N1TIXCk9Q9JbEPXQDclEL", "album")$id
canciones <- do.call(rbind.data.frame,
                     lapply(1:length(albums),
                            function(i) data.frame(get_album_tracks(albums[i])[,c("name","id")],
                                                   album = get_album(albums[i])$name,
                                                   fecha = get_album(albums[i])$release_date)))

canciones <- canciones %>%
  filter(album %in% c("Zona Temporalmente Autónoma (Edición Especial)",
                      "Principios Basicos De Astronomia",
                      "Canciones para una Orquesta Química") == F)

canciones <- canciones[-which(duplicated(canciones$name)),]

caract <- get_track_audio_features(canciones$id[1:100])
caract[101:nrow(canciones),] <- get_track_audio_features(canciones$id[101:nrow(canciones)])

canciones %>% left_join(caract, "id") %>%
  filter(!str_detect(name, "Demo")) %>%
  ggplot(aes(x = duration_ms/(1000*60)
             , y = album)) +
  geom_density_ridges(alpha = .75, fill = "black") +
  scale_x_continuous(breaks = seq(0, 12, by = 2)) +
  labs(x = "DURACIÓN EN MINUTOS",
       y = "ÁLBUM",
       caption = "@PICANUMEROS",
       title = "EVOLUCIÓN EN LA DURACIÓN DE LAS CANCIONES DE LOS PLANETAS",
       subtitle = "FUENTE: API DE SPOTIFY ACCEDIDA A TRAVES DEL PAQUETE {SPOTIFYR}.\nDESAFIO #30díasdegráficos CON R DE @R4DS_es, DÍA 7.") +
  theme_bw(base_size = 16) +
  theme(panel.grid = element_line(colour = "grey50", linetype = "dashed"),
    panel.background = element_rect("#FDE33A"),
    text = element_text(family = "Verdana", face = "bold")) +
  geom_label(data = canciones %>% 
              left_join(caract, "id") %>% 
              group_by(album) %>% 
              summarise(mean = median(duration_ms/(1000*60))),
            aes(x = mean, y = 1:9 + 0.6, 
                label = paste0("MEDIANA = ",round(mean,1))),
            family = "Verdana", size = 3, alpha = 0.75)
ggsave("planetas.png", dpi = 300, width = 15.6, height = 10)

#Arcade Fire

View(spotifyr::search_spotify("Arcade Fire", "artist"))

albums <- spotifyr::get_artist_albums("3kjuyTCjPG1WMFCiyc5IuB", "album")$id
canciones <- do.call(rbind.data.frame,
                     lapply(1:length(albums),
                            function(i) data.frame(get_album_tracks(albums[i])[,c("name","id")],
                                                   album = get_album(albums[i])$name,
                                                   fecha = get_album(albums[i])$release_date)))

canciones <- canciones %>%
  filter(album %in% c("Reflektor (Deluxe)",
                      "The Suburbs (Deluxe)",
                      "The Suburbs (Deluxe Edition)") == F)

canciones <- canciones[-which(duplicated(tolower(canciones$name))),]
canciones <- canciones[-which(canciones$name %in% c("It's Never Over (Hey Orpheus)",
                                                    "Everything_Now (continued)",
                                                    "Infinite_Content")),]

caract <- get_track_audio_features(canciones$id)

canciones %>% left_join(caract, "id") %>%
  ggplot(aes(x = valence, y = album, fill = ..x..)) +
  geom_density_ridges_gradient() +
  theme_minimal(base_size = 16) +
  scale_x_continuous(breaks = seq(0, 1, by = 0.2)) +
  labs(x = "Índice 'valence' (positividad). 0 = lo más negativo. 1 = lo más positivo",
       y = "Álbum",
       title = "Las emociones de Arcade Fire a través de sus discos: de negativos a positivos",
       subtitle = "Fuente: API de Spotify accedida a través del paquete {spotifyr}.\nDesafío #30díasdegráficos con R de @R4DS_es, día 7.",
       caption = "@Picanumeros") +
  scale_fill_gradientn(colours = c("darkgreen","lightblue","red")) +
  theme(legend.position = "none",
        plot.background = element_rect("#E5F5FF"),
        panel.grid = element_line(colour = "grey50",
                                  linetype = "dashed"),
        text = element_text(family = "Source Sans Pro Black")) +
  geom_text(data = canciones %>% 
              left_join(caract, "id") %>% 
              group_by(album) %>% 
              summarise(mean = median(valence)),
            aes(x = mean, y = c(1:4 + 0.75, 5+0.8), 
                label = paste0("Mediana = ",round(mean,2))),
            family = "Source Sans Pro Black")

ggsave("arcade.png", dpi = 300, width = 11.2, height = 8)

#Radiohead

View(spotifyr::search_spotify("Radiohead", "artist"))

albums <- spotifyr::get_artist_albums("4Z8W4fKeB5YxbusRsdQVPb", "album")$id
canciones <- do.call(rbind.data.frame,
                     lapply(1:length(albums),
                            function(i) data.frame(get_album_tracks(albums[i])[,c("name","id")],
                                                   album = get_album(albums[i])$name,
                                                   fecha = get_album(albums[i])$release_date)))

canciones <- canciones %>%
  filter(album %in% c("Pablo Honey",
                      "The Bends",
                      "OK Computer",
                      "Kid A",
                      "Amnesiac",
                      "Hail To the Thief",
                      "In Rainbows",
                      "The King Of Limbs",
                      "A Moon Shaped Pool"))

caract <- get_track_audio_features(canciones$id[1:100])
caract[101:nrow(canciones),] <- get_track_audio_features(canciones$id[101:nrow(canciones)])

canciones %>% left_join(caract, "id") %>%
  ggplot(aes(x = acousticness, y = album, fill = ..x..)) +
  geom_density_ridges_gradient() +
  theme_minimal(base_size = 16) +
  scale_x_continuous(breaks = seq(0, 1, by = 0.2)) +
  labs(x = "Índice 'acousticness'\n(0 = ninguna confianza en que la canción es acústica, 1 = total confianza)",
       y = "Álbum",
       title = "Instrumentalidad de las canciones de Radiohead a través de sus álbums",
       subtitle = "Fuente: API de Spotify accedida a través del paquete {spotifyr}.\nDesafío #30díasdegráficos con R de @R4DS_es, día 7.",
       caption = "@Picanumeros") +
  scale_fill_gradientn(colours = c("#12519e", "#54a1cf", "#84aebe", "#96cedf", "#f8fdff")) +
  theme(legend.position = "none",
        panel.grid = element_line(colour = "grey50",
                                  linetype = "dashed"),
        text = element_text(family = "Franklin Gothic Medium")) +
  geom_text(data = canciones %>%
              left_join(caract, "id") %>%
              group_by(album) %>%
              summarise(mean = median(acousticness)),
            aes(x = mean, y = c(1.4, 2.25, 3.25,
                                4.5, 5.5, 6.35,
                                7.5, 8.75, 9.75),
                label = paste0("Mediana = ",round(mean,2))),
            family = "Franklin Gothic Medium")

ggsave("radio.png", dpi = 300, width = 11.2, height = 8)
