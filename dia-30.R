library(tidyverse)  #Preprocesamiento
library(extrafont)  #Uso de fuentes de Windows en ggplot
install.packages("HistData")
library(HistData)   #Paquete donde se encuentra el dataset con el que trabajó Florence Nightingale
library(patchwork)  #Para combinar varios ggplots en uno

dat <- Nightingale

#Añadimos una variable que almacenará el valor máximo 
dat$maximo <- apply(dat[,c("Disease", "Wounds", "Other")], 1, max)

g2 <- dat %>% filter(Date < "1855-03-02") %>%
  ggplot(aes(x = Date)) + 
  geom_bar(aes(y = Disease), stat = "identity", fill = "#C0C2F0", width = 31, col = "grey") +
  geom_bar(aes(y = Other), stat = "identity", fill = "#56565B", width = 31, col = "grey") +
  geom_bar(aes(y = Wounds), stat = "identity", fill = "#FFCCCC", width = 31, col = "grey") +
  scale_y_sqrt() +
  coord_polar(start = -pi/2) +
  geom_text(aes(y = ifelse(maximo < 20, 300, maximo + 200),
                label = c("APRIL\n1854", "MAY", "JUNE",
                          "JULY", "AUGUST", "SEPTEMBER", "OCTOBER",
                          "NOVEMBER", "DECEMBER", "JANUARY 1855",
                          "FEBRUARY", "MARCH 1855."), family = "Bahnschrift"),
            angle = seq(75, -285 + 360/12, by = -30), size = 3) +
  annotate("text", x = as.Date("1854-06-08"), y = 100,
           label = "BULGARIA", angle = 90, size = 3, family = "Bahnschrift") +
  theme_void(base_size = 12) +
  labs(title = "1.\nAPRIL 1854 to MARCH 1855") +
  theme(plot.title = element_text(hjust = 0.6,
                                  family = "Garamond"))

g1 <- dat %>% filter(Date > "1855-03-02") %>%
  ggplot(aes(x = Date)) + 
  geom_bar(aes(y = Disease), stat = "identity", fill = "#C0C2F0", width = 31, col = "grey") +
  geom_bar(aes(y = Wounds), stat = "identity", fill = "#FFCCCC", width = 31, col = "grey") +
  geom_bar(aes(y = Other), stat = "identity", fill = "#56565B", width = 31, col = "grey") +
  scale_y_sqrt() +
  coord_polar(start = -pi/2) +
  geom_text(aes(y = ifelse(maximo < 20, 300, maximo + 150),
                label = c("APRIL\n1855", "MAY", "JUNE",
                          "JULY", "AUGUST", "SEPTEMBER", "OCTOBER",
                          "NOVEMBER", "DECEMBER", "JANUARY\n1856",
                          "FEBRUARY", "MARCH")), size = 2.5,
            angle = seq(75, -285 + 360/12, by = -30), family = "Bahnschrift") +
  theme_void(base_size = 12) +
  labs(title = "2.\nAPRIL 1855 to MARCH 1856") +
  theme(plot.title = element_text(hjust = 0.6, 
                                  family = "Garamond"))

texto <- "The Areas of the blue, red & black wedges are each measured from
    the centre as the common vertex.
The blue wedges measured from the centre of the circle represent area
    for area the deaths from Preventible or Mitigable Zymotic diseases; the
    red wedges measured from the centre the deaths from wounds; & the
    black wedges measured from the centre the deaths from all other causes.
The black line across the red triangle in Nov. 1854 marks the boundary
    of the deaths from all other causes during the month.
In October 1854, & April 1855; the black area coincides with the red;
    in January & February 1856, the blue coincides with the black.
The entire areas may be compared by following the blue, the red & the
    black lines enclosing them."

g3 <- ggplot(data.frame(x = 0, y = 0),
       aes(x = x, y = y)) + 
  scale_x_continuous(limits = c(-5, -4)) +
  theme_void() + annotate("text", x = -5, y = 0, label = texto, hjust = 0, 
                          family = "Lucida Calligraphy", size = 4)

g4 <- ggplot() + theme_void() + annotate("text", x = 0, y = 0,
                                         label = "Fuente: librería {HistData} de R (dataset 'Nightingale')\nDesafío #30díasdegráficos con R de @R4DS_es, día 30 (y último).\n@Picanumeros")

g1 + g2 + g3 + g4 +
  plot_layout(ncol = 2,
              heights = c(150,50)) + 
  plot_annotation(title = "DIAGRAM OF THE CAUSES OF MORTALITY",
                  subtitle = "IN THE ARMY IN THE EAST",
                  theme = theme(plot.title = element_text(hjust = 0.5, size = 18,
                                                     family = "Lucida Calligraphy"),
                                plot.subtitle = element_text(hjust = 0.5, size = 16,
                                                             family = "Bahnschrift")))
  
  

ggsave("dia30.png", dpi = 300, width = 14, height = 12)
