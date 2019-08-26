library(dplyr)
library(ggplot2)
library(WDI)
library(ggthemes)
library(patchwork)

co2 <- readr::read_csv("https://raw.githubusercontent.com/cienciadedatos/datos-de-miercoles/master/datos/2019/2019-08-21/co2.csv")
co2_ingreso <- readr::read_csv("https://raw.githubusercontent.com/cienciadedatos/datos-de-miercoles/master/datos/2019/2019-08-21/co2_ingreso.csv")

pobl <- WDI(country = "WLD", indicator = "SP.POP.TOTL", 
            start = min(co2$anio), end = max(co2$anio))

diff_coc <- function(x) return(x[-1]/x[-length(x)])

arrow = arrow(angle=10)

g1 <- co2 %>% filter(pais_region == "Mundo") %>% 
  mutate(year = anio) %>% left_join(pobl, by = "year") %>%
  ggplot(aes(x = anio, y = emision_co2*SP.POP.TOTL/1000000)) + geom_line(size = 1.05) +
  theme_bw(base_size = 14) + 
  labs(title = "Evolución de la emisión de CO2 anual mundial (en millones de toneladas)",
       subtitle = "Fuente: Banco Mundial. Recopilado de data.world y traducido por @R4DS_es (#DatosDeMiercoles semana 21-08-2019)",
       x = "", y = "") +
  scale_x_continuous(breaks = seq(1960,2015,by=5)) +
  scale_y_continuous(breaks = seq(0,40000,by=5000),
                     labels = paste(seq(0,40000,by=5000),"M ton.", sep="")) +
  annotate("segment", x = 2012, xend = 2014, y = 27500, yend = 35000, arrow=arrow) +
  annotate("text", x = 2012, y = 20000, label = "En 2014 las emisiones\nmundiales de CO2\nfueron de más de\n36.000 millones\nde toneladas", size=4)

g2 <- co2 %>% filter(pais_region == "Mundo") %>% 
  mutate(year = anio) %>% left_join(pobl, by = "year") %>%
  mutate(dif_emis = c(NA, diff(emision_co2*SP.POP.TOTL/1000000))) %>%
  ggplot(aes(x = anio, y = dif_emis)) + geom_line(size = 1.05) + 
  geom_hline(yintercept = 0) + stat_smooth(se = F) + 
  theme_bw(base_size = 14) + 
  labs(title = "Diferencias interanuales en la emisión de CO2 anual mundial (en millones de toneladas)",
       subtitle = "Fuente: Banco Mundial. Recopilado de data.world y traducido por @R4DS_es (#DatosDeMiercoles semana 21-08-2019)",
       x = "", y = "",
       caption = "@Picanumeros") +
  scale_x_continuous(breaks = seq(1960,2015,by=5)) +
  scale_y_continuous(breaks = seq(-500,2000,by=250),
                     labels = paste(seq(-500,2000,by=250),"M ton.", sep="")) +
  annotate("segment", x = 1970, xend = 1974.25, y = -500, yend = -60, arrow=arrow) +
  annotate("segment", x = 1970, xend = 1980, y = -500, yend = -400, arrow=arrow) +
  annotate("text", x = 1965, y = -500, label = "Crisis del petróleo", size=5) +
  annotate("segment", x = 2005, xend = 2008.5, y = -425, yend = -250, arrow=arrow) +
  annotate("text", x = 2002, y = -375, label = "Crisis financiera\nde 2008", size=5)

g1 / g2 + plot_layout(height = c(150, 200))
