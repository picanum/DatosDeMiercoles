library(tidyverse)
library(extrafont)    #Estos dos paquetes son casi de la familia
library(treemapify)   #Este paquete nos permitira hacer el treemap
library(RColorBrewer) #Y este para las paletas de color

#Los datos se deben descargar de la primera tabla del siguiente enlace: https://www.epdata.es/datos/presupupuestos-generales-estado-pge-evolucion-datos-estadisticas/60
dat <- read.csv("gastos_previstos_en_los_p.csv", sep = ";", encoding = "UTF-8")

#Cambiamos nombres de columnas a algo mas manejable
colnames(dat) <- c("anio","unit","polit","eur")

#Quitamos la notacion 'española' a los numeros y pasamos la variable a numerica
dat$eur <- as.numeric(as.character(str_replace(str_remove(dat$eur, "\\."), ",", "\\.")))

#Al hacer el treemap, se necesita que el texto quepa en todos los recuadros. Sin embargo, la funcion no esta preparada para ajustar el texto.
#Lo tenemos que hacer manualmente, definiendo de nuevo los niveles y poniendo el salto de linea alla donde lo veamos conveniente.
levels(dat$polit) <- c(""                                                  
                       ,"Acceso a la Vivienda y\nFomento de la Edificación"  
                       ,"Administración Financiera\ny Tributaria"            
                       ,"Agricultura, Pesca y\nAlimentación"                 
                       ,"Alta Dirección"                                    
                       ,"Comercio, Turismo y\nP.Y.M.E.S."                    
                       ,"Cultura"                                           
                       ,"Defensa"                                           
                       ,"Desempleo"                                         
                       , "Deuda Pública"                                     
                       , "Educación"                                         
                       , "Fomento del empleo"                                
                       , "Gestión y Administración\nde la Seguridad Social"   
                       , "Industria y Energía"                               
                       , "Infraestructuras"                                  
                       , "Investigación civil"                               
                       , "Justicia"                                          
                       , "Otras actuaciones de\ncarácter económico"           
                       , "Otras Prestaciones\nEconómicas"                     
                       , "Pensiones"                                         
                       , "Política Exterior"                                 
                       , "Sanidad"                                           
                       , "Seguridad ciudadana e\nInstituciones penitenciarias"
                       , "Servicios de carácter\ngeneral"                     
                       , "Servicios Sociales y\nPromoción Social"             
                       , "Subvenciones al transporte"                        
                       , "Transferencias a otras\nAdministraciones Públicas" )


dat %>% filter(anio == 2019) %>%        #Nos quedamos con los presupuestos de 2019 (los ultimos disponibles)
  ggplot(aes(area = eur, fill = polit,  #Para el geom_treemap necesitamos los aesthetics area y label (para el texto de los recuadros)
             label = paste0(polit, "\n(", eur, " mill. €)"))) + 
  geom_treemap(layout = "srow") +         #Con 'srow' especificamos que queremos esparcir los recuadros por filas
  geom_treemap_text(layout = "srow",
                    family = "Verdana") +
  scale_fill_manual(values = colorRampPalette(brewer.pal(7, "BuPu"))(26)) +  #Como hay muchos recuadros y la paleta no tiene tantos colores,
                                                                             #podemos usar colorRampPalette para ampliar la paleta mediante
                                                                             #interpolacion entre los que hay.
  theme(legend.position = "none",
        text = element_text(family = "Verdana", size = 14)) + 
  labs(title = "Gastos dedicados a cada política previstos en los Presupuestos Generales del Estado 2019 de España",
       subtitle = "Fuente: Ministerio de Hacienda, EP Data (https://www.epdata.es/datos/presupupuestos-generales-estado-pge-evolucion-datos-estadisticas/60)\nDesafío #30díasdegráficos con R de @R4DS_es, día 14 | @Picanumeros")

ggsave("dia14.png", dpi = 300, width = 15.5, height = 12)
