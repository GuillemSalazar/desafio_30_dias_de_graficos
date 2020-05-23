library(tidyverse)

# Parámetros
twitter_dim<-list(width=unit(13/1.5,"cm"),height=unit(6.5/1.5,"cm"))
pais_europe<-c("Bélgica","Bulgaria","República Checa","Dinamarca","Alemania","Estonia","Irlanda","Grecia","España","Francia","Croacia","Italia","Chipre","Letonia","Lituania","Luxemburgo","Hungría","Malta","Países Bajos","Austria","Polonia","Portugal","Rumania","Eslovenia","Eslovaquia","Finlandia","Suecia","Reino Unido")

# Cargar datos
co2 <- readr::read_csv("https://raw.githubusercontent.com/cienciadedatos/datos-de-miercoles/master/datos/2019/2019-08-21/co2.csv")

# Limpiar datos
co2<-co2 %>%
  filter(anio %in% c(1990,2014)) %>%
  filter(!is.na(emision_co2)) %>%
  pivot_wider(names_from = "anio",values_from = emision_co2,names_prefix = "emision_co2_") %>%
  mutate(emision_co2_dif=100*(emision_co2_2014-emision_co2_1990)/emision_co2_1990) %>%
  filter(!is.na(emision_co2_dif)) %>%
  filter(pais_region %in% pais_europe) %>%
  arrange(desc(emision_co2_dif)) %>%
  mutate(pais_region=fct_relevel(pais_region,pais_region))
  
co2_annot<-co2 %>% filter(codigo_iso=="LUX")

# Graficar
g<-ggplot(data=co2) +
  geom_segment(aes(x=pais_region,xend=pais_region,y = emision_co2_1990,yend=emision_co2_2014),col="grey60") +
  geom_point(aes(x=pais_region,y=emision_co2_1990),col="#d8b365",size=5,alpha=0.6) +
  geom_point(aes(x=pais_region,y=emision_co2_2014),col="#5ab4ac",size=5,alpha=0.6) +
  annotate(geom="text",x = 19,y = 24,label="Año 1990",hjust=1.5,size=3) +
  annotate(geom="text",x = 15,y = 20,label="Año 2014",hjust=0,size=3) +
  annotate(geom = "curve", x = 18.3, y = co2_annot$emision_co2_1990, xend = 19, yend = 23.5,curvature = 0.3, arrow = arrow(length = unit(1, "mm")),color="#d8b365") +
  annotate(geom = "curve", x = 15.7, y = co2_annot$emision_co2_2014, xend = 15, yend = 19.5,curvature = 0.3, arrow = arrow(length = unit(1, "mm")),color="#5ab4ac") +
  coord_flip(clip = "off",ylim = c(0,27)) +
  theme_minimal() +
  theme(plot.margin = unit(c(1,1,1,1),"lines")) +
  ylab("Emisión de CO2 (toneladas métricas per cápita)") +
  xlab("") +
  labs(title="Cambio en las emisiones de CO2 en la Unión Europea",
       subtitle="Emisiones en 1990 y 2014",
       caption = "Código: https://github.com/GuillemSalazar/desafio_30_dias_de_graficos\ntwitter: @GuillemSalazar")

# Guardar gráfico
ggsave(filename = "../images/dia_12.png",g,width = twitter_dim$width,height = twitter_dim$height)
