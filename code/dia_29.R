library(tidyverse)
library(GGally)
library(ggrepel)
library(ggsci)

# Parámetros
twitter_dim<-list(width=unit(13/1.5,"cm"),height=unit(6.5/1.5,"cm"))
pais_europe<-c("Bélgica","Dinamarca","Alemania","Irlanda","Grecia","España","Francia","Italia","Países Bajos","Austria","Polonia","Portugal","Finlandia","Suecia","Reino Unido")
to_keep<-c("Producción de electricidad a partir de fuentes de petróleo, gas y carbón (% del total)",
           "Producción de electricidad a partir de fuentes hidroeléctricas (% del total)",
           "Producción de electricidad a partir de fuentes nucleares (% del total)",
           "Producción de electricidad a partir de fuentes renovables, excluida la hidroeléctrica (% del total)")

# Cargar datos
datos<-read_tsv("../datos/Data_Extract_From_Indicadores_del_desarrollo_mundial/ef3e3e9f-c747-47c8-b39b-11f0c4df02b2_Data.txt",na = "..")

# Limpiar datos
datos<-datos %>%
  select(pais_nombre="país Name",pais_codigo="país Code",serie="serie Name","1960 [YR1960]":"2015 [YR2015]") %>%
  pivot_longer(cols = contains("YR"),names_to = "anio",values_to = "porcentaje") %>%
  separate(anio,into=c("anio","anio_fct"),sep=" ") %>%
  filter(serie %in% to_keep) %>%
  mutate(serie=str_wrap(serie,40)) %>%
  mutate(anio=as.numeric(anio)) %>%
  na.exclude()

toplot<-datos %>%
  filter(pais_nombre %in% pais_europe) %>%
  filter(anio==2015) %>%
  pivot_wider(names_from = "serie",values_from = "porcentaje")


g<-ggparcoord(data = toplot,columns = 5:8, groupColumn = 1,scale = "globalminmax",order = "skewness") +
  geom_text_repel(data=toplot,aes(x=4,y=`Producción de electricidad a partir de\nfuentes de petróleo, gas y carbón (% del\ntotal)`,label=pais_nombre),inherit.aes = F,nudge_x = 0.1,hjust=0,size=2,segment.color = "gray80") +
  #scale_color_d3(palette = "category20b") +
  scale_color_simpsons() +
  theme_minimal() +
  ylab("Porcentaje del total (%)") +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_text(size=6,color="black"),
        axis.text.y = element_text(size=6,color="black"),
        legend.position = "none",
        plot.caption = element_text(hjust=0,size=4)) +
  labs(title="Producción de electricidad en la UE según origen",
       subtitle="Año 2015",
       caption = "twitter: @GuillemSalazar\nCódigo: https://github.com/GuillemSalazar/desafio_30_dias_de_graficos")

# Guardar gráfico
ggsave(filename = "../images/dia_29.png",g,width = twitter_dim$width,height = twitter_dim$height)

