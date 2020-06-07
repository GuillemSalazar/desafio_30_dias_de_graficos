library(tidyverse)
library(gganimate)
library(gifski)

# Parámetros
pais_europe<-c("Bélgica","Dinamarca","Alemania","Irlanda","Grecia","España","Francia","Italia","Luxemburgo","Países Bajos","Austria","Polonia","Portugal","Finlandia","Suecia","Reino Unido")
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
  filter(pais_nombre %in% pais_europe)

g<-ggplot(data=toplot,aes(x=2,y=porcentaje,fill=serie)) +
  geom_bar(stat = "identity",color="white") +
  coord_polar(theta = "y", start = 0) +
  xlim(0.5, 3) +
  scale_fill_manual(values = c("#4daf4a","#e41a1c","#377eb8","#984ea3")) +
  transition_states(states = anio) +
  facet_wrap(~pais_nombre,nrow = 3) +
  theme_void() +
  theme(legend.position = "bottom",
        legend.direction = "horizontal",
        legend.key.size = unit(0.2,"cm"),
        legend.text = element_text(size=6),
        legend.title = element_blank(),
        plot.caption = element_text(size=5,hjust=0),
        plot.margin = unit(c(1,4,4,1),"lines"),
        plot.title = element_text(hjust=0.5,vjust=2),
        plot.subtitle = element_text(hjust=0.5,vjust=2)) +
  labs(title="Producción de electricidad en la UE según origen",
       subtitle="Año: {closest_state}",
       caption = "twitter: @GuillemSalazar\nCódigo: https://github.com/GuillemSalazar/desafio_30_dias_de_graficos")


fps<-10
nframes<-200
end_pause<-fps*3
animation<-animate(g, nframes=nframes, fps = fps,end_pause = end_pause,height = 6.5*2, width = 13*2,units="cm",res = 200,renderer = gifski_renderer())
anim_save(animation = animation,filename = "../images/dia_27.gif")
 
