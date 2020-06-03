library(tidyverse)
library(viridis)

# Parametros
url<-"https://raw.githubusercontent.com/cienciadedatos/datos-de-miercoles/master/datos/2019/2019-07-31/la_casa_de_papel.csv"
personajes<-c("Tokio","Lisboa","Profesor","Moscú","Berlín","Nairobi","Río","Denver","Estocolmo","Helsinki","Bogotá","Palermo","Marsella")
twitter_dim<-list(width=unit(13/1.5,"cm"),height=unit(6.5/1.5,"cm"))


# Cargar datos
datos<-read_csv(url)
personajes_occ<-sapply(personajes,function(x){grepl(x,datos$texto,)}) %>% as_tibble()

# Limpiar datos
datos<-datos %>%
  mutate(temporada=paste("S",str_pad(string = temporada,width = 2,pad = 0),sep=""),
         episodio=paste("E",str_pad(string = episodio,width = 2,pad = 0),sep=""),
         temporada_episodio=paste(temporada,episodio)) %>%
  bind_cols(personajes_occ) %>%
  select(episodio:Marsella) %>%
  pivot_longer(Tokio:Marsella,names_to = "personaje",values_to = "presente") %>%
  mutate(presente=as.numeric(presente)) %>%
  group_by(episodio,temporada,temporada_episodio,personaje,.drop=F) %>%
  summarise(n=sum(presente)) %>%
  mutate(personaje=fct_reorder(personaje,n,sum,.desc = T))
  

toplot_main<-datos %>%
  group_by(temporada,personaje) %>%
  summarise(n=sum(n)) %>%
  mutate(temporada_personaje=paste(temporada,personaje)) %>%
  ungroup() 

toplot_inner_ring<-toplot_main %>%
  group_by(personaje) %>%
  summarise(n=sum(n)) %>%
  ungroup() %>%
  arrange(desc(personaje)) %>%
  mutate(lab.ypos = cumsum(n) - 0.5*n) %>%
  mutate(label=ifelse(n<5,NA,as.character(personaje)))
toplot_outer_ring<-toplot_main %>%
  group_by(temporada_personaje,temporada,personaje) %>%
  summarise(n=sum(n)) %>%
  ungroup() %>%
  arrange(desc(personaje,temporada)) %>%
  mutate(lab.ypos = cumsum(n) - 0.5*n) %>%
  mutate(label=ifelse(n<5,NA,temporada))

# Graficar
g<-ggplot() +
  geom_bar(data = toplot_inner_ring,aes(x=5,y=n,fill=personaje),stat = "identity",color = "white",width=1,alpha=0.7) +
  geom_bar(data = toplot_outer_ring,aes(x=6,y=n,fill=personaje),stat = "identity",color = "white",width=1,alpha=0.7) +
  geom_text(data=toplot_inner_ring,aes(x=5,y = lab.ypos, label = label),size=2.5,fontface="bold",hjust=0.5) +
  geom_text(data=toplot_outer_ring,aes(x=6,y = lab.ypos, label = label),size=2,fontface="bold",hjust=0.5) +
  geom_text(aes(x=0,y=0,label=str_wrap("Menciones a cada personaje en La Casa de Papel",30))) +
  coord_polar(theta = "y", start = 0) +
  xlim(0, 7) +
  scale_fill_viridis(option = "plasma",discrete = T) +
  theme_void() +
  theme(plot.margin = margin(t = 0.1,r = 0.1,b = 0.1,l =0.1,"cm"),
        plot.title = element_text(vjust = 3),
        plot.subtitle = element_text(vjust = 3),
        legend.position = "none",
        legend.title = element_blank(),
        strip.text = element_text(size=6)) +
  labs(caption = "Código: https://github.com/GuillemSalazar/desafio_30_dias_de_graficos\ntwitter: @GuillemSalazar")

# Guardar gráfico
ggsave(filename = "../images/dia_23.png",g,width = twitter_dim$width,height = twitter_dim$height)

