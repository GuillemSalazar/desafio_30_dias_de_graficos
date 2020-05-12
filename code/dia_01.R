library(tidyverse)
library(ggthemes)

url<-"https://raw.githubusercontent.com/cienciadedatos/datos-de-miercoles/master/datos/2019/2019-07-31/la_casa_de_papel.csv"
personajes<-c("Tokio","Lisboa","Profesor","Moscú","Berlín","Nairobi","Río","Denver","Estocolmo","Helsinki","Bogotá","Palermo","Marsella")
colores<-rev(viridis(23))
twitter_dim<-list(width=unit(13,"cm"),height=unit(6.5,"cm"))

datos<-read_csv(url)
personajes_occ<-sapply(personajes,function(x){grepl(x,datos$texto,)}) %>% as_tibble()

datos<-datos %>%
  mutate(temporada_episodio=paste("S",str_pad(string = temporada,width = 2,pad = 0),"E",str_pad(string = episodio,width = 2,pad = 0),sep="")) %>%
  bind_cols(personajes_occ) %>%
  select(episodio:Marsella) %>%
  pivot_longer(Tokio:Marsella,names_to = "personaje",values_to = "presente") %>%
  mutate(presente=as.numeric(presente)) %>%
  group_by(episodio,temporada,temporada_episodio,personaje,.drop=F) %>%
  summarise(n=sum(presente)) %>%
  mutate(personaje=fct_reorder(personaje,n,sum,.desc = T))

g<-ggplot(data = datos,aes(x=personaje,y=n,fill=temporada_episodio)) +
  geom_bar(stat="identity",position = position_dodge()) +
  theme_hc() +
  scale_fill_manual(values=colores) +
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        legend.key.size = unit(5,"mm"),
        panel.grid.major.y = element_line(color="gray",size=0.3),
        axis.ticks = element_blank(),axis.text = element_text(color="black")) +
  guides(fill = guide_legend(nrow = 2)) +
  ylab("Número de menciones") +
  xlab("") +
  labs(title="Menciones por personaje en La Casa de Papel",subtitle="Temporadas 1-3",caption=element_text("twitter: @GuillemSalazar"))

ggsave(filename = "dia_1.png",g,width = twitter_dim$width,height = twitter_dim$height)

