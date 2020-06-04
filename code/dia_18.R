library(sf)
library(tidyverse)
library(ggalluvial)
library(ggrepel)

# Parámetros
twitter_dim<-list(width=unit(13/1.5,"cm"),height=unit(6.5/1.5,"cm"))

# Cargar datos
shp_df<-st_read("../datos/enp/enp.shp") # Source: https://www.miteco.gob.es/es/cartografia-y-sig/ide/descargas/biodiversidad/enp.aspx


# Limpiar datos
get_range<-function(x){apply(apply(x[[1]][[1]],2,range),2,mean)}

toplot<-shp_df %>%
  filter(FIGURA_LP %in% c("Parque Nacional","Parque Natural")) %>%
  filter(CCAA_N_ENP!="Canarias")
toplot<-toplot %>%
  bind_cols(data.frame(t(sapply(toplot$geometry,get_range)))) %>%
  mutate(SITENAME=str_wrap(SITENAME,30))

world_map <- map_data("world")
world_map<-world_map %>% filter(region %in% c("Spain"))

g<-ggplot() +
  geom_polygon(data=world_map,aes(x = long, y = lat, group = group),fill="lightgray", colour = "white") +
  geom_sf(data=toplot,col=NA,aes(fill=FIGURA_LP),alpha=0.5) +
  geom_text_repel(data=toplot %>% as.data.frame(),aes(x=X1,y=X2,label=SITENAME,col=FIGURA_LP),size=1.5,segment.size = 0.1,force = 2) +
  theme_minimal() +
  scale_color_manual(values = c("#C0504D","#366092")) +
  scale_fill_manual(values = c("#C0504D","#366092")) +
  theme(legend.position = "right",
        axis.text = element_blank(),
        axis.title = element_blank(),
        legend.title = element_blank()) +
  labs(title = "Parques Nacionales y Naturales en la España peninsular",
               caption = "twitter: @GuillemSalazar\nCódigo: https://github.com/GuillemSalazar/desafio_30_dias_de_graficos") +
  guides(colour=FALSE)


# Guardar gráfico
ggsave(filename = "../images/dia_18.png",g,width = twitter_dim$width,height = twitter_dim$height)

