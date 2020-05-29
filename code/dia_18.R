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

# Graficar
# g<-ggplot(data=toplot,aes(y=sqrt(AREA_HA),axis1=CCAA_N_ENP,axis2=YEAR,axis3=SITENAME)) +
#   geom_alluvium(col="white",fill="#7e41bc",knot.pos = 0.3,alpha=0.3) +
#   geom_stratum(col="white",fill="#7fbc41",width = c(rep(1/3,length(unique(toplot$CCAA_N_ENP))),rep(1/10,length(unique(toplot$YEAR))),rep(1/3,length(unique(toplot$SITENAME))))) +
#   geom_text(stat = "stratum", infer.label = TRUE,size=1.6,col="black") +
#   scale_x_continuous(breaks=1:3,labels = c("CCAA","Año de creación","Parque Nacional")) +
#   theme_minimal() +
#   ylab("") +
#   labs(title = "Parques Nacionales en España",
#        subtitle="Tamaño proporcional a la superficie del parque",
#        caption = "twitter: @GuillemSalazar\nCódigo: https://github.com/GuillemSalazar/desafio_30_dias_de_graficos") +
#   theme(panel.grid.major.x = element_blank(),
#         panel.grid.minor.x = element_blank(),
#         panel.grid.major.y = element_blank(),
#         panel.grid.minor.y = element_blank(),
#         axis.ticks.x = element_line(size=0.2),
#         axis.text.x = element_text(color="black",size=6),
#         plot.title = element_text(hjust = 0.5),
#         plot.subtitle = element_text(hjust = 0.5),
#         plot.caption = element_text(size=5),
#         axis.ticks.y = element_blank(),
#         axis.text.y = element_blank())

# Guardar gráfico
ggsave(filename = "../images/dia_18.png",g,width = twitter_dim$width,height = twitter_dim$height)

