library(sf)
library(tidyverse)
library(ggalluvial)

# Parámetros
twitter_dim<-list(width=unit(13/1.5,"cm"),height=unit(6.5/1.5,"cm"))

# Cargar datos
shp_df<-st_read("../datos/enp/enp.shp") # Source: https://www.miteco.gob.es/es/cartografia-y-sig/ide/descargas/biodiversidad/enp.aspx)

# Limpiar datos
toplot<-shp_df %>%
  as.data.frame() %>%
  filter(FIGURA_LP %in% c("Parque Nacional")) %>%
  select(CCAA_N_ENP,SITENAME,YEAR,AREA_HA) %>%
  mutate(Freq=1) %>%
  mutate(YEAR=as.numeric(as.character(YEAR))) %>%
  mutate(CCAA_N_ENP=fct_reorder(CCAA_N_ENP,YEAR,mean)) %>%
  mutate(CCAA_N_ENP=str_wrap(CCAA_N_ENP,20)) %>%
  mutate(SITENAME=gsub("Maritimo-Terrestre de las","",SITENAME))
  
# Graficar
g<-ggplot(data=toplot,aes(y=sqrt(AREA_HA),axis1=CCAA_N_ENP,axis2=YEAR,axis3=SITENAME)) +
  geom_alluvium(col="white",fill="gray30",knot.pos = 0.3,alpha=0.3) +
  geom_stratum(col="white",fill="#ba001f",width = c(rep(1/3,length(unique(toplot$CCAA_N_ENP))),rep(1/10,length(unique(toplot$YEAR))),rep(1/3,length(unique(toplot$SITENAME))))) +
  geom_text(stat = "stratum", infer.label = TRUE,size=1.6,col="white") +
  scale_x_continuous(breaks=1:3,labels = c("CCAA","Año de creación","Parque Nacional")) +
  theme_minimal() +
  ylab("") +
  labs(title = "Parques Nacionales en España",
       subtitle="Tamaño proporcional a la superficie del parque",
       caption = "twitter: @GuillemSalazar\nCódigo: https://github.com/GuillemSalazar/desafio_30_dias_de_graficos") +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        axis.ticks.x = element_line(size=0.2),
        axis.text.x = element_text(color="black",size=6),
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        plot.caption = element_text(size=5),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank())
  
# Guardar gráfico
ggsave(filename = "../images/dia_17.png",g,width = twitter_dim$width,height = twitter_dim$height)
 
