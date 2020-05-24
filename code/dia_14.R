library(sf)
library(tidyverse)
library(treemapify)
library(viridis)

# Parámetros
twitter_dim<-list(width=unit(13/1.5,"cm"),height=unit(6.5/1.5,"cm"))

# Cargar datos
shp_df<-st_read("../datos/enp/enp.shp") # Source: https://www.miteco.gob.es/es/cartografia-y-sig/ide/descargas/biodiversidad/enp.aspx)

# Limpiar datos
pn<-shp_df %>%
  filter(FIGURA_LP=="Parque Nacional") %>%
  mutate(label=paste(SITENAME," (",round(AREA_HA/100,1)," km2)",sep=""))

# Graficar
g<-ggplot(data=pn,aes(area=AREA_HA,fill=CCAA_N_ENP,label=str_wrap(label,20))) +
  geom_treemap(color="black") +
  geom_treemap_text(min.size = 3) +
  scale_fill_manual(name="",values = c("#8dd3c7","#ffffb3","#bebada","#fb8072","#80b1d3","#fdb462","#b3de69","#fccde5","#d9d9d9","#bc80bd")) +
  theme(legend.position = "none") +
  labs(title="Parques nacionales de España",
       caption = "twitter: @GuillemSalazar\nCódigo: https://github.com/GuillemSalazar/desafio_30_dias_de_graficos")

# Guardar gráfico
ggsave(filename = "../images/dia_14.png",g,width = twitter_dim$width,height = twitter_dim$height)

