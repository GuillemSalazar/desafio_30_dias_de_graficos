library(marmap)
library(tidyverse)
library(viridis)


# Parámetros
twitter_dim<-list(width=unit(13/1.5,"cm"),height=unit(6.5/1.5,"cm"))

datos<-readGEBCO.bathy(file = "../datos/gebco_2020_n43.68_s41.666_w-2.0_e3.2.nc") # Source: # https://download.gebco.net
datos_xyz<-as.xyz(datos) %>%
  rename(x="V1",y="V2",z="V3")

# Graficar
g<-ggplot(data=datos_xyz %>% slice(which(row_number() %% 4 == 0)),aes(x=x,y=y,z=z,color=after_stat(level))) +
  geom_contour(size=0.2) +
  coord_fixed() +
  scale_color_viridis(option = "plasma") +
  theme_void() +
  theme(legend.position = "bottom",legend.justification = "center",
        legend.key.height = unit(0.2,"cm"),
        legend.key.width = unit(2,"cm"),legend.title = element_text(hjust=0,vjust=1),
        plot.title = element_text(hjust = 0.1,vjust = 2)) +
  labs(title="Mapa topográfico de los Pirineos",caption=element_text("twitter: @GuillemSalazar"),color="Altitud (m)")

# Guardar gráfico
ggsave(filename = "../images/dia_08.png",width = twitter_dim$width,height = twitter_dim$height)

