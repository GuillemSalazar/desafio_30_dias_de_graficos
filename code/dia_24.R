library(sf)
library(tidyverse)
library(ggalluvial)
library(viridis)

# Parámetros
twitter_dim<-list(width=unit(13/1.5,"cm"),height=unit(6.5/1.5,"cm"))

# Cargar datos
shp_df<-st_read("../datos/series_p_tcm30-199924/Series_p.shp") # Source: http://centrodedescargas.cnig.es/CentroDescargas/catalogo.do?Serie=CAANE

aux_data<-tibble(PISO=c("A","B","C","D","E","F","G","H","I","K","L","M","N","O","-"),
                 PISO_name=c("Piso alpino","Piso subalpino","Piso montano","Piso colino","Piso crioromediterraneo","Piso oromediterraneo","Piso supramediterraneo","Piso mesomediterraneo","Piso termomediterraneo","Piso orocanario","Piso supracanario","Piso mesocanario","Piso termocanario","Piso infracanario","Ninguno")) %>%
  mutate(PISO_name=gsub("\\-. ","",paste(PISO,". ",PISO_name,sep="")))

# Limpiar datos
toplot<-shp_df
toplot<-toplot %>%
  left_join(aux_data,by="PISO")

g<-ggplot() +
  geom_sf(data=toplot,aes(fill=PISO_name),color=NA) +
  scale_fill_viridis(discrete = T,na.value = "grey50") +
  theme_minimal() +
  labs(title = "Mapa de Pisos de Vegetación de España",
       subtitle="Se denomina piso de vegetación a la estratificación vegetal\nen función de la altitud en ecosistemas montañosos",
       caption = "twitter: @GuillemSalazar\nCódigo: https://github.com/GuillemSalazar/desafio_30_dias_de_graficos") +
  theme(plot.title = element_text(hjust=0.5),
        plot.subtitle = element_text(hjust=0.5,size=6),
        legend.title = element_blank())

# Guardar gráfico
ggsave(filename = "../images/dia_24.png",g,width = twitter_dim$width,height = twitter_dim$height,dpi=700)

