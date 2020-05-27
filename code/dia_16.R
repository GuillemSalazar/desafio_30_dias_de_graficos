library(sf)
library(tidyverse)
library(waffle) # devtools::install_github("hrbrmstr/waffle")

# Parámetros
twitter_dim<-list(width=unit(13/1.5,"cm"),height=unit(6.5/1.5,"cm"))

# Cargar datos
shp_df<-st_read("../datos/enp/enp.shp") # Source: https://www.miteco.gob.es/es/cartografia-y-sig/ide/descargas/biodiversidad/enp.aspx)

# Limpiar datos
toplot<-shp_df %>%
  as.data.frame() %>%
  select(FIGURA_LP) %>%
  group_by(FIGURA_LP) %>%
  summarise(n=n()) %>%
  mutate(FIGURA_LP=str_wrap(FIGURA_LP,10)) %>%
  mutate(FIGURA_LP=fct_reorder(FIGURA_LP,n,.fun = sum,.desc = T))

# Graficar
g<-ggplot(data=toplot,aes(fill="no_group",values=n)) +
  geom_waffle(color = "white", size = .25, n_rows = 15, flip = TRUE) +
  facet_wrap(~FIGURA_LP,nrow=3, strip.position = "bottom") +
  scale_x_discrete() + 
  scale_y_continuous(labels = function(x) x * 10, # make this multiplyer the same as n_rows
                     expand = c(0,0)) +
  ggthemes::scale_fill_tableau(name=NULL) +
  coord_equal() +
  theme_minimal() +
  labs(title = "Espacios naturales protegidos en España según tipo",
       y = "Número de espacios",
       caption = "twitter: @GuillemSalazar\nCódigo: https://github.com/GuillemSalazar/desafio_30_dias_de_graficos") +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(size=0.3),
        axis.ticks.y = element_line(size=0.2),
        axis.text = element_text(color="black",size=4),
        legend.position = "none",
        strip.text = element_text(size=5),
        plot.caption = element_text(size=5),
        plot.title = element_text(hjust=0.5)) +
  guides(fill = guide_legend(reverse = TRUE))
  
# Guardar gráfico
ggsave(filename = "../images/dia_16.png",g,width = twitter_dim$width,height = twitter_dim$height)

