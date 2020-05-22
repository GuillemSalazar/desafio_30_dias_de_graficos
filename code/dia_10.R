library(tidyverse)
library(scico)
library(patchwork)

# Parámetros
twitter_dim<-list(width=unit(13/1.5,"cm"),height=unit(6.5/1.5,"cm"))
nmax<-20

# Paletas
palette_names<-scico_palette_names()

# Crear lista de gráficos
plot_list<-list(NULL)
for (i in 1:length(palette_names)){
  dat<-data.frame(xmin=1:nmax,ymin=rep(1,nmax),xmax=2:(nmax+1),ymax=rep(2,nmax),fill=1:nmax)
  plot_list[[i]]<-ggplot(data=dat,aes(xmin=xmin,ymin=ymin,xmax=xmax,ymax=ymax,fill=fill)) +
    geom_rect(color="white") +
    xlim(c(1,nmax+1)) +
    ylim(c(1,2)) +
    coord_fixed() +
    theme_void() +
    scale_fill_scico(palette = palette_names[i]) +
    labs(title = paste("scale_fill_scico(palette = '",palette_names[i],"')")) +
    theme(legend.position = "none",plot.title = element_text(hjust = 0.5,size=4))
}

# Reunir gráficos
g<-wrap_plots(plot_list,ncol = 3,nrow = 8) +
  plot_annotation(title = "Probando paletas de colores",subtitle = "Paletas del paquete 'scico'",caption = "Código: https://github.com/GuillemSalazar/desafio_30_dias_de_graficos\ntwitter: @GuillemSalazar")

# Guardar gráfico
ggsave(filename = "../images/dia_10.png",width = twitter_dim$width,height = twitter_dim$height)

