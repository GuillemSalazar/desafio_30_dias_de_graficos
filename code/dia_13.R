library(tidyverse)
library(gganimate)
library(gifski)
library(viridis)

# Parámetros
twitter_dim<-list(width=unit(13/1.5,"cm"),height=unit(6.5/1.5,"cm"))

# Cargar datos
temp_df <- readr::read_csv("../datos/noaa__ny_temp.csv")
temp_df<-temp_df %>% mutate(seq_point=1:nrow(temp_df)) %>%
  separate(DATE,into=c("year","month"),sep="-",remove = F) %>%
  mutate(year=as.integer(year),month=as.integer(month))


# Graficar
g<-ggplot(data=temp_df,aes(x=month,y=TAVG,col=TAVG)) +
  geom_line(aes(group=year)) +
  coord_polar() +
  scale_color_viridis(name="Temperatura",option = "magma") +
  transition_reveal(along = seq_point) +
  shadow_wake(wake_length = 0.1) +
  theme_minimal() +
  xlab("") +
  ylab("Temperatura promedio mensual") +
  scale_x_continuous(breaks = 1:12,labels = c("Enero","Febrero","Marzo","Abril","Mayo","Junio","Julio","Agosto","Septiembre","Octubre","Noviembre","Diciembre")) +
  theme(axis.text.x = element_text(size=8),
        legend.position = "right",
        legend.direction = "vertical",
        legend.title = element_text(color="black"),
        legend.text = element_text(color="black"),
        plot.background = element_rect(color="white",fill="white"),
        axis.title.y = element_text(color="black"),
        axis.text = element_text(color="black",size=7),
        panel.grid = element_line(color="gray85"),
        title = element_text(color="black")) +
  labs(title="Temperatura en Central Park (NY)",
       subtitle="{temp_df$year[frame_along]}",
       caption = "twitter: @GuillemSalazar\nCódigo: https://github.com/GuillemSalazar/desafio_30_dias_de_graficos")

duration<-20
fps<-10 #ceiling(nrow(temp_df)/duration)
animation<-animate(g, duration = duration, fps = fps,renderer = gifski_renderer())
anim_save(animation = animation,filename = "../images/dia_13.gif")
