library(tidyverse)

# Parámetros
twitter_dim<-list(width=unit(13/1.5,"cm"),height=unit(6.5/1.5,"cm"))

# Cargar datos
datos_nacimientos<-read_tsv("../datos/nacimientos.csv",locale = locale(decimal_mark = ","))
datos_defunciones<-read_tsv("../datos/defunciones.csv",locale = locale(decimal_mark = ","))

# Limpiar datos
datos_nacimientos<-datos_nacimientos %>%
  select(Mes,Total) %>%
  mutate(Medida="Nacimientos")
datos_defunciones<-datos_defunciones %>%
  select(Mes,Total) %>%
  mutate(Medida="Defunciones")

datos<-datos_nacimientos %>%
  bind_rows(datos_defunciones) %>%
  mutate(Mes=fct_relevel(Mes,"Enero","Febrero","Marzo","Abril","Mayo","Junio","Julio","Agosto","Septiembre","Octubre","Noviembre","Diciembre")) %>%
  mutate(Medida=fct_relevel(Medida,"Defunciones","Nacimientos")) %>%
  group_by(Mes) %>%
  mutate(Midpoint=sum(Total)/2) %>%
  ungroup()


# Graficar
g<-ggplot(data=datos,aes(x=Mes,y=Total,fill=Medida,color=Medida)) +
  geom_bar(stat="identity",width = 1,alpha=0.8) +
  geom_point(data=datos,aes(x=Mes,y=Midpoint),shape=4,inherit.aes = F,color="white") +
  scale_fill_manual(values=c("#d73027","#4575b4")) +
  scale_color_manual(values=c("#d73027","#4575b4")) +
  coord_polar(start = -pi/12) +
  theme_minimal() +
  theme(panel.grid = element_blank(),
        axis.text.x = element_text(size=6,color = "black"),
        axis.text.y = element_blank(),
        axis.title = element_blank(),
        legend.title = element_blank(),
        plot.caption = element_text(hjust=0,size=4),
        plot.title = element_text(hjust=0.5)) +
  annotate(geom = "text",x = 1,y=datos$Midpoint[1]*1.3,label="Defunciones = Nacimientos",hjust=0.5,size=2.5,vjust=0,color="white") +
  annotate(geom = "segment",x = 1,xend=1,y=datos$Midpoint[1]*1.3,yend=datos$Midpoint[1]*1.1,arrow = arrow(length = unit(1, "mm")),color="white") +
  labs(title="Nacimientos y Defunciones en España en 2019",
       subtitle="Datos: Instituto Nacional de Estadística (INE)",
       caption = "twitter: @GuillemSalazar\nCódigo: https://github.com/GuillemSalazar/desafio_30_dias_de_graficos")

# Guardar gráfico
ggsave(filename = "../images/dia_30.png",g,width = twitter_dim$width,height = twitter_dim$height)

