library(rjson)
library(tidyverse)

# Parametros
twitter_dim<-list(width=unit(13/2,"cm"),height=unit(6.5/2,"cm"))

# Cargar datos y convertir en data frame
datos_json <- fromJSON(file = "../datos/flujo_migracion_2018.json")

json_parser<-function(x){
  if(length(x$Data)>0) {
    res<-data.frame(origen=x$MetaData[[3]]$Nombre,
                    destino=x$MetaData[[2]]$Nombre,
                    migracion=x$Data[[1]]$Valor)
  } else {
    res<-data.frame(origen=x$MetaData[[3]]$Nombre,
                    destino=x$MetaData[[2]]$Nombre,
                    migracion=NA)
  }
  
  return(res)
}

datos_df<-NULL
for (i in 1:length(datos_json)){datos_df<-datos_df %>% bind_rows(json_parser(datos_json[[i]]))}

# Crear las posiciones
ccaa_positions<-datos_df %>%
  distinct(origen) %>%
  select(ccaa=origen) %>%
  mutate(x_position=1:n())

# Añadir las posiciones
datos_df<-datos_df %>%
  left_join(ccaa_positions,by=c("origen"="ccaa")) %>%
  rename(x_position_origen=x_position) %>%
  left_join(ccaa_positions,by=c("destino"="ccaa")) %>%
  rename(x_position_destino=x_position) %>%
  filter(!is.na(migracion)) %>%
  mutate(y_position=ifelse(x_position_origen>x_position_destino,0.1,-0.1))

# Calcular migración total
datos_df<-datos_df %>%
  group_by(destino) %>%
  mutate(migracion_total_destino=sum(migracion)) %>%
  ungroup() %>%
  group_by(origen) %>%
  mutate(migracion_total_origen=sum(migracion)) %>%
  arrange(migracion)

# Filtrar el destino con mayor migración para cada origen
datos_df<-datos_df %>%
  group_by(origen,.drop = T) %>%
  filter(migracion==max(migracion)) %>%
  arrange(migracion)
  

# Graficar
g<-ggplot() +
  geom_segment(data=datos_df %>% distinct(origen,x_position_origen),
               aes(x=x_position_origen,y=0,xend=x_position_origen,yend=0.7),
               alpha=0.2,size=0.5,linetype="dotted") +
  geom_point(data=datos_df %>% distinct(origen,x_position_origen,migracion_total_origen),
             aes(x=x_position_origen,y=0,size=migracion_total_origen),
             alpha=0.5,
             col="#35978f") +
  geom_curve(data=datos_df,
             aes(x=x_position_origen,y=y_position,xend=x_position_destino,yend=y_position,color=migracion),
             arrow=arrow(type="closed",length = unit(2,"mm"),angle = 20),angle=60,curvature=0.3,ncp = 20) +
  geom_text(data=datos_df %>% distinct(origen,x_position_origen,migracion_total_origen),
            aes(x=x_position_origen,y=0.75,label=origen),
            angle=40,size=2.4,hjust=0) +
  ylim(c(-0.6,1.5)) +
  scale_color_gradient(low = "#e0ecf4",high = "#4d004b",breaks = c(1000,5000,10000,15000),labels = c("1,000","5,000","10,000","15,000")) +
  scale_size(breaks = c(5000,10000,50000),labels = c("5,000","10,000","50,000")) +
  theme_void() +
  theme(plot.margin = margin(t = 0.1,r = 0.5,b = 0.5,l =0.5,"cm"),
        legend.position = "left",
        legend.key.size = unit(3,"mm"),
        legend.title = element_text(size=4,face = "bold"),
        legend.text = element_text(size=3)) +
  labs(title="Flujo de migración interautonómica en España (2018)",subtitle="Destino con mayor migración para cada Comunidad Autónoma",color="Migración",size="Emigración total",caption=element_text("twitter: @GuillemSalazar")) +
  guides(color = guide_legend(order = 2),
         size = guide_legend(order = 1))
  

# Guardar gráfico
ggsave(filename = "../images/dia_05.png",g,width = twitter_dim$width,height = twitter_dim$height)

