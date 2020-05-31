library(tidyverse)
library(data.table)
library(rjson)
library(network)
library(ggnetwork)
library(viridis)

# Parámetros
twitter_dim<-list(width=unit(13/1.5,"cm"),height=unit(6.5/1.5,"cm"))

# Obtener datos
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

# Limpiar nombres de CCAA
datos_df<-datos_df %>%
  separate(origen,into=c("origen","todelete"),sep=",",fill = "right") %>%
  select(-todelete) %>%
  separate(destino,into=c("destino","todelete"),sep=",",fill = "right") %>%
  select(-todelete)

# Filtrar solo el mayor flujo para cada origen y destino
datos_max_emigracion<-datos_df %>%
  group_by(origen) %>%
  mutate(migracion=replace(migracion,migracion<max(migracion,na.rm = T),0)) %>%
  arrange(origen,destino) %>%
  pivot_wider(names_from = "destino",values_from = "migracion") %>%
  mutate_all(funs(replace(., is.na(.), 0))) %>%
  arrange(origen) %>%
  column_to_rownames(var="origen") %>%
  as.matrix()


# Crear network
net<-datos_max_emigracion %>% as.network.matrix(matrix.type = "adjacency",directed = T,names.eval = "weights",ignore.eval = F,hyper = F,loops = T,multiple = T)

# Graficar
g<-ggplot(ggnetwork(net), aes(x = x, y = y, xend = xend, yend = yend)) +
  geom_edges(aes(color=weights),arrow = arrow(length = unit(3, "pt"), type = "closed"),curvature = 0) +
  geom_nodes() +
  geom_nodelabel_repel(aes(label = vertex.names),size=2,alpha=0.8) +
  theme_blank() +
  scale_color_viridis(name="Emigración",option="magma") +
  labs(title="Flujo migratorio interautonómico (2018)",
       subtitle="Solo se muestra el flujo de máxima emigración para cada CCAA",
       caption = "Código: https://github.com/GuillemSalazar/desafio_30_dias_de_graficos\ntwitter: @GuillemSalazar")

ggsave(filename = "../images/dia_20.png",g,width = twitter_dim$width,height = twitter_dim$height)
