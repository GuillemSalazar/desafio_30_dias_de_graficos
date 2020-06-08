library(rjson)
library(tidyverse)
library(circlize)

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

datos_df<-datos_df %>%
  filter(!is.na(migracion)) %>%
  separate(origen,into=c("origen","todelete1"),sep=",",fill = "right") %>%
  separate(destino,into=c("destino","todelete2"),sep=",",fill = "right") %>%
  select(-starts_with("todelete"))

# Graficar
png(filename = "../images/dia_28.png",res=300,width=13*2,height=6.5*2,units = "cm")
set.seed(1234)
chordDiagram(datos_df,
             directional = 1,
             direction.type = c("diffHeight","arrows"),
             diffHeight  = -0.04,
             annotationTrack = "grid",
             link.arr.type = "big.arrow")
circos.track(track.index = 1, panel.fun = function(x, y) {
  circos.text(CELL_META$xcenter, CELL_META$ylim[1], CELL_META$sector.index,facing = "clockwise", niceFacing = TRUE,cex=0.8, adj = c(1, 0.5))
  },bg.border = NA)
title("Flujo de migración interautonómica en España (2018)", cex = 0.6)
mtext("twitter: @GuillemSalazar\nCódigo: https://github.com/GuillemSalazar/desafio_30_dias_de_graficos",side = 1,adj = 0,cex=0.5)
dev.off()
