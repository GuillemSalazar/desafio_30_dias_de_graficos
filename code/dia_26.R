library(tidyverse)
library(stringi)
library(scico)
library(rjson)

# Parámetros
twitter_dim<-list(width=unit(13/1.5,"cm"),height=unit(6.5/1.5,"cm"))
mycols <- c("#006d2c","#66c2a4","#54278f","#9e9ac8")

# Cargar datos y convertir en data frame
datos_json <- fromJSON(file = "../datos/personal_id_2018.json")

json_parser<-function(x){
  res<-data.frame(ccaa=x$MetaData[[1]]$Nombre,
                  tipo=x$MetaData[[2]]$Nombre,
                  valor=x$Data[[1]]$Valor)
  
  return(res)
}

datos_df<-NULL
for (i in 1:length(datos_json)){datos_df<-datos_df %>% bind_rows(json_parser(datos_json[[i]]))}

# Limpiar datos
datos_df<-datos_df %>%
  mutate(tipo=fct_recode(tipo,personal_total='Personal en EJC: Total personal',
                         personal_mujeres='Personal en EJC: Mujeres',
                         investigador_total='Investigadores en EJC: Total personal',
                         investigador_mujeres='Investigadores en EJC: Mujeres')) %>%
  pivot_wider(names_from = "tipo",values_from = "valor") %>%
  mutate(personal_hombres=personal_total-personal_mujeres,
         investigador_hombres=investigador_total-investigador_mujeres) %>%
  select(-personal_total,-investigador_total) %>%
  pivot_longer(-ccaa,names_to = "tipo",values_to = "n") %>%
  group_by(ccaa) %>%
  arrange(ccaa,desc(tipo)) %>%
  group_by(ccaa) %>%
  separate(ccaa,into=c("ccaa","todelete"),sep=",",extra="merge",fill="right") %>%
  select(-todelete) %>%
  mutate(tipo=gsub("_"," ",tipo)) %>%
  mutate(tipo=fct_relevel(tipo,"investigador hombres","personal hombres","investigador mujeres","personal mujeres"))

toplot<-datos_df %>%
  group_by(ccaa) %>%
  mutate(total_ccaa=sum(n),y=n/sum(n),total_y=sum(n)) %>%
  ungroup() %>%
  mutate(x=total_ccaa/sum(n)) %>%
  arrange(x) %>%
  group_by(ccaa) %>%
  mutate(ymax=cumsum(y),ymin=ymax-y) %>%
  group_by(tipo) %>%
  mutate(xmax=cumsum(x),xmin=xmax-x)


g<-ggplot(data=toplot,aes(xmin=xmin,xmax=xmax,ymin=ymin,ymax=ymax,fill=tipo)) +
  geom_rect(color="white") +
  scale_x_continuous(breaks=c(0,0.01,0.02,0.05,seq(0.1,1,by=0.1)),limits=c(0,1),trans = "sqrt",labels = scales::percent) +
  scale_y_continuous(breaks=seq(0,1,by=0.1),limits=c(0,1.35),labels = scales::percent) +
  scale_fill_manual(values=mycols) +
  geom_text(data=toplot,aes(x=xmin+(xmax-xmin)/2,y=1,label=ccaa),angle=45,hjust=0,vjust=-1,size=2.5) +
  theme_minimal() +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        axis.ticks = element_line(),
        axis.title = element_blank(),
        axis.text.x = element_text(angle=45,hjust=1,vjust=1),
        legend.title = element_blank()) +
  labs(title="Personal en I+D en España por sexos (2018)",
       subtitle="Porcentaje de personal en EJC (equivalencia a jornada completa)",
       caption = "Código: https://github.com/GuillemSalazar/desafio_30_dias_de_graficos\ntwitter: @GuillemSalazar")


# Guardar gráfico
ggsave(filename = "../images/dia_26.png",g,width = twitter_dim$width,height = twitter_dim$height,dpi=700)





