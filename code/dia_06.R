library(rjson)
library(tidyverse)
library(ggrepel)

# Parametros
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
  mutate(prop=n/sum(n)) %>%
  arrange(ccaa,desc(tipo)) %>%
  group_by(ccaa) %>%
  mutate(lab.ypos = cumsum(prop) - 0.5*prop) %>%
  separate(ccaa,into=c("ccaa","todelete"),sep=",",extra="merge",fill="right") %>%
  select(-todelete) %>%
  mutate(tipo=gsub("_"," ",tipo)) %>%
  mutate(tipo=fct_relevel(tipo,"investigador hombres","personal hombres","investigador mujeres","personal mujeres"))

# Graficar
g<-ggplot(data=datos_df,aes(x = 2,y=prop,fill=tipo)) +
  geom_bar(stat = "identity", color = "white") +
  geom_text(aes(x=2.7,y = lab.ypos, label = round(prop,2)),size=2,fontface="bold") +
  facet_wrap(~ccaa,nrow = 3) +
  coord_polar(theta = "y", start = 0) +
  scale_fill_manual(values = mycols) +
  scale_color_manual(values = mycols) +
  theme_void()+
  theme(plot.margin = margin(t = 0.1,r = 0.1,b = 0.1,l =0.1,"cm"),plot.title = element_text(vjust = 3),plot.subtitle = element_text(vjust = 3),legend.position = "bottom",legend.direction = "horizontal",legend.title = element_blank(),strip.text = element_text(size=6)) +
  xlim(0.5, 3) +
  labs(title="Personal en I+D en España por sexos (2018)",subtitle="Porcentaje de personal en EJC (equivalencia a jornada completa)",caption=element_text("twitter: @GuillemSalazar"))


# Guardar gráfico
ggsave(filename = "../images/dia_06.png",g,width = twitter_dim$width,height = twitter_dim$height)

