library(tidyverse)
library(stringi)
library(scico)

# Parámetros
twitter_dim<-list(width=unit(13/1.5,"cm"),height=unit(6.5/1.5,"cm"))

# Cargar datos
datos<-read_tsv("../datos/frq_incendios2001-2014_dd-descarga_tcm30-199964.txt",locale = readr::locale(encoding = "latin1")) # Source: https://www.miteco.gob.es/es/biodiversidad/servicios/banco-datos-naturaleza/acceso-rapido-datos.aspx

# Limpiar datos
datos<-datos %>%
  select(provincia="Provincia",
         codigo_ine_municipio="C\u0097digo INE",
         municipio="Nombre del Municipio",
         num_conatos="N¼ de conatos",
         num_incendios="N¼ de incendios",
         total_conatos_incendios="Total conatos + incendios",
         superficie_arbolada_ha="Superficie arbolada (ha)",
         superficie_desarbolada_ha="Superficie desarbola (ha)",
         superficie_total_ha="Superficie forestal total (ha)",
         superficie_municipio_ha="Superficie total del Municipio") %>%
  filter(!is.na(codigo_ine_municipio)) %>%
  mutate(codigo_ine_provincia=substr(codigo_ine_municipio,start = 1,stop=2))

datos_provincia_nombre<-datos %>%
  filter(!is.na(provincia)) %>%
  select(codigo_ine_provincia,provincia) %>%
  mutate(provincia=fct_recode(provincia,"A Coruña"="A Coru\u0096a",
                              "Cádiz"="C\u0087diz",
                              "Jaén"="Ja\u008en",
                              "Araba / Álava"="Araba/çlava"))

datos<-datos %>%
  select(-provincia) %>%
  filter(codigo_ine_provincia!=53) %>%
  left_join(datos_provincia_nombre,by="codigo_ine_provincia") %>%
  group_by(provincia) %>%
  mutate(total_incendios_provincia=sum(num_incendios)) %>%
  ungroup() %>%
  filter(provincia!="Ceuta" & provincia!="Melilla")



# Graficar

g<-ggplot(data=datos,aes(x=fct_reorder(provincia,superficie_total_ha,function(x){mean(sqrt(x),na.rm = T)},.desc = T),y=superficie_total_ha,fill=total_incendios_provincia)) +
  geom_violin(scale = "width",alpha=0.8,color="white",lwd=0.2) +
  scale_fill_scico(palette = "roma",direction = -1,breaks=c(100,5000,10000,15000,20000,25000)) +
  ylab("Superficie afectada por municipio (ha)") +
  scale_y_continuous(breaks = c(200,1000,2000,4000,6000,10000,15000,20000,25000),trans = "sqrt") +
  theme_minimal() +
  theme(axis.text.x = element_text(hjust=1,vjust=0.5,angle=90)) +
  theme(axis.title.x = element_blank(),
        axis.text = element_text(color="black"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank()) +
  labs(title="Frecuencia de incedios ocurridos entre los años 2001 a 2014",fill="Número total\nde incendios")

# Guardar gráfico
ggsave(filename = "../images/dia_25.png",g,width = twitter_dim$width,height = twitter_dim$height,dpi=700)





