library(tidyverse)
library(vegan)
library(ggdendro)
library(patchwork)

# Parámetros
url<-"https://www.gstatic.com/covid19/mobility/Global_Mobility_Report.csv?cachebust=722f3143b586a83f"
twitter_dim<-list(width=unit(13/1.5,"cm"),height=unit(6.5/1.5,"cm"))
phase_data<-data.frame(sub_region_1=c("Andalusia","Aragon","Asturias","Balearic Islands","Basque Country","Canary Islands","Cantabria","Castile and León","Castile-La Mancha","Catalonia","Community of Madrid","Extremadura","Galicia","La Rioja","Navarre","Region of Murcia","Valencian Community"),
           phase=c("Partial phase 1","Phase 1","Phase 1","Phase 1","Phase 1","Phase 1","Phase 1","Partial phase 1","Partial phase 1","Partial phase 1","Phase 0","Phase 1","Phase 1","Phase 1","Phase 1","Phase 1","Partial phase 1"))

# Cargar datos
dat<-read_csv(url)

# Limpiar datos
dat_filtered<-dat %>%
  mutate(date=as.POSIXct(date,format="%Y-%m-%d")) %>%
  filter(country_region_code=="ES") %>%
  filter(!is.na(sub_region_1)) %>%
  filter(sub_region_1 %in% c("Ceuta","Melilla","")==F) %>%
  filter(date>as.POSIXct("2020-04-11",format="%Y-%m-%d") & date<as.POSIXct("2020-04-17",format="%Y-%m-%d")) %>%
  pivot_longer(cols = ends_with("baseline"),names_to = "sector",values_to = "perc_change") %>%
  left_join(phase_data,by="sub_region_1") %>%
  mutate(sub_region_1=paste(sub_region_1," (",phase,")",sep=""))

# Calcular distancia (después de estandarizar)
dat_filtered_dist<-dat_filtered %>%
  select(-date) %>%
  group_by(sector,sub_region_1) %>%
  summarise(perc_change=mean(perc_change,na.rm=T)) %>%
  pivot_wider(names_from = "sector",values_from = "perc_change") %>%
  column_to_rownames(var="sub_region_1") %>%
  decostand(method = "range",MARGIN = 2) %>%
  as.matrix() %>%
  dist()

cluster_res<-hclust(dat_filtered_dist)

# Graficar
plot1<-ggdendrogram(cluster_res,rotate = T) +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_text(color="black",hjust=0.5))

plot2<-dat_filtered %>%
  group_by(sub_region_1,sector) %>%
  summarise(perc_change=mean(perc_change,na.rm=T)) %>%
  ungroup() %>%
  mutate(sub_region_1=fct_relevel(sub_region_1,cluster_res$labels[cluster_res$order])) %>%
  mutate(sector=gsub("_"," ",gsub("_change_from_baseline","",sector))) %>%
ggplot(aes(x=sub_region_1,y=perc_change,col=sector,fill=sector)) +
  geom_hline(yintercept = 0,linetype="dotted") +
  geom_point(alpha=0.8,shape=23) +
  coord_flip() +
  theme_minimal() +
  theme(legend.position = "bottom",
        axis.text.y = element_blank(),
        axis.text.x = element_text(color="black"),
        legend.text = element_text(size=6)) +
  ylab("Change in visits (% change to baseline)") +
  xlab("Region") +
  ylim(c(-100,50)) +
  scale_color_brewer(name=NULL,palette = "Set2") +
  scale_fill_brewer(name=NULL,palette = "Set2")

  
p<-(plot2 | plot1) + plot_layout(widths = c(4,1),ncol = 2) +
  plot_annotation(title="Ordination of Spanish regions based on mobility patterns",
                  subtitle="Data: COVID-19 Google's Community Mobility Report (week 11 - 17 Mayo 2020)",
       caption = "twitter: @GuillemSalazar\nCódigo: https://github.com/GuillemSalazar/desafio_30_dias_de_graficos")

# Guardar gráfico
ggsave(filename = "../images/dia_15.png",p,width = twitter_dim$width,height = twitter_dim$height)

