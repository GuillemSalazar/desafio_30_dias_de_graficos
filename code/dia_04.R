library(tidyverse)

# Parámetros
url<-"https://www.gstatic.com/covid19/mobility/Global_Mobility_Report.csv?cachebust=722f3143b586a83f"
twitter_dim<-list(width=unit(13,"cm"),height=unit(6.5,"cm"))

# Cargar datos
dat<-read_csv(url)

# Limpiar datos
dat_filtered<-dat %>%
  mutate(date=as.POSIXct(date,format="%Y-%m-%d")) %>%
  filter(country_region_code=="ES") %>%
  pivot_longer(cols = ends_with("baseline"),names_to = "sector",values_to = "perc_change") %>%
  mutate(sector=gsub(" percent change from baseline","",gsub("_"," ",sector))) %>%
  filter(!is.na(sub_region_1)) %>%
  filter(sub_region_1 %in% c("Ceuta","Melilla","")==F)

# Graficar
p<-ggplot(data = dat_filtered,aes(x=date,y=perc_change,col=sector)) +
  geom_hline(yintercept = 0) +
  geom_line() +
  facet_wrap(~sub_region_1) +
  theme_minimal() +
  xlab("") +
  ylab("Percent change in visits from baseline") +
  theme(legend.position = c(0.8, 0.1),legend.direction = "horizontal",
        legend.title = element_blank(),
        legend.text = element_text(size=10),legend.key.size = unit(0.5,"cm"),
        strip.text = element_text(face = "bold")) +
  scale_color_brewer(palette="Set2") +
  labs(title="COVID-19 Google's Community Mobility Report",subtitle="Mean change in visits to different category of places in Spain (by region)",caption=element_text("twitter: @GuillemSalazar"))

# Añadir anotaciones
p<-p +
  geom_vline(xintercept = as.POSIXct("2020-03-14",format="%Y-%m-%d"),linetype=3) + # Lockdown
  annotate(geom = "curve", x = as.POSIXct("2020-03-10",format="%Y-%m-%d"), y = 90, xend = as.POSIXct("2020-03-13",format="%Y-%m-%d"), yend = 70,curvature = 0.3, arrow = arrow(length = unit(1, "mm"))) +
  annotate(geom = "text", x = as.POSIXct("2020-03-10",format="%Y-%m-%d"), y = 100, label = "Lockdown", hjust = "right",size=2) +
  geom_vline(xintercept = as.POSIXct("2020-04-26",format="%Y-%m-%d"),linetype=3) + # Salida de menores de 14 años
  annotate(geom = "curve", x = as.POSIXct("2020-04-22",format="%Y-%m-%d"), y = 90, xend = as.POSIXct("2020-04-25",format="%Y-%m-%d"), yend = 70,curvature = 0.3, arrow = arrow(length = unit(1, "mm"))) +
  annotate(geom = "text", x = as.POSIXct("2020-04-22",format="%Y-%m-%d"), y = 100, label = "1h walks allowed (<14 y/o)", hjust = "right",size=2) +
  geom_vline(xintercept = as.POSIXct("2020-05-02",format="%Y-%m-%d"),linetype=3) + # Fase 0
  annotate(geom = "curve", x = as.POSIXct("2020-04-30",format="%Y-%m-%d"), y = 50, xend = as.POSIXct("2020-05-02",format="%Y-%m-%d"), yend = 30,curvature = 0.3, arrow = arrow(length = unit(1, "mm"))) +
  annotate(geom = "text", x = as.POSIXct("2020-04-30",format="%Y-%m-%d"), y = 60, label = "Phase 0: walks allowed (everyone)", hjust = "right",size=2)

# Guardar gráfico
ggsave(filename = "../images/dia_04.png",p,width = twitter_dim$width,height = twitter_dim$height)
