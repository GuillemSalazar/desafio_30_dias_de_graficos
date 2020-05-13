library(tidyverse)
library(ggrepel)

# Parámetros
url<-"https://www.gstatic.com/covid19/mobility/Global_Mobility_Report.csv?cachebust=722f3143b586a83f"
twitter_dim<-list(width=unit(13/2,"cm"),height=unit(6.5/2,"cm"))

# Cargar datos
dat<-read_csv(url)

# Limpiar datos
dat_filtered<-dat %>%
  mutate(date=as.POSIXct(date,format="%Y-%m-%d")) %>%
  filter(country_region_code=="ES") %>%
  pivot_longer(cols = ends_with("baseline"),names_to = "sector",values_to = "perc_change") %>%
  mutate(sector=gsub(" percent change from baseline","",gsub("_"," ",sector))) %>%
  group_by(country_region_code,country_region,date,sector) %>%
  summarise(perc_change=mean(perc_change,na.rm = T))

# Graficar
p<-ggplot(data = dat_filtered,aes(x=date,y=perc_change,col=sector)) +
  geom_hline(yintercept = 0) +
  geom_line() +
  theme_minimal() +
  xlab("") +
  ylab("Percent change in visits from baseline") +
  theme(legend.position = "none",
        strip.text = element_text(face = "bold")) +
  scale_color_brewer(palette="Set2") +
  labs(title="COVID-19 Google's Community Mobility Report",subtitle="Mean change in visits to different category of places in Spain",caption=element_text("twitter: @GuillemSalazar"))

# Añadir anotaciones
p<-p +
  geom_vline(xintercept = as.POSIXct("2020-03-14",format="%Y-%m-%d"),linetype=3) + # Lockdown
  annotate(geom = "curve", x = as.POSIXct("2020-03-10",format="%Y-%m-%d"), y = 70, xend = as.POSIXct("2020-03-13",format="%Y-%m-%d"), yend = 50,curvature = 0.3, arrow = arrow(length = unit(1, "mm"))) +
  annotate(geom = "text", x = as.POSIXct("2020-03-10",format="%Y-%m-%d"), y = 80, label = "Lockdown", hjust = "right",size=3) +
  geom_vline(xintercept = as.POSIXct("2020-04-26",format="%Y-%m-%d"),linetype=3) + # Salida de menores de 14 años
  annotate(geom = "curve", x = as.POSIXct("2020-04-22",format="%Y-%m-%d"), y = 70, xend = as.POSIXct("2020-04-25",format="%Y-%m-%d"), yend = 50,curvature = 0.3, arrow = arrow(length = unit(1, "mm"))) +
  annotate(geom = "text", x = as.POSIXct("2020-04-22",format="%Y-%m-%d"), y = 80, label = "1h walks allowed (<14 y/o)", hjust = "right",size=3) +
  geom_text(data=filter(dat_filtered,date=="2020-05-07 02:00:00 CEST"),aes(x=date,y=perc_change,label=sector),hjust="left",size=2.5) +
  coord_cartesian(clip="off") +
  theme(plot.margin = unit(c(1,4,1,1),"lines"))

# Guardar gráfico
ggsave(filename = "../images/dia_02.png",p,width = twitter_dim$width,height = twitter_dim$height)
