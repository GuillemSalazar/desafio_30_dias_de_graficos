library(tidyverse)

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
  theme(legend.position = "right",legend.direction = "vertical",
        legend.title = element_blank(),
        strip.text = element_text(face = "bold")) +
  scale_color_brewer(palette="Set2") +
  labs(title="COVID-19 Google's Community Mobility Report",subtitle="Mean change in visits to different category of places in Spain",caption=element_text("twitter: @GuillemSalazar"))

# Añadir anotaciones
p<-p +
  geom_vline(xintercept = as.POSIXct("2020-03-14",format="%Y-%m-%d"),linetype=3) + # Lockdown
  annotate(geom = "curve", x = as.POSIXct("2020-03-10",format="%Y-%m-%d"), y = 90, xend = as.POSIXct("2020-03-13",format="%Y-%m-%d"), yend = 70,curvature = 0.3, arrow = arrow(length = unit(1, "mm"))) +
  annotate(geom = "text", x = as.POSIXct("2020-03-10",format="%Y-%m-%d"), y = 100, label = "Lockdown", hjust = "right",size=3) +
  geom_vline(xintercept = as.POSIXct("2020-04-26",format="%Y-%m-%d"),linetype=3) + # Salida de menores de 14 años
  annotate(geom = "curve", x = as.POSIXct("2020-04-22",format="%Y-%m-%d"), y = 90, xend = as.POSIXct("2020-04-25",format="%Y-%m-%d"), yend = 70,curvature = 0.3, arrow = arrow(length = unit(1, "mm"))) +
  annotate(geom = "text", x = as.POSIXct("2020-04-22",format="%Y-%m-%d"), y = 100, label = "1h walks allowed (<14 y/o)", hjust = "right",size=3)

# Guardar gráfico
ggsave(filename = "../images/dia_02.png",p,width = twitter_dim$width,height = twitter_dim$height)
