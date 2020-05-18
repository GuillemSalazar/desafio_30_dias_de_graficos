library(tidyverse)
library(ggridges)
library(viridis)
library(lubridate)

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
  filter(sub_region_1 %in% c("Ceuta","Melilla","")==F) %>%
  mutate(date_new=paste(str_pad(year(date),2,pad="0"),str_pad(month(date),2,pad="0"),str_pad(day(date),2,pad="0"),sep="-"))

g<-ggplot(data=dat_filtered,aes(x=perc_change,y=as.factor(date_new),fill=stat(x))) +
  geom_vline(xintercept = 0) +
  geom_density_ridges_gradient(scale=3,rel_min_height=0.01) +
  scale_fill_gradient2(low = "#313695" ,mid = "gray90" ,high = "#a50026",midpoint = 0) +
  #scale_x_continuous(trans = "sqrt") +
  theme_minimal() +
  facet_grid(.~sector,scales = "free_x") +
  #theme_ridges(center_axis_labels = FALSE) +
  theme(panel.grid.major.y = element_line(),legend.position = "none",axis.text.y = element_text(size=5)) +
  xlab("Percent change in visits from baseline") +
  ylab("Date (Y-M-D)") +
  labs(title="COVID-19 Google's Community Mobility Report",subtitle="Change in visits to different category of places in Spain",caption=element_text("twitter: @GuillemSalazar"))

# Guardar gráfico
ggsave(filename = "../images/dia_07.png",g,width = twitter_dim$width,height = twitter_dim$height)
