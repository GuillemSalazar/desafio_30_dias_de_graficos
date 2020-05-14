library(tidyverse)
library(ggrepel)

# Parámetros
url<-"https://www.gstatic.com/covid19/mobility/Global_Mobility_Report.csv?cachebust=722f3143b586a83f"
twitter_dim<-list(width=unit(13/1.5,"cm"),height=unit(6.5/1.5,"cm"))

# Cargar datos
dat<-read_csv(url)

# Limpiar datos
dat_filtered<-dat %>%
  mutate(date=as.POSIXct(date,format="%Y-%m-%d")) %>%
  pivot_longer(cols = ends_with("baseline"),names_to = "sector",values_to = "perc_change") %>%
  mutate(sector=gsub("_percent_change_from_baseline","",sector)) %>%
  filter(date>as.POSIXct("2020-04-01",format="%Y-%m-%d")) %>%
  group_by(country_region_code,country_region,sector) %>%
  summarise(perc_change=mean(perc_change,na.rm = T)) %>%
  pivot_wider(names_from = "sector",values_from = "perc_change")

# Graficar
p<-ggplot(data = dat_filtered,aes(x=workplaces,y=parks,label=country_region)) +
  geom_hline(yintercept = 0,linetype="dotted") +
  geom_vline(xintercept = 0,linetype="dotted") +
  geom_point(alpha=0.5,size=4,color="#756bb1") +
  geom_text_repel(size=2,force = 2,segment.alpha = 0.5) +
  theme_minimal() +
  xlab("Percent change in visits to workplaces") +
  ylab("Percent change in visits to parks") +
  theme(strip.text = element_text(face = "bold")) +
  labs(title="COVID-19 Google's Community Mobility Report",subtitle="Mean change in visits to workplaces and parks after April 1st (compared to baseline: January 3 - February 6)",caption=element_text("twitter: @GuillemSalazar"))

# Guardar gráfico
ggsave(filename = "../images/dia_03.png",p,width = twitter_dim$width,height = twitter_dim$height)
