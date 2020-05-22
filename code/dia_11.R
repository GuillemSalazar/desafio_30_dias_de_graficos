library(tidyverse)
library(curl)
library(data.table)
library(lubridate)
library(scico)

# Functions -----------------------------------
get_verba<-function(search_term){
  library(tidyverse)
  library(curl)
  library(data.table)
  library(lubridate)
  
  search_term_fixed<-curl_escape(search_term)
  search_url<-paste("https://verba.civio.es/api/search.csv?q=%22",search_term_fixed,"%22&size=20000",sep="")
  test<-try(fread(search_url,sep=",",data.table = F),silent = T)
  if (class(test)=="try-error") {
    cat("No results found for",search_term,"\n")
    dat<-NULL
    } else {
      dat<-test
      dat<-dat %>%
        mutate(start_time=seconds_to_period(start_time)) %>%
        mutate(end_time=seconds_to_period(end_time)) %>%
        mutate(td=substr(programme_date,11,13)) %>%
        mutate(programme_date=ymd(substr(programme_date,1,10))) %>%
        mutate(day=day(programme_date),wday=wday(programme_date),week=week(programme_date),month=month(programme_date),year=year(programme_date)) %>%
        mutate(search_term=search_term) %>%
        select(search_term,everything())
      }
  return(dat)
}

# Parámetros
twitter_dim<-list(width=unit(13/1.5,"cm"),height=unit(6.5/1.5,"cm"))


# Obtener datos
terms<-c("Pablo Iglesias","Alfredo Pérez Rubalcaba","Pedro Sánchez","Mariano Rajoy","Pablo Casado","Albert Rivera","Inés Arrimadas")
datos_ncitas<-NULL
#datos<-NULL
for (i in terms){
  cat("Searching",i,"\n")
  dat<-get_verba(i)
  if (!is.null(dat)>0){
    datos_ncitas<-dat %>%
      mutate(year_month=floor_date(programme_date,"month")) %>%
      group_by(search_term,year_month) %>%
      summarise(ncitas=n()) %>%
      bind_rows(datos_ncitas)
    #datos<-datos %>% bind_rows(dat)
  }
}

# Limpiar datos
datos_ncitas<-datos_ncitas %>%
  pivot_wider(names_from = "search_term",values_from = "ncitas") %>%
  replace(is.na(.),0) %>%
  pivot_longer(-year_month,names_to = "search_term",values_to = "ncitas") %>%
  arrange(year_month,search_term) %>%
  mutate(search_term=fct_relevel(search_term,terms))

# Graficar
g<-ggplot(data=datos_ncitas,aes(x=year_month,y=search_term,fill=ncitas)) +
  geom_tile() +
  geom_tile(colour="gray80", size=0.2) +
  scale_fill_scico(palette = "bilbao",trans="sqrt") +
  xlab("") +
  ylab("") +
  theme_minimal() + 
  theme(legend.position = "bottom",
        legend.title = element_text(vjust=1,hjust=0),
        legend.key.size = unit(0.3,"cm"),
        legend.key.width = unit(2,"cm"),
        panel.grid.major=element_blank()) +
  labs(title="Menciones a políticos en telediarios de TVE",
       subtitle="Número de menciones por mes (Datos: verba.civio.es)",
       fill="",
       caption = "Código: https://github.com/GuillemSalazar/desafio_30_dias_de_graficos\ntwitter: @GuillemSalazar")


# Guardar gráfico
ggsave(filename = "../images/dia_11.png",g,width = twitter_dim$width,height = twitter_dim$height)
