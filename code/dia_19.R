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
colors<-c("#4daf4a","#e41a1c")

# Obtener datos
terms<-c("pandemia","virus")
terms<-c("Siria","refugiados")
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

streamgraph_transform<-function(data,fct,x,y){
  .data_transformed<-data %>%
    rename(y=y,x=x,fct=fct) %>%
    arrange(x,fct) %>%
    group_by(x) %>%
    mutate(ymax=cumsum(y),
           ymin=ymax-y,
           mean=mean(y),
           ymin_centered=ymin-mean,
           ymax_centered=ymax-mean) %>%
    ungroup() %>%
    mutate(tosubstract=min(ymin_centered),
           ymin_centered=ymin_centered-tosubstract,
           ymax_centered=ymax_centered-tosubstract) %>%
    select(-tosubstract,-mean)
  
  return(.data_transformed)
}

toplot<-streamgraph_transform(data = datos_ncitas,fct = "search_term",x="year_month",y = "ncitas")

g<-ggplot(data=toplot,aes(x=x,ymin=ymin_centered,ymax=ymax_centered,fill=fct)) +
  geom_ribbon(stat = "identity",alpha=0.8) +
  theme_minimal() +
  scale_fill_manual(values=colors) +
  ylab("Menciones por mes") +
  theme(panel.grid.major.y = element_blank(),
        axis.title.x = element_blank(),
        legend.position = "none") +
  annotate(geom = "curve",x = as.Date("2016-02-01"),xend=as.Date("2015-11-01"),y = 700,yend=600,curvature = .3, arrow = arrow(length = unit(2, "mm")),color=colors[2]) +
  annotate(geom = "curve",x = as.Date("2016-02-01"),xend=as.Date("2015-11-01"),y = 100,yend=200,curvature = -0.3, arrow = arrow(length = unit(2, "mm")),color=colors[1]) +
  annotate(geom = "text",x = as.Date("2016-02-01"),y = 700,label="refugiados",hjust=-0.1,size=3,color=colors[2]) +
  annotate(geom = "text",x = as.Date("2016-02-01"),y = 100,label="Siria",hjust=-0.1,size=3,color=colors[1]) +
  labs(title=expression(paste("Menciones a los términos"~bolditalic(Siria)~"y"~bolditalic(refugiados)~"en los telediarios de TVE")),
       subtitle="Datos: verba.cvio.es",
       caption = "Código: https://github.com/GuillemSalazar/desafio_30_dias_de_graficos\ntwitter: @GuillemSalazar")



# Guardar gráfico
ggsave(filename = "../images/dia_19.png",g,width = twitter_dim$width,height = twitter_dim$height)
