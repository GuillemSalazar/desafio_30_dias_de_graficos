library(tidyverse)
library(tokenizers)
library(ggwordcloud)
library(scico)

# Parametros
url<-"https://raw.githubusercontent.com/cienciadedatos/datos-de-miercoles/master/datos/2019/2019-07-31/la_casa_de_papel.csv"
url_stopwords<-"https://countwordsfree.com/stopwords/spanish/txt"
twitter_dim<-list(width=unit(13/1.5,"cm"),height=unit(6.5/1.5,"cm"))

# Cargar datos
datos<-read_csv(url)
stopwords<-read_csv(url_stopwords,col_names = "palabra")

palabras_df<-data.frame(palabra=unlist(tokenize_words(datos$texto))) %>%
  group_by(palabra) %>%
  summarise(n=n()) %>%
  anti_join(stopwords,by = "palabra") %>%
  filter(str_length(palabra)>3) %>%
  arrange(desc(n)) %>%
  head(n=100)
  

# Graficar
g<-ggplot(data = palabras_df,aes(label=palabra,size=n,color=n)) +
  geom_text_wordcloud() +
  scale_color_scico(palette = "vik") +
  theme_void() +
  labs(title="Las 100 palabras más usadas en La Casa de Papel",
       subtitle="Temporadas 1-3",
       caption = "Código: https://github.com/GuillemSalazar/desafio_30_dias_de_graficos\ntwitter: @GuillemSalazar") +
  theme(plot.title = element_text(hjust=0.5),
        plot.subtitle = element_text(hjust=0.5))

# Guardar gráfico
ggsave(filename = "../images/dia_22.png",g,width = twitter_dim$width,height = twitter_dim$height)

