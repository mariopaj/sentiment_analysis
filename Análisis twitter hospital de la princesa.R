
library(stringr)
library(tidytext)
library(ggplot2)
library(quanteda)
library(purrr)
library(rtweet)
library(tidyverse) 
library(knitr)
library(tokenizers)
library(tm)
library(wordcloud)
library(RColorBrewer)
library(igraph)
library(ggraph)
library(widyr)
library(syuzhet)
library(rjson)
library(usethis)
library(devtools)
library(bit64)
library(httr)
library(twitteR)
library(udpipe)
library(plyr)
library(httpuv)
library(syuzhet)
library(RCurl)


# Instalainstall.packages("tm")r http: // si aún no lo ha hecho

if (!requireNamespace("httpuv", quietly = TRUE)) {
  install.packages("httpuv")
}

## Almacenar claves de API 

api_key <- "tQoqA6RMrsd123t0PQjWz4TQh"
api_secret_key <- "UOIw3w6bs8lOIijYEVSIK7bAoyArINx2NVmPRNTlUhrJ64ZUsy"
access_token <- "1366714869601480711-yUpgG29Ml0xgIMzuHa1njn2fYpg4oX"
access_token_secret <- "yQpJYc5j1L8GxvH3schnLPSb4198lDoR5IzqiAmZ53CPJ"

## Autentificarse a través del navegador web

token <- create_token(
  app = "textmininghospitallaprincesa",
  consumer_key = api_key,
  consumer_secret = api_secret_key,
  access_token = access_token,
  access_secret = access_token_secret)

## Verificamos

get_token() # Coincide

###### BUSQUEDA DE TWEETS

hosp_princesa_tweets <- get_timeline("@HULPrincesa", n = 3200)
length(hosp_princesa_tweets$text) #3143 tweets
colnames(hosp_princesa_tweets)

###### MUESTRA DE LO QUE FUNCIONA MEJOR Y LO QUE NO

## Eliminamos retweets

hosp_princesa_tweets_organic <- hosp_princesa_tweets[hosp_princesa_tweets$is_retweet == F,]

## Eliminamos respuestas

hosp_princesa_tweets_organic <- subset(hosp_princesa_tweets_organic, 
                                       is.na(hosp_princesa_tweets_organic$reply_to_status_id))

length(hosp_princesa_tweets_organic$text) # 1618 tweets orgánicos

## Observamos el nº de me gustas y retweets por orden

hosp_princesa_tweets_organic <- hosp_princesa_tweets_organic[with(hosp_princesa_tweets_organic, order(-hosp_princesa_tweets_organic[,13])),]

head(hosp_princesa_tweets_organic$text, 5) # tweets con más nº de favoritos

hosp_princesa_tweets_organic_2 <- hosp_princesa_tweets_organic[with(hosp_princesa_tweets_organic, order(-hosp_princesa_tweets_organic[,14])),]

hosp_princesa_tweets_organic_2[1,5] # tweet con más nº de retweets

###### MOSTRAR LA PROPORCIÓN RESPUESTA/ RETWEETS/ TWEETS ORGÁNICOS

## Mantener solo los retweets

hosp_princesa_retweets <- hosp_princesa_tweets [hosp_princesa_tweets$is_retweet == T,]

length(hosp_princesa_retweets$text) # 1464 retweets

## Mantener solo las respuestas

hosp_princesa_replies <- subset(hosp_princesa_tweets, 
                                ! is.na(hosp_princesa_tweets$reply_to_status_id))

length(hosp_princesa_replies$text) # 61 respuestas

## Creamos un marco de datos que contenga tweets orgánicos, respuestas y retweets

datos_tweets <- data.frame(type_of_tweet = c("Orgánico", "Retweets", "Respuestas"),
                           count = c(1618, 1464, 61))

datos_tweets <- datos_tweets %>% mutate(fraccion = round(count/sum(count), 2),
                                        percentage = round(count/sum(count)*100, 2),
                                        ymax = cumsum(fraccion),
                                        ymin = c (0, head(ymax, n = -1))
                                        )

## Especificamos lo que dice la leyenda 

Type_of_tweet <- paste (datos_tweets$type_of_tweet, datos_tweets$percentage, "%")

ggplot (datos_tweets, aes(ymax = ymax, ymin = ymin, xmax = 4, xmin = 3, fill = Type_of_tweet)) + 
  geom_rect () + 
  coord_polar (theta = "y") + 
  xlim (c (2, 4)) + 
  theme_void () + 
  theme (legend.position = "right")

###### MOSTRAMOS CUANDO SE PUBLICAN LOS TWEETS

colnames (hosp_princesa_tweets) [colnames (hosp_princesa_tweets) == "screen_name"] <- "Twitter_Account"
ts_plot (group_by (hosp_princesa_tweets, Twitter_Account), "month") + 
 theme_minimal () + 
 theme (plot.title = element_text (face = "bold")) + 
 labs ( 
    x = NULL, y = NULL, 
    title = "Frecuencia de tweets de HULPrincesa", 
    subtitle = "Recuento de tweets agregados por mes", caption = "\ nFuente: datos recopilados de la API REST de Twitter a través de rtweet" 
  )

###### MUESTRA DESDE DONDE SE PUBLICAN LOS TWEETS

hosp_princesa_app <- hosp_princesa_tweets %>% 
  select (source)%>% 
  group_by (source)%>% 
  summarise (count = n ()) # IFTTT automatizar los tweets que se mandan

hosp_princesa_app <- hosp_princesa_app %>% mutate(fraccion = round(count/sum(count), 2),
                                        percentage = round(count/sum(count)*100, 2),
                                        ymax = cumsum(fraccion),
                                        ymin = c (0, head(ymax, n = -1))
                                        )


Source <- paste(hosp_princesa_app$source, hosp_princesa_app$percentage, "%")

ggplot (hosp_princesa_app, aes (ymax = ymax, ymin = ymin, xmax = 4, xmin = 3, fill = Source)) + 
  geom_rect () + 
  coord_polar (theta = "y") + 
  xlim (c (2, 4)) + 
  theme_void () + 
  theme (legend.position = "right")

###### MOSTRAR LAS PALABRAS MÁS FRECUENTES QUE SE ENCUENTRAN EN LOS TWEETS

## Limpieza de tweets

hosp_princesa_tweets_organic$text <- gsub("http.*", " ", hosp_princesa_tweets_organic$text)
hosp_princesa_tweets_organic$text <- gsub("[[:cntrl:]]", " ", hosp_princesa_tweets_organic$text) 
hosp_princesa_tweets_organic$text <- gsub("@ \\ S *", " ", hosp_princesa_tweets_organic$text) 
hosp_princesa_tweets_organic$text <- tolower(hosp_princesa_tweets_organic$text)
hosp_princesa_tweets_organic$text <- removePunctuation(hosp_princesa_tweets_organic$text)
hosp_princesa_tweets_organic$text <- removeNumbers(hosp_princesa_tweets_organic$text)

head(hosp_princesa_tweets_organic$text)

## Separamos los tweets por plabras

words_tweets <- hosp_princesa_tweets_organic %>% 
               select(text) %>%
               unnest_tokens(word, text)


## Eliminamos las stop words

lista_stopwords <- stopwords("spanish")

words_tweets <- words_tweets %>% filter(!(word %in% lista_stopwords))

words_tweets <- words_tweets %>% filter(word != "q")

words_tweets %>% count(word, sort = T) %>%
  top_n(15) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(x = word, y = n)) +
  geom_col(fill = "lightblue", col = "black") +
  xlab(NULL) +
  coord_flip() +
  labs(y = "Nº de veces que se repite la palabra",
       x = "Palabras",
       title = "Palabras más frecuentes en la cuenta @HULPrincesa")

## wordscloud

words_tweets_count <- words_tweets %>% count(word, sort = T)

wordcloud(words = words_tweets_count$word, freq = words_tweets_count$n,scale = c(2.5, .5) ,max.words = 100, 
          random.order = F, rot.per = 0.35,
          colors = brewer.pal(8, "Dark2"))

###### HASHTAGS QUE SE UTILIZAN CON MÁS FRECUENCIA

hosp_princesa_tweets_organic$hashtags <- as.character(hosp_princesa_tweets_organic$hashtags)

wordcloud (hosp_princesa_tweets_organic$hashtags, min.freq = 5, scale = c (3.5, .5), 
           random.order = F, rot.per = 0.35, 
           colors = brewer.pal (8, "Dark2"))

###### ANÁLISIS DE SENTIMIENTOS

## Conversión de tweets a ASCII para rastrear tweets de caracteres extraños

tweets <-iconv(words_tweets, from="UTF-8", to="ASCII", sub="")

## Eliminamos retweets en caso de que sea necesario

tweets <- gsub("(RT|via)((?:\\b\\w*@\\w+)+)","",words_tweets)

## Eliminamos menciones en caso de que sea necesario
  
tweets <-gsub("@\\w+","",tweets)

ew_sentiment<-get_nrc_sentiment((tweets))
sentimentscores<-data.frame(colSums(ew_sentiment[,]))

names(sentimentscores) <- "Score"

sentimentscores <- cbind("sentiment" = rownames(sentimentscores),sentimentscores)
rownames(sentimentscores) <- NULL

ggplot(data=sentimentscores,aes(x=sentiment,y=Score))+
  geom_bar(aes(fill=sentiment),stat = "identity")+
  theme(legend.position="none")+
  xlab("Sentimientos")+ylab("Puntuación")+
  ggtitle("Sentimientos totales")+
  theme_minimal()

## Proporción de sentimientos positivos y negativos

sentimientos_words <- read.csv("lexico_afinn.csv", stringsAsFactors = F, fileEncoding = "latin1") %>% 
  tbl_df()

sentimientos_words$word <- NULL

colnames(words_tweets)[1] <- "palabra"
words_tweets <- count(words_tweets, palabra)

sentimientos_words <- inner_join(x = words_tweets, y = sentimientos_words)

sentimientos_words <- sentimientos_words %>% mutate(total_feelings = n*puntuacion,
                                                    Tipo = ifelse(puntuacion > 0, "Positiva", 
                                                                  ifelse(puntuacion < 0, "Negativa", "Neutra")
                                                    )
                                                    )


## Representamos todas las palabras juntas

sentimientos_words <- sentimientos_words %>% 
  mutate(abs_total_felings = abs(total_feelings))

sentimientos_words %>% 
  group_by(Tipo) %>% 
  summarise(proportion =(sum(abs_total_felings)/3866)*100) %>% 
  filter(Tipo != "Neutra") %>%
  ggplot(aes(x = Tipo, y = proportion)) + 
  geom_col(aes(fill = Tipo)) + 
  labs(x = "Connotación palabras",
       y = "Proporción sentimientos (%)",
       title = "Comparación sentimientos positivos vs negativos")

##### MENCIONES E HILOS

mentions_princesa <- search_tweets(q = "@HULPrincesa", n = 1000, include_rts = F)
mentions_princesa$text # No hay suficiente información para analizar

text_princesa <- search_tweets(q = "hospitaldelaprincesa",n = 1000, include_rts = F)
text_princesa$text # No hay suficiente información para analizar

## Capturamos tweets durante un tiempo de un tema

termino_de_busqueda_1 <- "hospitaldelaprincesa"
termino_de_busqueda_2 <- "@HULPrincesa"

tiempo <- 60 * 60 * 24 * 60 # 2 meses

archivo_1 <- "busqueda_tweets1.json"
archivo_2 <- "busqueda_tweets2.json"

## Almacenamos en una base de datos

stream_tweets(q = termino_de_busqueda_1,
              timeout = tiempo,
              file_name = archivo_1,
              parse = FALSE)           

stream_tweets(q = termino_de_busqueda_2,
              timeout = tiempo,
              file_name = archivo_2,
              parse = FALSE)
