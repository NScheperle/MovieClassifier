library(RMySQL)
library(tidytext)
library(dplyr)
library(tidyr)

mydb = dbConnect(MySQL(), user='mxd', dbname='IMDB', host='vcm-5368.vm.duke.edu')
dbListTables(mydb)

dbListFields(mydb, "subtitles")
subtitles<-dbReadTable(mydb, "subtitles")
nm<-as.data.frame(table(subtitles$tconst))
names(nm)<-c('tid','freq')
nm$tid<-as.character(nm$tid)


movie_ratings<-merge(sentiment, ratings, by = "tconst")
movie_box<-merge(sentiment, budget_rating, by = "tconst")
summary(movie_ratings$numVotes)
movie_ratings<-movie_ratings[movie_ratings$numVotes>=20000,]
movie_ratings<-movie_ratings[,-43]
summary(movie_box$gross_usa)
movie_box<-movie_box[movie_box$gross_usa>=5000000,]
movie_box<-movie_box[,c(1:41,44)]
movie_box<-na.omit(movie_box)



budget_rating<-dbReadTable(mydb, "budget_rating")
ratings<-dbReadTable(mydb, "ratings")
cluster_genre<-dbReadTable(mydb, "cluster_genre")

ngroup=40

flag = 1
flagmovie = 1

movieid<-nm[1,1]
movielen<-nm[1,2]
if (movielen>=925 && movielen <=2000){
  moviesubs<-subtitles[flag:(flag+movielen-1),1:3]
  g=movielen %/% ngroup +1
  moviesubs$group <- moviesubs$subID %/% g+1
  
  tidy_subs<- moviesubs %>%
    select(group,dialogue) %>%
    unnest_tokens("word", dialogue)
  
  subs_sentiment <- tidy_subs %>%
    inner_join(get_sentiments("bing")) %>%
    count(word, sort= TRUE)
  
  subs_sentvec <-
    tidy_subs %>%
    anti_join(stop_words) %>% 
    inner_join(get_sentiments("bing")) %>% 
    count(group, sentiment) %>% 
    spread(sentiment, n, fill =0) %>% 
    mutate(sentiment = positive - negative)
  
  tempsent<-as.data.frame(t(c(movieid,as.matrix(subs_sentvec$sentiment))))
  flagmovie = flagmovie +1
  flag=flag+movielen
  }
}

for (i in flagmovie:nrow(nm)) {
  movieid<-nm[i,1]
  movielen<-nm[i,2]
  if (movielen>=925 && movielen <=2000){
    moviesubs<-subtitles[flag:(flag+movielen-1),1:3]
    g=movielen %/% ngroup +1
    moviesubs$group <- moviesubs$subID %/% g+1
  
    tidy_subs<- moviesubs %>%
      select(group,dialogue) %>%
      unnest_tokens("word", dialogue)
  
    subs_sentiment <- tidy_subs %>%
      inner_join(get_sentiments("bing")) %>%
      count(word, sort= TRUE)
  
    subs_sentvec <-
      tidy_subs %>%
      anti_join(stop_words) %>% 
      inner_join(get_sentiments("bing")) %>% 
      count(group, sentiment) %>% 
      spread(sentiment, n, fill =0) %>% 
      mutate(sentiment = positive - negative)
  
    if (nrow(subs_sentvec)==39) {
      subs_sentvec<-rbind(subs_sentvec,c(40,0,0,0))
    }
    tempsent2<-as.data.frame(t(c(movieid,as.matrix(subs_sentvec$sentiment))))
    tempsent<-rbind(tempsent,tempsent2)
    flagmovie = flagmovie +1
    flag=flag+movielen
  } else {
    flagmovie = flagmovie +1
    flag=flag+movielen
  }

}
subs_sentvec<-rbind(subs_sentvec,c(35,0,0,0))
subs_sentvec<-subs_sentvec[order(subs_sentvec$group),]

flagmovie = flagmovie +1
flag=flag+movielen

names(tempsent)<-c('tconst','vec1','vec2','vec3','vec4','vec5','vec6','vec7','vec8','vec9','vec10',
                   'vec11','vec12','vec13','vec14','vec15','vec16','vec17','vec18','vec19','vec20',
                   'vec21','vec22','vec23','vec24','vec25','vec26','vec27','vec28','vec29','vec30',
                   'vec31','vec32','vec33','vec34','vec35','vec36','vec37','vec38','vec39','vec40')
for(i in 2:41){
  sentiment[,i] <- as.numeric(sentiment[,i])
}

dbListTables(mydb)
dbWriteTable(mydb, "sentiment", tempsent, overwrite = TRUE, append = FALSE,field.types)
sentiment<-dbReadTable(mydb,'sentiment')

