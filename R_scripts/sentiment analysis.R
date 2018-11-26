# drop first quantile 
# use line/30

library(RMySQL)
mydb = dbConnect(MySQL(), user='mxd', dbname='IMDB', host='vcm-5368.vm.duke.edu')

dbListTables(mydb)
subtitles = dbReadTable(mydb, name = 'subtitles')

dbSendQuery(mydb, "SELECT COUNT (DISTINCT tconst) FROM subtitles GROUP BY tconst")
query1 <- dbGetQuery(mydb, "SELECT COUNT(*) FROM subtitles GROUP BY tconst")
summary(query1)


library(tidytext)
library(dplyr)
library(tidyr)
actual_movies = merge(x = corpus, y= movies)
movies = read.delim(file = 'tconst_genres.txt', header = F, sep = '\t', col.names = c('tconst', 'Genre_1', 'Genre_2', 'Genre_3'))


#movie1 <- dbGetQuery(mydb, "SELECT dialogue FROM subtitles WHERE tconst='tt0035423'")

m1 <- dbGetQuery(mydb, "SELECT subid, dialogue FROM subtitles WHERE tconst='tt0243585'")
nrow(m1)


m1[,3]<-seq(1, nrow(m1), by = 1)
summary(m1)
names(m1)<-c('subid','dialogue','show_at')
head(m1)

m1[,4]<-seq(1, nrow(m1), by = 1)

names(m1)<-c('subid','dialogue','show_at', 'show_at2')
m1$show_at2<-m1$show_at%/%30

head(m1)


library(tidytext)
library(dplyr)
tidy_m1<- m1 %>%
  select(show_at2,dialogue) %>%
  unnest_tokens("word", dialogue) %>%
  anti_join(stop_words)

m1_sentiment <- tidy_m1 %>%
  inner_join(get_sentiments("bing")) %>%
  count(show_at2, word)


head(m1_sentiment)

####positive words
m1_sentiment_plot <-
  tidy_m1 %>%
  inner_join(get_sentiments("bing")) %>% 
  filter(sentiment=="positive") %>%
  count(show_at2, sentiment)

library(ggplot2)
ggplot(m1_sentiment_plot, aes(x=show_at2, y=n))+
  geom_line(color="black",size=1)+
  ylab("Frequency of Positive Words in m1")+
  xlab("Minute")
###



####negative words
m1_sentiment_plot <-
  tidy_m1 %>%
  inner_join(get_sentiments("bing")) %>% 
  filter(sentiment=="negative") %>%
  count(show_at2, sentiment)

ggplot(m1_sentiment_plot, aes(x=show_at2, y=n))+
  geom_line(color="coral",size=0.7)+
  ylab("Frequency of Negative Words in Knight")+
  xlab("line/40") +
  ggtitle("m1")+
  theme(plot.title = element_text(hjust = 0.5,color="coral", size=14, face="bold.italic"),
        axis.title.x = element_text(color="coral",face="bold"),
        axis.title.y = element_text(color="coral",face="bold"))

#########################################
##General trend
m1_sentiment_plot_all <-
  tidy_m1 %>%
  inner_join(get_sentiments("bing")) %>% 
  count(show_at2, sentiment) %>% 
  spread(sentiment, n, fill =0) %>% 
  mutate(sentiment = positive - negative)

ggplot(m1_sentiment_plot_all, aes(x=show_at2, y = sentiment))+
  geom_col(show.legend = FALSE, fill="coral")+
  ylab("Sentiment of words in The m1")+
  xlab("line/40")+
  ggtitle("m1")+
  theme(plot.title = element_text(hjust = 0.5,color="coral", size=14, face="bold.italic"),
        axis.title.x = element_text(color="coral",face="bold"),
        axis.title.y = element_text(color="coral",face="bold"))

###################################################
##Wordcloud
library(RColorBrewer)
library(wordcloud)
library(reshape2)

tidy_knight %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  acast(word ~ sentiment, value.var = "n", fill = 0) %>%
  comparison.cloud(random.order=FALSE, colors = c("orangered4", "coral"))