library(RMySQL)
library(tidyverse)
library(tidytext)
library(reshape2)

library(SnowballC)
library(topicmodels)
library(tidytext)
library(dplyr)
library(ggplot2)
library(readtext)

mydb = dbConnect(MySQL(), user = 'jh608', dbname ='IMDB', host = 'vcm-5368.vm.duke.edu')
dbListTables(mydb)

names<- dbReadTable(mydb, name = 'names')
names

subtitles <- dbReadTable(mydb, name = 'subtitles')

dbSendQuery(mydb, "SELECT COUNT(DISTINCT tconst) FROM subtitles GROUP BY tconst")
query10 <- dbGetQuery(mydb, "SELECT COUNT(*) FROM subtitles GROUP BY tconst")
summary(query10)





library(tidytext)
library(dplyr)
library(tidyr)


#movie1 <- dbGetQuery(mydb, "SELECT dialogue FROM subtitles WHERE tconst='tt0035423'")

movi <- dbGetQuery(mydb, "SELECT subid, dialogue FROM subtitles GROUP BY tconst")
nrow(movi)




##Preprocessing
#sentiment
sentiment = dbReadTable(mydb, name = 'sentiment')
sentiment[, c(2:41)] <- sapply(sentiment[, c(2:41)], as.numeric)
sentiment<- sentiment[-c(1), ]
# cluster_genre
cluster = dbReadTable(mydb, name = 'cluster_genre')
cluster[, c(1:5)] <- sapply(cluster[, c(1:5)], as.character)



#left-join
sentiment_bytype <- sentiment %>% 
  left_join(tconst_genres, by ="tconst") %>% 
  arrange(genre1)

sentiment_bytype %>%
  summarise_each(funs(n_distinct))
table(sentiment_bytype$genre1)





#left-join
sentiment_bycluster <- sentiment %>% 
  left_join(cluster, by ="tconst") %>% 
  arrange(CLUSTER)
cluster2 = sentiment_bycluster

cluster2 %>%
  summarise_each(funs(n_distinct))
table(cluster2$CLUSTER)

cluster2

##convert from long table to wide table
plain_long_table <- cluster2 %>% select(-one_of(c("Genre_1", "Genre_2", "Genre_3", "CLUSTER")))
plain_long_table <- plain_long_table[,-c(1)]
plain_wide_table <- as.data.frame(t(plain_long_table))
colnames(plain_wide_table) <- cluster2$tconst
plain_wide_table$show_at <- c(1:40)


#cluster1=c('Drama','Comedy','Romance','Family','Sport','Musical')
#cluster2=c('Action','Western','War')
#cluster3=c('Sci-Fi','Adventure','Fantasy','Animation')
#cluster4=c('Crime','Mystery','Thriller')
#cluster5=c('Horror')

#Cluster1 (plot 20 lines on one plot)
Cluster1 <- plain_wide_table[,c(1:404)]
Cluster1$average <- rowMeans(Cluster1)
Cluster1$bin <- c(1:40)
c1_m <- data.matrix(Cluster1, rownames.force = NA)
c1_m <- c1_m[,c(1:20)]
c1_m_m <- melt(c1_m)
ggplot() +
  geom_line(data = c1_m_m, aes(x = Var1, y = value, group = Var2, color = Var2))+
  ggtitle("Cluster1 sentiment plot")

ggplot() +
  geom_line(data = Cluster1, aes(x = bin, y = average, color = "red"))+
  ggtitle("Cluster1 sentiment plot (average), ('Drama','Comedy','Romance','Family','Sport','Musical')")

#Cluster2 (plot 20 lines on one plot)
Cluster2 <- plain_wide_table[,c(405:406)]
Cluster2$average <- rowMeans(Cluster2)
Cluster2$bin <- c(1:40)
c2_m <- data.matrix(Cluster2, rownames.force = NA)
c2_m <- c2_m[,c(1:20)]
c2_m_m <- melt(c2_m)
ggplot() +
  geom_line(data = c2_m_m, aes(x = Var1, y = value, group = Var2, color = Var2))+
  ggtitle("Cluster2 sentiment plot")

ggplot() +
  geom_line(data = Cluster2, aes(x = bin, y = average, color = "red"))+
  ggtitle("Cluster2 sentiment plot (average), ('Action','Western','War')")

#Cluster3 (plot 20 lines on one plot)
Cluster3 <- plain_wide_table[,c(407:474)]
Cluster3$average <- rowMeans(Cluster3)
Cluster3$bin <- c(1:40)
c3_m <- data.matrix(Cluster3, rownames.force = NA)
c3_m <- c3_m[,c(1:20)]
c3_m_m <- melt(c3_m)
ggplot() +
  geom_line(data = c3_m_m, aes(x = Var1, y = value, group = Var2, color = Var2))+
  ggtitle("Cluster3 sentiment plot")

ggplot() +
  geom_line(data = Cluster3, aes(x = bin, y = average, color = "red"))+
  ggtitle("Cluster3 sentiment plot (average), ('Sci-Fi','Adventure','Fantasy','Animation')")

#Cluster4 (plot 20 lines on one plot)
Cluster4 <- plain_wide_table[,c(407:559)]
Cluster4$average <- rowMeans(Cluster4)
Cluster4$bin <- c(1:40)
c4_m <- data.matrix(Cluster4, rownames.force = NA)
c4_m <- c4_m[,c(1:20)]
c4_m_m <- melt(c4_m)
ggplot() +
  geom_line(data = c4_m_m, aes(x = Var1, y = value, group = Var2, color = Var2))+
  ggtitle("Cluster4 sentiment plot")

ggplot() +
  geom_line(data = Cluster4, aes(x = bin, y = average, color = "red"))+
  ggtitle("Cluster4 sentiment plot (average), ('Crime','Mystery','Thriller')")


#Cluster5 (plot 20 lines on one plot)
Cluster5 <- plain_wide_table[,c(559:562)]
Cluster5$average <- rowMeans(Cluster5)
Cluster5$bin <- c(1:40)
c5_m <- data.matrix(Cluster4, rownames.force = NA)
c5_m <- c5_m[,c(1:20)]
c5_m_m <- melt(c5_m)
ggplot() +
  geom_line(data = c5_m_m, aes(x = Var1, y = value, group = Var2, color = Var2))+
  ggtitle("Cluster5 sentiment plot")

ggplot() +
  geom_line(data = Cluster5, aes(x = bin, y = average, color = "red"))+
  ggtitle("Cluster5 sentiment plot (average), ('Horror')")

