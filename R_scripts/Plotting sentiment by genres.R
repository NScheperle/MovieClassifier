library(RMySQL)
library(tidyverse)
library(tidytext)

mydb = dbConnect(MySQL(), user = 'bl199', dbname ='IMDB', host = 'vcm-5368.vm.duke.edu')
dbListTables(mydb)

##Preprocessing
#sentiment
genre = dbReadTable(mydb, name = 'cluster_genre')
sentiment = dbReadTable(mydb, name = 'sentiment')
sentiment[, c(2:41)] <- sapply(sentiment[, c(2:41)], as.numeric)
sentiment<- sentiment[-c(1), ]
#genre table
tconst_genres <- read.delim("C:/Users/Echo Liu/Downloads/Duke University/1st semester/704_Data_Scraping/Team_project_movie/tconst_genres.txt", header=FALSE)
names(tconst_genres) <- c("tconst", "genre1","genre2","genre3")
tconst_genres[, c(1:4)] <- sapply(tconst_genres[, c(1:4)], as.character)
#left-join
sentiment_bytype <- sentiment %>% 
  left_join(tconst_genres, by ="tconst") %>% 
  arrange(genre1)

sentiment_bytype %>%
  summarise_each(funs(n_distinct))
table(sentiment_bytype$genre1)

##convert from long table to wide table
plain_long_table <- sentiment_bytype %>% select(-one_of(c("genre1", "genre2", "genre3")))
plain_long_table <- plain_long_table[,-c(1)]
plain_wide_table <- as.data.frame(t(plain_long_table))
colnames(plain_wide_table) <- sentiment_bytype$tconst
plain_wide_table$show_at <- c(1:40)

###########################################################################
##Step2: Plotting
#Action Movie (plot 20 lines on one plot)
action_movie <- plain_wide_table[,c(1:135)]
action_movie$average <- rowMeans(action_movie)
action_movie$bin <- c(1:40)
am_m <- data.matrix(action_movie, rownames.force = NA)
am_m <- am_m[,c(1:20)]
action_movie_m <- melt(am_m)
ggplot() +
  geom_line(data = action_movie_m, aes(x = Var1, y = value, group = Var2, color = Var2))+
  ggtitle("Action Movie sentiment plot")

ggplot() +
  geom_line(data = action_movie, aes(x = bin, y = average, color = "tomato1"))+
  ggtitle("Action Movie sentiment plot (average)")

#Adventure Movie
adventure_movie <- plain_wide_table[,c(136:193)]
adventure_movie$average <- rowMeans(adventure_movie)
adventure_movie$bin <- c(1:40)
ad_m <- data.matrix(adventure_movie, rownames.force = NA)
ad_m <- ad_m[,c(1:20)]
adventure_movie_m <- melt(ad_m)
ggplot() +
  geom_line(data = adventure_movie_m, aes(x = Var1, y = value, group = Var2, color = Var2))+
  ggtitle("Adventure Movie sentiment plot")

ggplot() +
  geom_line(data = adventure_movie, aes(x = bin, y = average, color = "tomato1"))+
  ggtitle("Adventure Movie sentiment plot (average)")

#Comedy Movie
comedy_movie <- plain_wide_table[,c(230:452)]
comedy_movie$average <- rowMeans(comedy_movie)
comedy_movie$bin <- c(1:40)
co_m <- data.matrix(comedy_movie, rownames.force = NA)
co_m <- co_m[,c(1:20)]
comedy_movie_m <- melt(co_m)
ggplot() +
  geom_line(data = comedy_movie_m, aes(x = Var1, y = value, group = Var2, color = Var2))+
  ggtitle("Comedy Movie sentiment plot")

ggplot() +
  geom_line(data = comedy_movie, aes(x = bin, y = average, color = "tomato1"))+
  ggtitle("Comedy Movie sentiment plot (average)")

#Crime Movie
crime_movie <- plain_wide_table[,c(453:515)]
crime_movie$average <- rowMeans(crime_movie)
crime_movie$bin <- c(1:40)
cr_m <- data.matrix(crime_movie, rownames.force = NA)
cr_m <- cr_m[,c(1:20)]
crime_movie_m <- melt(cr_m)
ggplot() +
  geom_line(data = crime_movie_m, aes(x = Var1, y = value, group = Var2, color = Var2))+
  ggtitle("Crime Movie sentiment plot")

ggplot() +
  geom_line(data = crime_movie, aes(x = bin, y = average, color = "tomato1"))+
  ggtitle("Crime Movie sentiment plot (average)")


#Drama Movie
drama_movie <- plain_wide_table[,c(516:635)]
drama_movie$average <- rowMeans(drama_movie)
drama_movie$bin <- c(1:40)
dr_m <- data.matrix(drama_movie, rownames.force = NA)
dr_m <- dr_m[,c(1:20)]
drama_movie_m <- melt(dr_m)
ggplot() +
  geom_line(data = drama_movie_m, aes(x = Var1, y = value, group = Var2, color = Var2))+
  ggtitle("Drama Movie sentiment plot")

ggplot() +
  geom_line(data = drama_movie, aes(x = bin, y = average, color = "tomato1"))+
  ggtitle("Drama Movie sentiment plot (average)")



#Horror Movie
horror_movie <- plain_wide_table[,c(637:653)]
horror_movie$average <- rowMeans(horror_movie)
horror_movie$bin <- c(1:40)
ho_m <- data.matrix(horror_movie, rownames.force = NA)
ho_m <- dr_m[,c(1:20)]
horror_movie_m <- melt(ho_m)
ggplot() +
  geom_line(data = horror_movie_m, aes(x = Var1, y = value, group = Var2, color = Var2))+
  ggtitle("Horror Movie sentiment plot")

ggplot() +
  geom_line(data = horror_movie, aes(x = bin, y = average, color = "tomato1"))+
  ggtitle("Horror Movie sentiment plot (average)")

#Mystery Movie
mystery_movie <- plain_wide_table[,c(654:659)]
mystery_movie$average <- rowMeans(mystery_movie)

##############################################################################
##Step 3: Mean Plotting





