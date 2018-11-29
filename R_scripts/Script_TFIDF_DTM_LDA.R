
# I think it ill be more smart use a python script. I will Querry every tcosnt I have and save in different files with the tcosnt.
# In this way I could save every file in an R object and wont kill my machine. Better 925 heavy rows than 6000000+ rows to iterate
# I USE THE get_subtitles_by_tconst.py script to do this

# I need a query to get all my genre right now. 
# #mysql IMDB --execute="select distinct(subtitles.tconst), titles.genres from subtitles 
# left join titles on subtitles.tconst = titles.tconst" > '/home/jm622/tconst_genre.csv'

install.packages('dplyr')
install.packages('stm')
install.packages('SnowballC')
install.packages('topicmodels')
install.packages('tidytext')
install.packages('dplyr')
install.packages('ggplot2')
install.packages('readtext')
install.packages('cleanNLP')
install.packages('udpipe')
#############
library(dplyr)
library(stm)
library(SnowballC)
library(topicmodels)
library(tidytext)
library(ggplot2)
library(readtext)
library(cleanNLP)
cnlp_init_udpipe()


setwd('C:/Users/joaqu/OneDrive/Escritorio/MovieClassifier/R_scripts/')
#corpus = read.delim(file = 'test_sql.txt' , header = T, stringsAsFactors = F, sep = '\t') Older aproach

# NEw APPROACH!
files <- list.files(path="C:/Users/joaqu/OneDrive/Escritorio/Text_Analysis-master/Text_Analysis/python/subtitles", pattern="*.txt", full.names=TRUE, recursive=FALSE)
test_file = readtext::readtext(files[1])
raw_scripts = as.data.frame(test_file)
for (i in files[-1]){ script_file = readtext::readtext(i)
raw_scripts = rbind(raw_scripts,script_file)
}

corpus = raw_scripts
colnames(corpus) = c('tconst','text')
corpus$tconst <- gsub(".txt",'', corpus$tconst)  #Removing the .txt to have the tconst.
corpus$tconst = as.factor(corpus$tconst) #convert to factor


# Create a dataframe with genre to add to the corpus -----
# This .txt is from the .csv. I need to modify by hand to adjust the format. USE th TEMPLATE file to do so.
movies = read.delim(file = 'tconst_genres.txt', header = T, sep = ',', col.names = c('tconst', 'Genre_1', 'Genre_2', 'Genre_3'))
levels(as.factor(corpus$tconst)) #924 movies

#Merge scripts with Movie data----
actual_movies = merge(x = corpus, y= movies)



#Categorizing my movies in Cluster (Shah,Motiani & Patel, [https://sahebmotiani.com/Movie%20Clustering.pdf])----

cluster1=c('Drama','Comedy','Romance','Family','Sport','Musical')
cluster2=c('Action','Western','War')
cluster3=c('Sci-Fi','Adventure','Fantasy','Animation')
cluster4=c('Crime','Mystery','Thriller')
cluster5=c('Horror')

actual_movies <- actual_movies %>%
  mutate(Genre_1 = as.factor(Genre_1),
         Genre_2 = as.factor(Genre_2),
         Genre_3 = as.factor(Genre_3))

# I need to create new columns to put the cluster each genre is in

actual_movies$cluster_gen1 = 0
actual_movies$cluster_gen2 = 0
actual_movies$cluster_gen3 = 0

in.genre <- function(dataset,genre.col,cluster,cluster.col.name, number_cluster) {for (i in seq(0,length(dataset[[genre.col]]))){
  if (length(dataset[i,][[genre.col]]) > 0){
    if (as.character(dataset[i,][[genre.col]]) %in% cluster){
      dataset[i,][[cluster.col.name]] = number_cluster}}}
  return(dataset)
}

actual_movies = in.genre(actual_movies,genre.col = 'Genre_1',cluster1,cluster.col.name = 'cluster_gen1', 1)
actual_movies = in.genre(actual_movies,genre.col = 'Genre_1',cluster2,cluster.col.name = 'cluster_gen1', 2)
actual_movies = in.genre(actual_movies,genre.col = 'Genre_1',cluster3,cluster.col.name = 'cluster_gen1', 3)
actual_movies = in.genre(actual_movies,genre.col = 'Genre_1',cluster4,cluster.col.name = 'cluster_gen1', 4)
actual_movies = in.genre(actual_movies,genre.col = 'Genre_1',cluster5,cluster.col.name = 'cluster_gen1', 5)

actual_movies = in.genre(actual_movies,genre.col = 'Genre_2',cluster1,cluster.col.name = 'cluster_gen2', 1)
actual_movies = in.genre(actual_movies,genre.col = 'Genre_2',cluster2,cluster.col.name = 'cluster_gen2', 2)
actual_movies = in.genre(actual_movies,genre.col = 'Genre_2',cluster3,cluster.col.name = 'cluster_gen2', 3)
actual_movies = in.genre(actual_movies,genre.col = 'Genre_2',cluster4,cluster.col.name = 'cluster_gen2', 4)
actual_movies = in.genre(actual_movies,genre.col = 'Genre_2',cluster5,cluster.col.name = 'cluster_gen2', 5)

actual_movies = in.genre(actual_movies,genre.col = 'Genre_3',cluster1,cluster.col.name = 'cluster_gen3', 1)
actual_movies = in.genre(actual_movies,genre.col = 'Genre_3',cluster2,cluster.col.name = 'cluster_gen3', 2)
actual_movies = in.genre(actual_movies,genre.col = 'Genre_3',cluster3,cluster.col.name = 'cluster_gen3', 3)
actual_movies = in.genre(actual_movies,genre.col = 'Genre_3',cluster4,cluster.col.name = 'cluster_gen3', 4)
actual_movies = in.genre(actual_movies,genre.col = 'Genre_3',cluster5,cluster.col.name = 'cluster_gen3', 5)

# I need to make a system to classify my genre 
# I will take only movies that belong to one cluster.
actual_movies$CLUSTER = NA
largo = length(actual_movies$CLUSTER)

for (i in seq(1,largo)){
  if(actual_movies[i,]$cluster_gen2 <=0 & actual_movies[i,]$cluster_gen3 <=0){
    actual_movies[i,]$CLUSTER = actual_movies[i,]$cluster_gen1}
  if(actual_movies[i,]$cluster_gen1 <=0 & actual_movies[i,]$cluster_gen3 <=0){
  actual_movies[i,]$CLUSTER = actual_movies[i,]$cluster_gen2}
  if(actual_movies[i,]$cluster_gen1 <=0 & actual_movies[i,]$cluster_gen2 <=0){
  actual_movies[i,]$CLUSTER = actual_movies[i,]$cluster_gen3}
  
  if(actual_movies[i,]$cluster_gen1 > 0){if (actual_movies[i,]$cluster_gen1 == actual_movies[i,]$cluster_gen2)
  {actual_movies[i,]$CLUSTER = actual_movies[i,]$cluster_gen1} else if(actual_movies[i,]$cluster_gen1 == actual_movies[i,]$cluster_gen3){
      actual_movies[i,]$CLUSTER = actual_movies[i,]$cluster_gen1}}
  
  if(actual_movies[i,]$cluster_gen2 > 0){if (actual_movies[i,]$cluster_gen2 == actual_movies[i,]$cluster_gen3){
    actual_movies[i,]$CLUSTER = actual_movies[i,]$cluster_gen2} else if(actual_movies[i,]$cluster_gen2 == actual_movies[i,]$cluster_gen1){
      actual_movies[i,]$CLUSTER = actual_movies[i,]$cluster_gen2}}
  
  if(actual_movies[i,]$cluster_gen3 > 0){if (actual_movies[i,]$cluster_gen3 == actual_movies[i,]$cluster_gen2){
    actual_movies[i,]$CLUSTER = actual_movies[i,]$cluster_gen3} else if(actual_movies[i,]$cluster_gen3 == actual_movies[i,]$cluster_gen1){
      actual_movies[i,]$CLUSTER = actual_movies[i,]$cluster_gen1}}
 }

 

#Count the lines inside each movie
actual_movies$num.lines = NA
count = 1
for (movie in actual_movies$text){actual_movies[count,]$num.lines = length(stringr::str_split(movie,pattern = '\\),')[[1]])
                            count = count + 1
                            }

#plot desce order by num.lines

numero = actual_movies %>%
  select(tconst,num.lines) %>%
  arrange((num.lines))

#lets remove shorter movies

     #actual_movies = actual_movies[actual_movies$num.lines > 800,]

# Save data unitl now
save(actual_movies, file = 'actual_movies.rda')

write.csv(actual_movies[,c(1,3:5,9)], file = 'actual_movies.csv') # THIS WOULD BE TO UPLOAD TO THE DATA BASE!
# Select only one subset of my movies ----

subset = actual_movies[!is.na(actual_movies$CLUSTER),]
subset$CLUSTER = as.factor(subset$CLUSTER)
#summary(subset)   

# Remove words starting with x9...
counter = 0
for(movie in subset$text) {counter = counter +1
subset$text[counter] = gsub(paste0(' x9[0-9]*\\w*'),' ', subset$text[counter])
}

# Try a tidy format
tidy_corpus <- subset %>%
  select(tconst,text,Genre_1, Genre_2, Genre_3, CLUSTER) %>%
  unnest_tokens("word", text) 

tidy_corpus %>% 
  count(word) %>%
  arrange(desc(n))

data("stop_words")
words = c('?', '?', "lt's", 'x92s', 'x95','?','x92t', "i'ii", 'y:i', 'i?','x92re', 'x92ll', 'x92m', "?",
          'll"',"?","it?","lt?","??","x92ve","qu?","x92d","l?","??","0h",'y?',"a'ight",'^','?',"l'm","i'?","â" ,"i'?"
          ,'^','ª','ã','å','a.a','a.b','a.b.c', "ð","î","de","ng","tð","harri","thðµ","nã","yð","hð","ll","tñ","ðµ","wð","estã","nñ","yous",
          "para","nð","fã","lð","fð","uñ","mð","det","en","dð","um","thð",'ñ','aii', 'iik','iâ','yeah','gonna', 'hey') 
#Most popular Words 
# 'dont', 'know','want', 'think', 'come', 'just', 'like', 'that', 'time', 'hey', 'yeah', 'uh', 'gonna'
dict = as.data.frame(words)
colnames(dict) = 'word'
dict$word = as.character(words)

tidy_corpus<-tidy_corpus %>%
  anti_join(stop_words)  #Can add a dicitionary if I want

tidy_corpus<-tidy_corpus %>%
  anti_join(dict) 


tidy_corpus<-tidy_corpus[-grep("\\b\\d+\\b", tidy_corpus$word),]
tidy_corpus<-tidy_corpus[-grep("\\d+", tidy_corpus$word),]

tidy_corpus$word <- gsub("\\s+","",tidy_corpus$word)

tidy_corpus<-tidy_corpus %>%
  mutate_at("word", funs(wordStem((.), language="en")))


############
#I wil try to trim all the words below than 1 word appearence
number_words = tidy_corpus %>%
  count(word) %>%
  arrange(desc(n))

number_words = number_words$word[number_words$n > 1]
summary(number_words)

tidy_corpus = tidy_corpus %>%
  filter(word %in% number_words)

tidy_corpus = tidy_corpus %>% #removing words from dictionary
  filter(word %in% words)

#tidy_corpus = tidy_corpus[c(1,6)]
###########

save(tidy_corpus, file = "Subtitles_tidy_corpus.rda")

tidy_corpus %>%
  count(word) %>%
  arrange(desc(n))

# IT-IDF -----

tidy_tfidf<- tidy_corpus %>%
  count(word, tconst) %>%
  bind_tf_idf(word, tconst, n)
save(tidy_tfidf, file = 'Subtitle_TFIDF.rda')

#Need to add CLUSTER to the IT matrix

tidy_tfidf$CLUSTER = NA
tidy_tfidf$Genre_1 = NA
tidy_tfidf$Genre_2 = NA
tidy_tfidf$Genre_3 = NA

tidy_tfidf$CLUSTER = with(actual_movies, CLUSTER[match(tidy_tfidf$tconst, tconst)])
tidy_tfidf$Genre_1 = with(actual_movies, Genre_1[match(tidy_tfidf$tconst, tconst)])
tidy_tfidf$Genre_2 = with(actual_movies, Genre_2[match(tidy_tfidf$tconst, tconst)])
tidy_tfidf$Genre_3 = with(actual_movies, Genre_3[match(tidy_tfidf$tconst, tconst)])
save(tidy_tfidf, file = 'Subtitle_TFIDF.rda')

#Cunting all my words 
total_words = tidy_corpus %>%
  count(word) %>%
  arrange(desc(n))

ALL_WORDS =total_words %>%
  select(n)%>%
  sum()

# Only to graph CLUSTER
tidy_tfidf_average_cluster = tidy_tfidf %>% 
                      select(word,n,tf_idf,CLUSTER) %>%
                      group_by(word,CLUSTER) %>%
                        summarize( n_cluster = sum(n), tf_idf = mean(tf_idf))

tidy_tfidf_average_cluster$Nwords = NA
tidy_tfidf_average_cluster$Nwords = with(total_words, n[match(tidy_tfidf_average_cluster$word, word)])

# Graphic
tidy_tfidf_average_cluster %>%
  arrange(desc(tf_idf)) %>%
  group_by(CLUSTER) %>% 
  top_n(15,tf_idf) %>%
  ungroup %>%
  ggplot(aes(word, tf_idf, fill = CLUSTER)) +
  geom_col(show.legend = FALSE) + labs(x = NULL, y = "tf-idf") +
  facet_wrap(~CLUSTER, ncol = 2, scales = "free") +  
  coord_flip()

ggsave('subtitles_tf_idf.png', width = 6, height =  6)

# Graphhing Genre

freq_by_rank <- tidy_tfidf_average_cluster %>% 
  group_by(CLUSTER) %>% 
  mutate(rank = row_number(),`term frequency` = Nwords/ALL_WORDS)


ggplot(tidy_tfidf_average_cluster, aes(Nwords/ALL_WORDS, fill = CLUSTER)) +
  geom_histogram(show.legend = FALSE) +
  xlim(NA, 0.0009) +
  facet_wrap(~CLUSTER, ncol = 2, scales = "free_y")   #Terror (cluster5) and Sci-Fi  (cluster2) use more infrequent words.

ggsave('subtitles_rank_per_cluster.png', width = 6, height =  6)



  ##### NOT USEFUL STUFF -----
library(topicmodels)
library(tm)
# DTM
tidy_corpus_simple = tidy_corpus[-c(2:5)]
tidy_corpus_simple$tconst = as.character(levels(tidy_corpus_simple$tconst)[tidy_corpus_simple$tconst])
tidy_corpus.DTM <-  tidy_corpus_simple %>%
  count(tconst, word) %>%
  cast_dtm(tconst,word,n)

save(tidy_corpus.DTM,file = 'DTM_subtitles.Rda') 

#LDA
Script_topic_model<-LDA(tidy_corpus.DTM, k=7, control = list(seed = 123)) # 5  different topics, not supervised.
save(Script_topic_model,file = 'First_LDA_subtitles') 

Script_topics <- tidy(Script_topic_model, matrix = "beta")

Script_top_terms <- Script_topics %>%
  group_by(topic) %>%
  top_n(30, beta) %>%   #Can adjust for more precision
  ungroup() %>%
  arrange(topic, -beta)

LDA_topics = Script_top_terms %>%
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip()

ggsave(plot = LDA_topics,filename = 'LDA_subtitles_v6.png')

save(Script_topics,file = 'Subt_topics_tidy.Rda') 


#################

# Structural Topic Modeling ----
processed <- textProcessor(subset$text,metadata = subset)

docs <- out$documents
vocab <- out$vocab
meta <- out$meta

out <- prepDocuments(processed$documents,processed$vocab, processed$meta)

First_STM <- stm(documents = out$documents, vocab = out$vocab,
                 K = 20, prevalence =~ CLUSTER, content = ~CLUSTER,
                 max.em.its = 75, data = out$meta,
                 init.type = "Spectral", verbose = FALSE)
plot(First_STM)
plot(First_STM, type = "perspectives", topics = c(1,7))
