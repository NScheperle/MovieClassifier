# I need to to a querry in SQL to take all the subtitles we have until the moment:
# mysql IMDB --execute="select * from subtitles " > $HOME/test_sql.txt

# I think it ill be more smart use a python script. I will Querry every tcosnt I have and save in different files with the tcosnt.
# In this way I could save every file in an R object and wont kill my machine. Better 925 heavy rows than 6000000+ rows to iterate

# I need a query to get all my genre right now. 
# #mysql IMDB --execute="select distinct(subtitles.tconst), titles.genre from subtitles 
# left join titles on subtitles.tconst = titles.tconst" > '/home/jm622/tconst_genre.csv'

library(dplyr)
library(stm)

setwd('C:/Users/joaqu/OneDrive/Escritorio/Text_Analysis-master/Text_Analysis/')
#corpus = read.delim(file = 'test_sql.txt' , header = T, stringsAsFactors = F, sep = '\t') Older aproach

# NEw APPROACH!
files <- list.files(path="C:/Users/joaqu/OneDrive/Escritorio/Text_Analysis-master/Text_Analysis/python/subtitles/", pattern="*.txt", full.names=TRUE, recursive=FALSE)
test_file = readtext::readtext(files[1])
raw_scripts = as.data.frame(test_file)
for (i in files[-1]){ script_file = readtext::readtext(i)
raw_scripts = rbind(raw_scripts,script_file)
}
corpus = raw_scripts
colnames(corpus) = c('tconst','text')
corpus$tconst <- gsub(".txt",'', corpus$tconst)  #Removing the .txt to have the tconst.
corpus$tconst = as.factor(corpus$tconst) #convert to factor

movies = read.delim(file = 'tconst_genres.txt', header = F, sep = '\t', col.names = c('tconst', 'Genre_1', 'Genre_2', 'Genre_3'))


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

actual_movies$cluster_gen1 = NA
actual_movies$cluster_gen2 = NA
actual_movies$cluster_gen3 = NA

in.genre <- function(dataset,genre.col,cluster,cluster.col.name, number_cluster) {for (i in seq(0,length(dataset[[genre.col]]))){
  if (length(dataset[i,][[genre.col]]) > 0){
    if (as.character(dataset[i,][[genre.col]])%in% cluster){
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
actual_movies$CLUSTER1.2= NA
for (i in seq(1,length(actual_movies$CLUSTER1.2 ))) {
  if (!is.na(actual_movies[i,]$cluster_gen1)) {
    if(!is.na(actual_movies[i,]$cluster_gen2)){
      if(actual_movies[i,]$cluster_gen1 == actual_movies[i,]$cluster_gen2){
        
        actual_movies[i,]$CLUSTER1.2 = actual_movies[i,]$cluster_gen1}
      
      else {actual_movies[i,]$CLUSTER1.2 = NA}}
    
    else{actual_movies[i,]$CLUSTER1.2 = actual_movies[i,]$cluster_gen1}}
  
  else{actual_movies[i,]$CLUSTER1.2 = actual_movies[i,]$cluster_gen1}
}

actual_movies$CLUSTER= NA
for (i in seq(1,length(actual_movies$CLUSTER1.2 ))) {
  if (!is.na(actual_movies[i,]$CLUSTER1.2)) {
    if(!is.na(actual_movies[i,]$cluster_gen3)){
      if(actual_movies[i,]$CLUSTER1.2 == actual_movies[i,]$cluster_gen3){
        
        actual_movies[i,]$CLUSTER = actual_movies[i,]$CLUSTER1.2}
      
      else {actual_movies[i,]$CLUSTER = NA}}
    
    else{actual_movies[i,]$CLUSTER = actual_movies[i,]$cluster_gen1}}
  
  else{actual_movies[i,]$CLUSTER = actual_movies[i,]$CLUSTER1.2}
} 

# Save data unitl now
saveRDS(actual_movies, file = 'actual_movies.rds')

# Select only one subset of my movies ----

subset = actual_movies[!is.na(actual_movies$CLUSTER),]
subset$CLUSTER = as.factor(subset$CLUSTER)
summary(subset)  



# Structural Topic Modeling ----
processed <- textProcessor(subset$text,metadata = subset)

#docs <- out$documents
#vocab <- out$vocab
#meta <- out$meta

out <- prepDocuments(processed$documents,processed$vocab, processed$meta)

First_STM <- stm(documents = out$documents, vocab = out$vocab,
                 K = 20, prevalence =~ CLUSTER, content = ~CLUSTER,
                 max.em.its = 75, data = out$meta,
                 init.type = "Spectral", verbose = TRUE)
plot(First_STM)
plot(First_STM, type = "perspectives", topics = c(1,7))

saveRDS(First_STM, file = 'First_STM_subtitles.RDS')

# A test to see the possible K
findingk <- searchK(out$documents, out$vocab, K = c(5:15),
                    prevalence =~ CLUSTER, data = out$meta, verbose=T)

saveRDS(findingk, file = 'findingk_subtitles.rds')
plot(findingk)
