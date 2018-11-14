## Structural Topic Modeling
# install.packages('stm')
setwd('C:/Users/joaqu/OneDrive/Escritorio/Text_Analysis-master/Text_Analysis')
library(stm)
library(readtext)
library(dplyr)
#Uploading the data available for all movies----
corpus = read.csv(file = 'names_tt_genre2.csv' , header = T, stringsAsFactors = F, na.strings = '')
colnames(corpus)[1] <- 'doc_id' 
corpus$doc_id = paste0(corpus$doc_id,'.txt')

#Reading all the scripts we scraped----
files <- list.files(path="C:/Users/joaqu/OneDrive/Escritorio/Text_Analysis-master/Scripts", pattern="*.txt", full.names=TRUE, recursive=FALSE)
test_file = readtext::readtext(files[1])
raw_scripts = as.data.frame(test_file)
for (i in files[-1]){ script_file = readtext::readtext(i)
raw_scripts = rbind(raw_scripts,script_file)
}
#Merge scripts with Movie data----
actual_movies = merge(x = corpus, y= raw_scripts)
#Check if I let movies out
corpus$doc_id  %in% actual_movies$doc_id

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

# Select only one subset of my movies ----

subset = actual_movies[!is.na(actual_movies$CLUSTER),]
subset$CLUSTER = as.factor(subset$CLUSTER)
summary(subset)  


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

### This is a function to check for different models (Dont know very well if I can use this Models)
scripts_models <- selectModel(out$documents, out$vocab, K = 20,
                              prevalence =~ CLUSTER, max.em.its = 75,
                              data = out$meta, runs = 20, seed = 8458159)
plotModels(scripts_models)
topicQuality(model = scripts_models$runout[[3]],documents = out$documents)
####

findingk <- searchK(out$documents, out$vocab, K = c(5:15),
                    prevalence =~ CLUSTER, data = out$meta, verbose=T)

saveRDS(findingk, file = 'findingk.rds')
plot(findingk)
