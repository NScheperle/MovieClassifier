library(RMySQL)
library(tidyverse)
library(tidytext)
library(reshape2)

library(caTools)
mydb = dbConnect(MySQL(), user = 'jh608', dbname ='IMDB', host = 'vcm-5368.vm.duke.edu')
dbListTables(mydb)

sentiment = dbReadTable(mydb, name = 'sentiment')
sentiment

#install.packages('distances')
#library(distances)
sentiment[c(2:801), c(2:41)] <- sapply(sentiment[c(2:801), c(2:41)], as.numeric)
sentiment<- sentiment[-c(1), ]
sentiment<-suppressWarnings(sentiment)
df<-as.data.frame(sentiment)
df <- df %>% drop_na()

#distances(df, id_variable = NULL, dist_variables = NULL)
dendrogram = hclust(d = dist(df, method = 'euclidean'), method = 'ward.D')
plot(dendrogram,
     main = paste('Dendrogram'),
     xlab = 'movies',
     ylab = 'Euclidean distances')

# Fitting Hierarchical Clustering to the dataset
hc = hclust(d = dist(df, method = 'euclidean'), method = 'ward.D')
y_hc = cutree(hc, 3)
#is.na(df)
# Visualising the clusters
library(cluster)
clusplot(df,
         y_hc,
         lines = 0,
         shade = TRUE,
         color = TRUE,
         labels= 2,
         plotchar = FALSE,
         span = TRUE,
         main = paste('Clusters of movies'),
         xlab = 'movies',
         ylab = 'sentiment vectors')