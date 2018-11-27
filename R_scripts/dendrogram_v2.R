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
df <- as.numeric(df)

#distances(df, id_variable = NULL, dist_variables = NULL)
dendrogram = hclust(d = dist(df, method = 'euclidean'), method = 'ward.D')
plot(dendrogram,
     main = paste('Dendrogram'),
     xlab = 'movies',
     ylab = 'Euclidean distances')

# Fitting Hierarchical Clustering to the dataset
hc = hclust(d = dist(df, method = 'euclidean'), method = 'ward.D')
hc = hclust(d = dist(df, method = 'euclidean'), method = 'ward.D')


install.packages("ggdendro")

dend_data <- dendro_data(dendrogram, type = "rectangle")
head(dend_data$segments)
dend_data$segments
tail(dend_data$segments)

install.packages("dendextend")
library(dendextend)
library(ggdendro)
library(ggplot2)

# What contains dend_data
names(dend_data)
# Extract data for line segments
head(dend_data$segments)



---------------------------still working-----------------
### work out the hight you want to cut the groups
check = as.dendrogram(hc)
heights_per_k.dendrogram(check)
h = 6 # this should be 4 leafs, we check below
dend_data$segments = subset(dend_data$segments, y > h )
# check how many leafs you have
ggplot(dend_data$segments) + geom_segment(aes(x = x, y = y, xend = xend, yend = yend))
leaf_numb = 4 # from the number of leafs you now have
# Extract data for labels
head(dend_data$labels)
tail(dend_data$labels)
lab = data.frame(dend_data$segments[ order(dend_data$segments[,4]), ][1:leaf_numb,])
lab$label = factor(c(seq(1,leaf_numb)))
lab$y = h-1 # the end number will vary, I suggest plotting without this value and adjust as required
# make the leafs end at the same place
dend_data$segments = dend_data$segments[ order(dend_data$segments[,4]), ]
dend_data$segments[1:leaf_numb,4] = h
# Plot line segments and add labels
p <- ggplot(dend_data$segments) +
  geom_segment(aes(x = x, y = y, xend = xend, yend = yend))+
  geom_text(data = lab, aes(x = x, y = y, label = label), size = 3)
print(p)

-----------------------still working-----------


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