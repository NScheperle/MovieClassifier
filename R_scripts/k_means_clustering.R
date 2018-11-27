library(RMySQL)
library(tidyverse)
library(tidytext)

mydb = dbConnect(MySQL(), user = 'bl199', dbname ='IMDB', host = 'vcm-5368.vm.duke.edu')
dbListTables(mydb)
sentiment = dbReadTable(mydb, name = 'sentiment')
clgenres = dbReadTable(mydb, name = 'cluster_genre')
sentiment[, c(2:41)] <- sapply(sentiment[, c(2:41)], as.numeric)
sentiment<- sentiment[-c(1), ]
row.names(sentiment) <- sentiment$tconst

##K-means clusters 
set.seed(1010103)
df <- sentiment[, c(2:41)]
km <- kmeans(df, centers=10, nstart=10)

km$size

centers <- data.frame(km$centers)
centers

#plot center of clusters
centers <- as.data.frame(t(centers))
names(centers) <- paste("Cluster", 1:10)
centers$Symbol <- row.names(centers)
centers <- gather(centers, "Cluster", "Mean", -Symbol)

centers$Color = centers$Mean > 0
ggplot(centers, aes(x=Symbol, y=Mean, fill=Color)) +
  geom_bar(stat='identity', position = "identity", width=.75) + 
  scale_x_discrete(limits=c(paste("vec", 1:40, sep = "")))+
  facet_grid(Cluster ~ ., scales='free_y') +
  guides(fill=FALSE)  +
  ylab('Component Loading') +
  theme_bw() +
  theme(axis.title.x = element_blank(),
        axis.text.x  = element_text(angle=90, vjust=0.5))



## Selecting the number of clusters (elbow plot)
pct_var <- data.frame(pct_var = 0,
                      num_clusters=2:14)
totalss <- kmeans(df, centers=14, nstart=50, iter.max = 100)$totss
for(i in 2:14){
  pct_var[i-1, 'pct_var'] <- kmeans(df, centers=i, nstart=50, iter.max = 100)$betweenss/totalss
}

ggplot(pct_var, aes(x=num_clusters, y=pct_var)) +
  geom_line() +
  geom_point() +
  labs(y='% Variance Explained', x='Number of Clusters') +
  scale_x_continuous(breaks=seq(2, 14, by=2))   +
  theme_bw()

##explore whether the clusters are mostly correct
df$cluster <- factor(km$cluster)
head(df)
df$tconst <- row.names(df)

require(MASS)
require(dplyr)
df_arranged <- df %>% 
  left_join(clgenres, by ="tconst") %>% 
  dplyr::select(cluster:tconst, CLUSTER) %>% 
  arrange(cluster)

result = df_arranged %>%
  group_by(cluster) %>% 
  count(CLUSTER) 
result$CLUSTER <- ifelse(is.na(result$CLUSTER), 'No category', result$CLUSTER)
  
ggplot(result, aes(x=CLUSTER, y=n, fill = "Coral")) +
  geom_bar(stat='identity', position = "identity", width=.75) + 
  facet_grid(cluster ~ ., scales='free_y') +
  ylab('K Clusters') +
  guides(fill=FALSE)  +
  theme_bw() +
  theme(axis.title.x = element_blank(),
        axis.text.x  = element_text(angle=90, vjust=0.5))




