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

#k-means clustering is a method of cluster analysis which aims to partition n observations into k clusters 
#in which each observation belongs to the cluster with the nearest mean.

##K-means clusters 
set.seed(1010103)
df <- sentiment[, c(2:41)]

#use principal component analysis to reduce dimension
pca_ratings<-princomp(data.matrix(df),cor=TRUE)
summary(pca_ratings)
ratings_pca<-predict(pca_ratings,data.matrix(df))
ratings_pca<-ratings_pca[,1:27]

##Selecting the number of clusters (elbow plot): identify when the set of clusters explins "most" of the variance in the data.
pct_var <- data.frame(pct_var = 0,
                      num_clusters=2:14)
totalss <- kmeans(ratings_pca, centers=14, nstart=50, iter.max = 100)$totss
for(i in 2:14){
  pct_var[i-1, 'pct_var'] <- kmeans(ratings_pca, centers=i, nstart=50, iter.max = 100)$betweenss/totalss
}

ggplot(pct_var, aes(x=num_clusters, y=pct_var)) +
  geom_line() +
  geom_point() +
  labs(y='% Variance Explained', x='Number of Clusters') +
  scale_x_continuous(breaks=seq(2, 14, by=2))   +
  theme_bw()

##k means
require(MASS)
require(dplyr)
set.seed(1010103)
km <- kmeans(ratings_pca, centers=4, nstart=50)
km$size
centers <- data.frame(km$centers)

#plot center of clusters
centers <- as.data.frame(t(centers))
names(centers) <- paste("Cluster", 1:4)
centers$Symbol <- row.names(centers)
centers <- gather(centers, "Cluster", "Mean", -Symbol)

centers$Color = centers$Mean > 0
ggplot(centers, aes(x=Symbol, y=Mean, fill=Color)) +
  geom_bar(stat='identity', position = "identity", width=.75) + 
  scale_x_discrete(limits=c(paste("Comp.", 1:27, sep = "")))+
  facet_grid(Cluster ~ .) +
  guides(fill=FALSE)  +
  ylab('Component Loading') +
  theme_bw() +
  theme(axis.title.x = element_blank(),
        axis.text.x  = element_text(angle=90, vjust=0.5))
#, scales='free_y'

##Explore whether the clusters are mostly correct
df$cluster <- factor(km$cluster)
df$tconst <- row.names(df)

result <- df %>% 
  left_join(clgenres, by ="tconst") %>% 
  dplyr::select(cluster:tconst, CLUSTER) %>% 
  group_by(cluster) %>% 
  count(CLUSTER)
result$CLUSTER <- ifelse(is.na(result$CLUSTER), 'No category', result$CLUSTER)
  
ggplot(result, aes(x=CLUSTER, y=n, fill = "Coral")) +
  geom_bar(stat='identity', position = "identity", width=.75) + 
  facet_grid(cluster ~ ., scales='free_y') +
  guides(fill=FALSE)  +
  theme_bw() +
  theme(axis.title.x = element_blank(),
        axis.text.x  = element_text(angle=90, vjust=0.5))

sum_value <- result %>% 
  group_by(cluster) %>% 
  summarise(sum_value = sum(n))

intermediate_table <- merge(result, sum_value)

percetage_table <- intermediate_table %>% 
  mutate(percent = n/sum_value)

percentage_table

# ggplot(percetage_table, aes(x=CLUSTER, y=percent, color = "Coral",group = 1)) +
#   geom_line() +
#   geom_point(size = 4)+ 
#   facet_grid(cluster ~ .) +
#   guides(fill=FALSE)  +
#   theme_bw() +
#   theme(axis.title.x = element_blank(),
#         axis.text.x  = element_text(angle=90, vjust=0.5))

ggplot(percetage_table, aes(x=cluster, y=percent, fill = CLUSTER))+
  geom_bar(stat="identity", position = position_stack(reverse = TRUE)) +
  scale_fill_brewer(name="Original Labels",
                    breaks=c("1", "2", "3","4","5","No category"),
                    labels=c("Drama, Comedy, Romance", "Western, War", "Scifi Adventure", "Crime, Mystery","Horror","No Category"), palette = "Spectral")+
  ylab("Percentages of Movies in the cluster")+
  xlab("4 clusters given by kmeans")+
  coord_flip() +
  theme(legend.position = "top")

#################################################################################

# Disconnect from the database
dbDisconnect(mydb)

