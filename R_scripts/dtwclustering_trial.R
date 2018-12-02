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
sentiment$tconst <- NULL

library(dtw)
library(dtwclust)

##Turn sentiment into S4 object and normalize data using zscore
sentiment_timeseries <- tslist(sentiment, simplify = FALSE)
sentiment_timeseries <- zscore(sentiment_timeseries)

##Hierachical
hc_sbd <- tsclust(sentiment_timeseries, type = "h", k = 10L, seed = 899,
                  distance = "sbd", centroid = shape_extraction,
                  control = hierarchical_control(method = "average"))
hc_sbd@clusinfo
# By default, the dendrogram is plotted in hierarchical clustering
plot(hc_sbd)
plot(hc_sbd, type = "sc")

plot(hc_sbd, type = "series", clus = 1L)
plot(hc_sbd, type = "centroids", clus = 1L)
###########################################################################
##k-shape
pc_ks <- tsclust(sentiment_timeseries, k = 5L,
                 distance = "sbd", centroid = "shape",
                 seed = 8, trace = TRUE)
plot(pc_ks)
clusin <- pc_ks@clusinfo
pc_ks@

##Tadpole
pc_tp <- tsclust(sentiment_timeseries, k = 5L, type = "t",
                 seed = 8, trace = TRUE,
                 control = tadpole_control(dc = 1.5,
                                           window.size = 20L))
plot(pc_tp)
pc_tp@clusinfo

##DTW_basic
pc_dtw <- tsclust(sentiment_timeseries, k = 5L,
                  distance = "dtw_basic", centroid = "pam",
                  trace = TRUE, seed = 1995,
                  norm = "L2", window.size = 20L,
                  args = tsclust_args(cent = list(trace = TRUE)))
plot(pc_dtw)
pc_dtw@clusinfo

##Compare variance of information across different hard clustering methods
result <- sentiment %>% 
  left_join(clgenres, by ="tconst") %>% 
  dplyr::select(CLUSTER)
result$CLUSTER <- ifelse(is.na(result$CLUSTER), 'No category', result$CLUSTER)

# sentimentlabel <- as.factor(clgenres$CLUSTER)
# addNA(sentimentlabel)
# 
# sapply(list(DTW = pc_dtw, kShape = pc_ks, TADPole = pc_tp), cvi, b = sentimentlabel, type = "VI")



### Let's try using k-shape info
sentiment$cluster <- factor(pc_ks@cluster)
sentiment$tconst <- row.names(sentiment)

result <- sentiment %>% 
  left_join(clgenres, by ="tconst") %>% 
  dplyr::select(cluster:tconst, CLUSTER) %>% 
  group_by(cluster) %>% 
  count(CLUSTER)
result$CLUSTER <- ifelse(is.na(result$CLUSTER), 'No category', result$CLUSTER)

sum_value <- result %>% 
  group_by(cluster) %>% 
  summarise(sum_value = sum(n))

intermediate_table <- merge(result, sum_value)

percetage_table <- intermediate_table %>% 
  mutate(percent = n/sum_value)

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


