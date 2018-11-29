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


#install.packages("ggdendro")

dend_data <- dendro_data(dendrogram, type = "rectangle")
head(dend_data$segments)
dend_data$segments
tail(dend_data$segments)

#install.packages("dendextend")
library(dendextend)
library(ggdendro)
library(ggplot2)

# What contains dend_data
names(dend_data)
# Extract data for line segments
head(dend_data$segments)


#install.packages("dendextend")
library(dendextend)


# get the labels of the tree
dendrogram %>% labels

# get the number of leaves of the tree
dendrogram %>% nleaves

dendrogram%>% nnodes # get the number of nodes in the tree (including leaves)



#-----------------this part is for naming clusters --------------

hc = hclust(d = dist(df, method = 'euclidean'), method = 'ward.D')
out.id=cutree(dendrogram,k=3)
#mycl <- cutree(hc, h=max(hc$height/1.5))
mycl <- cutree(hc, 5)

# cutree returns a vector of cluster membership
# in the order of the original data rows
# examine it
mycl


# grab a cluster
cl_1 <- df[mycl == 1,]
cl_2 <- df[mycl == 2,]
cl_3 <- df[mycl == 3,]
cl_4 <- df[mycl == 4,]
cl_5 <- df[mycl == 5,]
head(cl_1)

# examine the cluster membership by it's order
# in the heatmap
mycl[hc$order]

# or simply add the cluster ID to your data
foo <- cbind(df, clusterID=mycl)



# examine the data with cluster ids attached, and ordered like the heat map
foo[hc$order,]
head(foo)




#--------------------------------
foo %>%
  summarise_each(funs(n_distinct))
table(foo$clusterID)


foo <-foo[order(foo$clusterID),]
head(foo)
tail(foo)

foo_long_table <- foo%>% select(-(c("clusterID")))
foo_long_table <- foo_long_table[,-c(1)]

foo_wide_table <- as.data.frame(t(foo_long_table))
colnames(foo_wide_table) <- foo$tconst
foo_wide_table$show_at <- c(1:40)


#1   2   3   4   5 
#261 307 449 254  39 

#Cluster1 (plot 20 lines on one plot)
mycl1 <- foo_wide_table[,c(1:261)]
mycl1$average <- rowMeans(mycl1)
mycl1$bin <- c(1:40)
myc1_m <- data.matrix(mycl1, rownames.force = NA)
myc1_m <- myc1_m[,c(1:20)]
myc1_m_m <- melt(myc1_m)
ggplot() +
  geom_line(data = myc1_m_m, aes(x = Var1, y = value, group = Var2, color = Var2))+
  ggtitle("My Cluster 1 sentiment plot") 
  

ggplot() +
  geom_line(data = mycl1, aes(x = bin, y = average, color = "red"))+
  ggtitle("My Cluster1 sentiment plot (average)") +
  ylim(-12, 4)


#Cluster2 (plot 20 lines on one plot)
mycl2 <- foo_wide_table[,c(262:568)]
mycl2$average <- rowMeans(mycl2)
mycl2$bin <- c(1:40)
myc2_m <- data.matrix(mycl2, rownames.force = NA)
myc2_m <- myc2_m[,c(1:20)]
myc2_m_m <- melt(myc2_m)
ggplot() +
  geom_line(data = myc2_m_m, aes(x = Var1, y = value, group = Var2, color = Var2))+
  ggtitle("My Cluster 2sentiment plot")

ggplot() +
  geom_line(data = mycl2, aes(x = bin, y = average, color = "red"))+
  ggtitle("My Cluster2 sentiment plot (average)") +
  ylim(-12, 4)



#Cluster3 (plot 20 lines on one plot)
mycl3 <- foo_wide_table[,c(569:1017)]
mycl3$average <- rowMeans(mycl3)
mycl3$bin <- c(1:40)
myc3_m <- data.matrix(mycl3, rownames.force = NA)
myc3_m <- myc3_m[,c(1:20)]
myc3_m_m <- melt(myc3_m)
ggplot() +
  geom_line(data = myc3_m_m, aes(x = Var1, y = value, group = Var2, color = Var2))+
  ggtitle("My Cluster 3 sentiment plot")

ggplot() +
  geom_line(data = mycl3, aes(x = bin, y = average, color = "red"))+
  ggtitle("My Cluster3 sentiment plot (average)") +
  ylim(-12, 4)


#Cluster4 (plot 20 lines on one plot)
mycl4 <- foo_wide_table[,c(1017:1271)]
mycl4$average <- rowMeans(mycl4)
mycl4$bin <- c(1:40)
myc4_m <- data.matrix(mycl4, rownames.force = NA)
myc4_m <- myc4_m[,c(1:20)]
myc4_m_m <- melt(myc4_m)
ggplot() +
  geom_line(data = myc4_m_m, aes(x = Var1, y = value, group = Var2, color = Var2))+
  ggtitle("My Cluster 4 sentiment plot")

ggplot() +
  geom_line(data = mycl4, aes(x = bin, y = average, color = "red"))+
  ggtitle("My Cluster4 sentiment plot (average)") +
  ylim(-12, 4)


#Cluster5 (plot 20 lines on one plot)
mycl5 <- foo_wide_table[,c(1272:1310)]
mycl5$average <- rowMeans(mycl5)
mycl5$bin <- c(1:40)
myc5_m <- data.matrix(mycl5, rownames.force = NA)
myc5_m <- myc5_m[,c(1:20)]
myc5_m_m <- melt(myc5_m)
ggplot() +
  geom_line(data = myc5_m_m, aes(x = Var1, y = value, group = Var2, color = Var2))+
  ggtitle("My Cluster 5 sentiment plot")

ggplot() +
  geom_line(data = mycl5, aes(x = bin, y = average, color = "red"))+
  ggtitle("My Cluster5 sentiment plot (average)") +
  ylim(-12, 4)

#----------------------------------

dend <- as.dendrogram(hclust(dist(df)))
# Like: 


# midpoint for all nodes
get_nodes_attr(dend, "midpoint")


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

