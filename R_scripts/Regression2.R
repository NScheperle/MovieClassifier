library(RMySQL)
library(tidytext)
library(dplyr)
library(tidyr)
library(raster)
library(rgeos)
library(dtw)
library(MASS)

mydb = dbConnect(MySQL(), user='mxd', dbname='IMDB', host='vcm-5368.vm.duke.edu')
dbListTables(mydb)


cluster_genre<-dbReadTable(mydb, "cluster_genre")
sentiment<-dbReadTable(mydb, "sentiment")
budget_rating<-dbReadTable(mydb, "budget_rating")

movie_genre<-merge(sentiment, cluster_genre, by = "tconst")
movie_genre<-movie_genre[,-42:-44]
movie_genre<-na.omit(movie_genre)
table(movie_genre$CLUSTER)
movie_genre<-movie_genre[!movie_genre$CLUSTER %in% c(2,5,NA),]
table(movie_genre$CLUSTER)

cluster1 <- movie_genre[movie_genre$CLUSTER == 1,]
table(cluster1$CLUSTER)
cluster1 <- cluster1[,-42]
cluster2 <- movie_genre[movie_genre$CLUSTER == 3,]
table(cluster2$CLUSTER)
cluster2 <- cluster2[,-42]
cluster3 <- movie_genre[movie_genre$CLUSTER == 4,]
table(cluster3$CLUSTER)
cluster3 <- cluster3[,-42]

c1<-colMeans(cluster1[,2:41], na.rm = TRUE, dims = 1)
c2<-colMeans(cluster2[,2:41], na.rm = TRUE, dims = 1)
c3<-colMeans(cluster3[,2:41], na.rm = TRUE, dims = 1)

c1<-as.data.frame(t(c1))
c2<-as.data.frame(t(c2))
c3<-as.data.frame(t(c3))

dist1<-as.data.frame(dtwDist(cluster1[,2:41],my=c1,method="DTW"))
dist2<-as.data.frame(dtwDist(cluster2[,2:41],my=c2,method="DTW"))
dist3<-as.data.frame(dtwDist(cluster3[,2:41],my=c3,method="DTW"))

temp1<-cbind(cluster1$tconst,dist1)
names(temp1)<-c('tconst','dist')
temp2<-cbind(cluster2$tconst,dist2)
names(temp2)<-c('tconst','dist')
temp3<-cbind(cluster3$tconst,dist3)
names(temp3)<-c('tconst','dist')

regmatrx<-rbind(temp1,temp2,temp3)
regmatrx<-merge(sentiment, cluster_genre, by = "tconst")
budget_rating<-dbReadTable(mydb, "budget_rating")
ratings<-dbReadTable(mydb, "ratings")

regmatrx<-merge(regmatrx, budget_rating, by = "tconst")
regmatrx<-regmatrx[,c(-4,-6:-7)]
regmatrx<-na.omit(regmatrx)

regbox = lm(gross_usa~dist+budget, data = regmatrx)
summary(regbox)
par(mfrow=c(2,2))
plot(regbox)

par(mfrow=c(1,1))
bc.lm = boxcox(regbox, lambda = seq(-1,2,0.1))
bc.lm$x[bc.lm$y==max(bc.lm$y)]

regbox.t = lm((gross_usa^(.3)-1)/.3~dist+budget, data = regmatrx)
par(mfrow=c(2,2))
plot(regbox.t)
summary(regbox.t)

regbox.t2 = lm((gross_usa^(.3)-1)/.3~budget, data = regmatrx)
anova(regbox.t2, regbox.t)

regmatrx <- merge(regmatrx, ratings, by = "tconst")
regmatrx<-regmatrx[,-6]
regmatrx<-na.omit(regmatrx)

regrat = lm(averageRating~dist+budget, data = regmatrx)
summary(regrat)
par(mfrow=c(2,2))
plot(regrat)

par(mfrow=c(1,1))
bc.lm = boxcox(regrat, lambda = seq(1,4,0.1))
bc.lm$x[bc.lm$y==max(bc.lm$y)]

regrat.t = lm((averageRating^(2.4)-1)/2.4~dist+budget, data = regmatrx)
par(mfrow=c(2,2))
plot(regrat.t)
summary(regrat.t)

regrat.t2 = lm((averageRating^(2.4)-1)/2.4~budget, data = regmatrx)
anova(regrat.t2, regrat.t)

