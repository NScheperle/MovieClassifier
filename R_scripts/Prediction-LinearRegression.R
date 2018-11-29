library(MASS)



budget_rating<-dbReadTable(mydb, "budget_rating")
ratings<-dbReadTable(mydb, "ratings")
cluster_genre<-dbReadTable(mydb, "cluster_genre")subs_sentvec<-subs_sentvec[-41,]

movie_ratings<-merge(sentiment, ratings, by = "tconst")
movie_box<-merge(sentiment, budget_rating, by = "tconst")
summary(movie_ratings$numVotes)
movie_ratings<-movie_ratings[movie_ratings$numVotes>=20000,]
movie_ratings<-movie_ratings[,-43]
summary(movie_box$gross_usa)
movie_box<-movie_box[movie_box$gross_usa>=5000000,]
movie_box<-movie_box[,c(1:41,44)]
movie_box<-na.omit(movie_box)

for(i in 2:41){
  movie_ratings[,i] <- as.numeric(movie_ratings[,i])
}

for(i in 2:41){
  movie_box[,i] <- as.numeric(movie_box[,i])
}

movie_ratings_c<-movie_ratings[,2:41]
for (i in 1:40) {
  movie_ratings_c[,i]=scale(movie_ratings_c[,i], center = TRUE, scale = TRUE)
}

movie_box_c<-movie_box[,2:41]
for (i in 1:40) {
  movie_box_c[,i]=scale(movie_box_c[,i], center = TRUE, scale = TRUE)
}

pca_ratings<-princomp(data.matrix(movie_ratings_c),cor=TRUE)
summary(pca_ratings)
ratings_pca<-predict(pca_ratings,data.matrix(movie_ratings_c))
ratings_pca<-ratings_pca[,1:27]

ratings<-data.frame(cbind(ratings_pca,movie_ratings$averageRating))
names(ratings)[28]<-'rating'
regratings = lm(rating~., data = ratings)
summary(regratings)
par(mfrow=c(2,2))
plot(regratings)

par(mfrow=c(1,1))
bc.lm = boxcox(regratings, lambda = seq(0,4,0.1))
bc.lm$x[bc.lm$y==max(bc.lm$y)]

regratings.t = lm((rating^(2.3)-1)/2.3~., data = ratings)
par(mfrow=c(2,2))
plot(regratings.t)
summary(regratings.t)


