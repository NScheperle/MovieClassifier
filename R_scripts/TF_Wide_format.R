library(ggplot2)
library(data.table)
library(cleanNLP)
library(dplyr)

load("C:/Users/joaqu/OneDrive/Escritorio/MovieClassifier/R_scripts/RDA_objects/tf123.RDA")

total_words_movies = data.frame('Cluster' = c('DCRFSM','AWW','SAFA','CMT','H'), 'N_words' = c(2134010,14260,370344,421559,23244))
total_words_movies$Cluster = factor(total_words_movies$Cluster, levels= c('DCRFSM','AWW','SAFA','CMT','H'))
ggplot(total_words_movies) + geom_col(aes(x=Cluster, y=N_words, color = Cluster, fill= Cluster)) + theme_classic()


tidy_tfidf_wide = tidy_tfidf[,c(1:2,6)]
tidy_tfidf_wide$tconst = as.character(levels(tidy_tfidf_wide$tconst)[tidy_tfidf_wide$tconst])
cluster = tidy_tfidf[,c(2,7)]

#Other reshape form
setDT(tidy_tfidf_wide)
dt_tfidf_wide =dcast(tidy_tfidf_wide, tconst ~ word, value.var = "tf_idf")
dt_tfidf_wide = as.data.frame(dt_tfidf_wide) #transform to data.frame


#I need to add the CLUSTER to the tfidf
clusters = cluster %>% 
            distinct(tconst,CLUSTER)

tcon = dt_tfidf_wide$tconst
tcon = data.frame('tconst' = tcon)
CL = merge(tcon,clusters)

#Adding CLUSTER
dt_tfidf_wide$CLUSTER = CL$CLUSTER


dt_tfidf_wide[is.na(dt_tfidf_wide)] = 0  #We need to replace NA with 0

row.names(dt_tfidf_wide) <- dt_tfidf_wide$tconst  #need to remove the character tconst from my matrix
dt_tfidf_wide = dt_tfidf_wide[,-1]
save(dt_tfidf_wide, file = '134wide.rda')
write.csv(dt_tfidf_wide, file = 'tf134wide.csv',fileEncoding = 'utf-8')


pca = cleanNLP::cnlp_utils_pca(x= dt_tfidf_wide , k = 50 )

save(pca, file = 'pca-tf-idf.rda' )

#Removing genres that are not 3 or 4





