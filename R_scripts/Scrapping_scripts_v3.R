#### HERE Start the code


library(SnowballC)
library(topicmodels)
library(tidytext)
library(dplyr)
library(ggplot2)
library(readtext)


######################################################################################33
# General Script for  read all my files.

install.packages('readtext') #Useful to read .txt as an object.
files <- list.files(path="C:/Users/joaqu/OneDrive/Escritorio/MovieClassifier/Subtitles_files", pattern="*.srt", full.names=TRUE, recursive=FALSE)

test_file = readtext::readtext(files[1])
raw_scripts = as.data.frame(test_file)
for (i in files[-1]){ script_file = readtext::readtext(i)
  raw_scripts = rbind(raw_scripts,script_file)
}

save(raw_scripts,file = 'Raw_corpus.Rda') #The file is raw. It does not have any transformation or steam 

load('C:/Users/joaqu/OneDrive/Escritorio/Raw IMDB/Raw_corpus.Rda')

# In this case we would apply the different modification to the corpus that we would use in the final scenario.
# Remember that a good idea could be to have  a third column called Movie_ID to link this results with data from IMBD_movie

corpus <- raw_scripts %>%
  select(doc_id,text) %>%
  unnest_tokens("word", text) 

corpus %>%
  count(word) %>%
  arrange(desc(n))

data("stop_words")
tidy_corpus<-corpus %>%
  anti_join(stop_words)

tidy_corpus %>%
  count(word) %>%
  arrange(desc(n))

tidy_corpus<-tidy_corpus[-grep("\\b\\d+\\b", tidy_corpus$word),]

tidy_corpus$word <- gsub("\\s+","",tidy_corpus$word)

tidy_corpus<-tidy_corpus %>%
  mutate_at("word", funs(wordStem((.), language="en")))

tidy_corpus.DTM<-  tidy_corpus %>%
  count(doc_id, word) %>%
  cast_dtm(doc_id, word, n)

save(tidy_corpus.DTM,file = 'tidy_corpus_DTM.Rda') 

Script_topic_model<-LDA(tidy_corpus.DTM, k=2, control = list(seed = 321)) # 5  different topics, not supervised.
save(Script_topic_model,file = 'Script_topics_model.Rda') 

Script_topics <- tidy(Script_topic_model, matrix = "beta")

Script_top_terms <- Script_topics %>%
  group_by(topic) %>%
  top_n(25, beta) %>%   #Can adjust for more precision
  ungroup() %>%
  arrange(topic, -beta)


Script_top_terms %>%
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip()

save(Script_topics,file = 'Script_topics_tidy.Rda') 
