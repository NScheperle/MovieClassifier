#### HERE Start the code
# The first part is only using two scripts



library(SnowballC)
library(topicmodels)
library(tidytext)
library(dplyr)
library(ggplot2)
library(readtext)



files <- list.files(path="C:/Users/Scarlett/Dowloads/text", pattern="*.srt", full.names=TRUE, recursive=FALSE)


#test_file = readtext::readtext(files[1])
#raw_scripts = as.data.frame(test_file)
#for (i in files[-1]){ script_file = readtext::readtext(i)
#raw_scripts = rbind(raw_scripts,script_file)
#}


#test_file = readtext::readtext('C:/Users/Scarlett/Downloads/text/test_output.txt')
#test_file.2 = readtext::readtext('C:/Users/Scarlett/Downloads/text/titanic.srt')
#test_file.2 = readtext::readtext(files[-1])


test_file = readtext::readtext('C:/Users/Scarlett/Downloads/text/AboutTime.srt')
test_file.2 = readtext::readtext('C:/Users/Scarlett/Downloads/text/Titanic.srt')
test_file.3 = readtext::readtext('C:/Users/Scarlett/Downloads/text/MoulinRouge.srt')
test_file.4 = readtext::readtext('C:/Users/Scarlett/Downloads/text/LoveActually.srt')
test_file.5 = readtext::readtext('C:/Users/Scarlett/Downloads/text/LaLaLand.srt')

test.files.1 = as.data.frame(test_file)
test.files.2 = as.data.frame(test_file.2)
test.files.3 = as.data.frame(test_file.3)
test.files.4 = as.data.frame(test_file.4)
test.files.5 = as.data.frame(test_file.5)

test.files_1= rbind(test.files.1,test_file.2)
test.files_2= rbind(test.files_1,test.files.3)
test.files_3= rbind(test.files_2,test.files.4)
test.files= rbind(test.files_3, test.files.5)

head(test.files)


#test.files = as.data.frame(test_file)
#test.files = rbind(test.files,test_file.2)

corpus <- test.files %>%
  select(doc_id,text) %>%
  unnest_tokens("word", text) 
  
corpus %>%
  count(word) %>%
  arrange(desc(n))

# Here we would like to add some personalize dictionaries to remove names of character.
# Maybe we would not do this with the entire corpus, but for now it would be useful
#word = c('jack', 'rose', "l'm", 'christian', 'zidler' )
#characters = as.data.frame(word )
#lexicon = rep('Eazye',5)
#characters= cbind(characters,lexicon)



word = c('jack', 'rose', "l'm", 'christian', 'zidler', 'ethan', 'bond', 'wayn', 'harvey', 'gotham', 'rachel', 'joker', 'batman', 'benji', 'gonna', 'wayn', 'bruce' )
characters_Action = as.data.frame(word)
lexicon = rep('Eazye',17)
characters_Action= cbind(word,lexicon)

data("stop_words")
new_stop_words_Action=rbind(stop_words, characters_Action)
head(new_stop_words_Action)


tidy_corpus<-corpus %>%
  anti_join(new_stop_words_Action)

data("stop_words")
#new_stop_words=rbind(stop_words, characters)

#data("new_stop_words")
#tidy_corpus<-corpus %>%
#  anti_join(stop_words)

#tidy_corpus<-tidy_corpus %>%
#  anti_join(characters)

#tidy_corpus<-tidy_corpus %>%
#  anti_join(characters)

tidy_corpus %>%
  count(word) %>%
  arrange(desc(n))

# Look if this is important to take in count. 
# CLEANING numbers and white spaces
tidy_corpus<-tidy_corpus[-grep("\\b\\d+\\b", tidy_corpus$word),]

tidy_corpus$word <- gsub("\\s+","",tidy_corpus$word)

tidy_corpus<-tidy_corpus %>%
  mutate_at("word", funs(wordStem((.), language="en")))

tidy_corpus.DTM<- 
  tidy_corpus %>%
  count(doc_id, word) %>%
  cast_dtm(doc_id, word, n)

library(topicmodels)
Script_topic_model<-LDA(tidy_corpus.DTM, k=2, control = list(seed = 321))

Script_topics <- tidy(Script_topic_model, matrix = "beta")

Script_top_terms <- Script_topics %>%
  group_by(topic) %>%
  top_n(15, beta) %>%   #Can adjust for more precision
  ungroup() %>%
  arrange(topic, -beta)


Script_top_terms %>%
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip()

###################################################################

# read Action movies
Action_file = readtext::readtext('C:/Users/Scarlett/Downloads/Action/Batman.Begins.srt')
Action_file.2 = readtext::readtext('C:/Users/Scarlett/Downloads/Action/MissionImpossibleFallout.srt')
Action_file.3 = readtext::readtext('C:/Users/Scarlett/Downloads/Action/QuantumOfSolace.srt')
Action_file.4 = readtext::readtext('C:/Users/Scarlett/Downloads/Action/Skyfall.srt')
Action_file.5 = readtext::readtext('C:/Users/Scarlett/Downloads/Action/TheDarkKnight.srt')

Action.files.1 = as.data.frame(Action_file)
Action.files.2 = as.data.frame(Action_file.2)
Action.files.3 = as.data.frame(Action_file.3)
Action.files.4 = as.data.frame(Action_file.4)
Action.files.5 = as.data.frame(Action_file.5)

Action.files_1= rbind(Action.files.1,Action_file.2)
Action.files_2= rbind(Action.files_1,Action.files.3)
Action.files_3= rbind(Action.files_2,Action.files.4)
Action.files= rbind(Action.files_3, Action.files.5)

# rbind(Action + Romance)

test.files.all= rbind(test.files, Action.files)



corpus.all <- test.files.all %>%
  select(doc_id,text) %>%
  unnest_tokens("word", text) 

corpus.all %>%
  count(word) %>%
  arrange(desc(n))

# Here we would like to add some personalize dictionaries to remove names of character.
# Maybe we would not do this with the entire corpus, but for now it would be useful
#word = c('jack', 'rose', "l'm", 'christian', 'zidler' )
#characters = as.data.frame(word )
#lexicon = rep('Eazye',5)
#characters= cbind(characters,lexicon)



word = c('jack', 'rose', "l'm", 'christian', 'zidler', 'ethan', 'bond', 'wayn', 'harvey', 'gotham', 'rachel', 'joker', 'batman', 'benji', 'gonna', 'wayn', 'bruce' )
characters_Action = as.data.frame(word)
lexicon = rep('Eazye',17)
characters_Action= cbind(word,lexicon)

data("stop_words")
new_stop_words_Action=rbind(stop_words, characters_Action)
head(new_stop_words_Action)


tidy_corpus.all<-corpus.all %>%
  anti_join(new_stop_words_Action)

data("stop_words")
#new_stop_words=rbind(stop_words, characters)

#data("new_stop_words")
#tidy_corpus<-corpus %>%
#  anti_join(stop_words)

#tidy_corpus<-tidy_corpus %>%
#  anti_join(characters)

#tidy_corpus<-tidy_corpus %>%
#  anti_join(characters)

tidy_corpus.all %>%
  count(word) %>%
  arrange(desc(n))

# Look if this is important to take in count. 
# CLEANING numbers and white spaces
tidy_corpus.all<-tidy_corpus.all[-grep("\\b\\d+\\b", tidy_corpus.all$word),]

tidy_corpus.all$word <- gsub("\\s+","",tidy_corpus.all$word)

tidy_corpus.all<-tidy_corpus.all %>%
  mutate_at("word", funs(wordStem((.), language="en")))

tidy_corpus.all.DTM<- 
  tidy_corpus.all %>%
  count(doc_id, word) %>%
  cast_dtm(doc_id, word, n)

library(topicmodels)
Script_topic_model.all<-LDA(tidy_corpus.all.DTM, k=2, control = list(seed = 321))

Script_topics.all <- tidy(Script_topic_model.all, matrix = "beta")

Script_top_terms.all <- Script_topics.all %>%
  group_by(topic) %>%
  top_n(15, beta) %>%   #Can adjust for more precision
  ungroup() %>%
  arrange(topic, -beta)


Script_top_terms.all %>%
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip()

######################################################################################33
# General Script for  read all my files.

install.packages('readtext') #Useful to read .txt as an object.
files <- list.files(path="C:/Users/Scarlett/Dowloads", pattern="*.srt", full.names=TRUE, recursive=FALSE)

test_file = readtext::readtext(files[1])
raw_scripts = as.data.frame(test_file)
for (i in files[-1]){ script_file = readtext::readtext(i)
  raw_scripts = rbind(raw_scripts,script_file)
}

save(raw_scripts,file = 'Raw_corpus.Rda') #The file is raw. It does not have any transformation or steam 

load("C:/Users/Scarlett/Dowloads/Raw_corpus.Rda")

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

Script_topic_model<-LDA(tidy_corpus.DTM, k=5, control = list(seed = 321)) # 5  different topics, not supervised.
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
