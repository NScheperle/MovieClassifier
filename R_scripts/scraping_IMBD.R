library(rvest)
library(tidytext)
install.packages('stringi')
library(stringi)


tconst_names = read.csv("C:/Users/joaqu/OneDrive/Escritorio/Text_Analysis-master/test.csv") #the file contains 5 cols. Cols{3:5} are ignorated
colnames(tconst_names)[1] = 'title'


all_budgets = data.frame(title = rep(x = NA,dim(tconst_names)[1]), tconst = NA, budget = NA)  #Create a DF with the parameters
ind = 0
for (tconst  in tconst_names$tconst){ IMDB_page = read_html(paste0('https://www.imdb.com/title/',tconst,'/')) #iterate above a list of tconst index
one_node = html_node(IMDB_page, xpath='//*[@id="titleDetails"]')
head(one_node)
IMDB_text = html_text(one_node)

pattern <- '(\\w* \\w+): *\\$([\\d\\,]+)'   #Regex patter to select only the budget. Possible problem with international movies
ind = ind + 1
Budgets = IMDB_text %>%
  stri_subset(regex= pattern) %>%
  stri_match_all(regex=pattern)

all_budgets$title[ind] = as.character(tconst_names$title[ind])
all_budgets$tconst[ind] = tconst
if (length(Budgets) == 0){all_budgets$budget[ind] = NA} #some movies elected on the script set has no budget or ar international
else {all_budgets$budget[ind] = Budgets}} 
#I do not know if IMBD will block after many queries on the page (i don't think so because is not an API, but maybe it could be 
# a good idea employ a sleep time)


