library(rvest)
library(tidytext)
#install.packages('stringi')
library(stringi)

# the csv was obtained using this query:
# mysql IMDB --execute="select distinct(subtitles.tconst),titles.originalTitle from sub titles left join titles on subtitles.tconst = titles.tconst" > '/home/jm622/tconst_genre.csv'
setwd('C:/Users/joaqu/OneDrive/Escritorio/MovieClassifier/R_scripts/')
tconst_names = read.csv("tconst_genre.csv", stringsAsFactors = F, sep = '\t') #the file contains 5 cols. Cols{3:5} are ignorated
tconst_names = tconst_names[-1,]  #I dont know why the querry give one row with null value after the header. Delete.
rownames(tconst_names) <- NULL

#all_budgets = data.frame(title = rep(x = NA,dim(tconst_names)[1]), tconst = NA, budget = NA,  weekend.usa = NA, gross.usa = NA,
                         #worldwide.gross = NA, ranking = NA)  #Create a DF with the parameters




load('RDA_objects/budget_ranking_subtitles.RDA')
ind = dim(all_budgets)[1]  # I START  ON MY LAST ROW SO I DONT SCRAP MORE THAN NEEDED
all_budgets[c((dim(all_budgets[1])+ 1):dim(tconst_names[1])),]= NA    #This is neccesary to start on the last place the table ended

# I can load the RDA object subtitles_all_budget.RDA
# So, in this way I dont need to scrap all the things that I have already scrapped.

for (tconst  in tconst_names$tconst[-c(1:ind)]){IMDB_page = read_html(paste0('https://www.imdb.com/title/',tconst,'/')) 
                                    #iterate above a list of tconst index
                                    one_node = html_node(IMDB_page, xpath='//*[@id="titleDetails"]')
                                    #head(one_node)
                                    IMDB_text = html_text(one_node)
                                    pattern <- '(\\w* \\w+): *\\$([\\d\\,]+)' 
                                    #Regex patter to select only the budget. Possible problem with international movies
                                    
                                    second_node =  html_node(IMDB_page, xpath='//*[@id="title-overview-widget"]/div[1]/div[2]/div/div[1]/div[1]/div[1]/strong/span')
                                    ranking = html_text(second_node)
                                    #Getting ranking
                                    
                                    ind = ind + 1
                                    if (length(ranking) == 0){all_budgets$ranking[ind] = NA}
                                    else {all_budgets$ranking[ind] = ranking}
                                    
                                    Budgets = IMDB_text %>%
                                      stri_subset(regex= pattern) %>%
                                      stri_match_all(regex=pattern)
                                    #all_budgets$title[ind] = as.character(tconst_names$title[ind])
                                    all_budgets$tconst[ind] = tconst
                                    
                                    if (length(Budgets) == 0){all_budgets$budget[ind] = NA
                                                              all_budgets$weekend.usa[ind] = NA
                                                              all_budgets$gross.usa[ind] = NA
                                                              all_budgets$worldwide.gross[ind] = NA} 
                                    #some movies elected on the script set has no budget or ar international
                                    else{if (length(Budgets[[1]][which(Budgets[[1]][,2] == " Budget"),3]) == 0){all_budgets$budget[ind] = NA}
                                          else {all_budgets$budget[ind] = Budgets[[1]][which(Budgets[[1]][,2] == " Budget"),3]}
                                      
                                        if (length(Budgets[[1]][which(Budgets[[1]][,2] == "Weekend USA"),3]) == 0){  all_budgets$weekend.usa[ind] = NA}
                                          else {all_budgets$weekend.usa[ind] = Budgets[[1]][which(Budgets[[1]][,2] == "Weekend USA"),3]}
                                      
                                        if (length(Budgets[[1]][which(Budgets[[1]][,2] == "Gross USA" ),3]) == 0){all_budgets$gross.usa[ind] = NA}
                                          else {all_budgets$gross.usa[ind] = Budgets[[1]][which(Budgets[[1]][,2] == "Gross USA" ),3]}
                                      
                                        if (length(Budgets[[1]][which(Budgets[[1]][,2] == "Worldwide Gross"),3]) == 0){  all_budgets$worldwide.gross[ind] = NA} 
                                        else {all_budgets$worldwide.gross[ind] = Budgets[[1]][which(Budgets[[1]][,2] == "Worldwide Gross"),3]} 
                                        }
}
#I do not know if IMBD will block after many queries on the page (i don't think so because is not an API, but maybe it could be 
# a good idea employ a sleep time)

all_budgets$budget= gsub(",","",all_budgets$budget)
all_budgets$weekend.usa= gsub(",","",all_budgets$weekend.usa)
all_budgets$gross.usa= gsub(",","",all_budgets$gross.usa)
all_budgets$worldwide.gross= gsub(",","",all_budgets$worldwide.gross)
save(all_budgets, file = 'budget_ranking_subtitles.RDA')
# all_budgets = all_budgets[2:7] because I do not upload the names.
write.csv(all_budgets,file = 'budget_ranking_subtitles.csv')   #WE USE THIS FILE TO UPLOAD THE DATA TO THE DATABASE!
# --------------------------------


