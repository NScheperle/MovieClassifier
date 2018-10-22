/* Subtitles Table */
create table IMDB.subtitles
            (tconst VARCHAR(9), 
            subID integer, 
            dialogue text, 
            start float, 
            end float, 
            primary key(tconst,subID));

/* Sentiment Table */                        
create table IMDB.sentiment
            (tconst VARCHAR(9), 
            subID integer, 
            sentiment_value FLOAT,
            primary key (tconst, subID));
			
/* Genres Table */
create table IMDB.genres
			(tconst VARCHAR(9),
			genre VARCHAR(20),
			primary key(tconst, genre));