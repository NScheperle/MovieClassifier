import os
import re
import mysql.connector

cnx = mysql.connector.connect(user='jm622', database= 'IMDB')
cur = cnx.cursor()
#query1 = 'CREATE TABLE IMDB.budget_rating\ 
        #(tconst VARCHAR(9), \
        #budget integer,\
        #weekend_usa integer,\
        #gross_usa integer,\
        #worldwide_gross integer\
        #rating float,\
        #PRIMARY KEY(tconst));'

#cur.execute(query1)
query2 = "INSERT INTO budget_rating (tconst,budget,weekend_usa,gross_usa,worldwide_gross,rating) VALUES(%s,%s,%s,%s,%s,%s);"

with open('/home/jm622/budget_ranking_subtitles.csv', 'r') as values:
    cont = 0
    for line in values:
        line = line.split(',')
        tconst = line[1].replace('\"','')
        budget = line[2].replace("\'",'')
        if budget == 'NULL':
            budget = None
        else:
            try:
                budget = int(budget)
            except:
                pass
        w_usa = line[3].replace("\'",'')
        if w_usa == 'NULL':
            w_usa = None
        else:
            try:
                w_usa = int(w_usa)
            except:
                pass
        g_usa = line[4].replace("\'",'')
        if g_usa == 'NULL':
            g_usa = None
        else:
            try:
                g_usa = int(g_usa)
            except:
                pass
        world_g = line[5].replace("\'",'') 
        if world_g == 'NULL':
            world_g = None
        else:
            try:
                world_g = int(world_g)
            except:
                pass
        rating = line[6].replace("\'",'').strip()
        try:
            rating = float(rating)
        except:
            pass
        
        if cont > 0:
            cur.execute(query2,(tconst,budget,w_usa,g_usa,world_g,rating))
        cont =+ 1
cnx.close()


