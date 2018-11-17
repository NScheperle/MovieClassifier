'''
Once we scrapped the IMDB page, get the budgets, gross and rating of the movies (Using the R script)
We would use this script  to uploaded that data to the database.

Watch out to remote all the "" from the csv file.
'''

import os
import re
import mysql.connector

cnx = mysql.connector.connect(user='jm622', database= 'IMDB')
cur = cnx.cursor()
#query1 = 'CREATE TABLE IMDB.budget_rating (tconst VARCHAR(9),  budget integer, weekend_usa integer, gross_usa integer, worldwide_gross integer , rating float, PRIMARY KEY(tconst));'

#cur.execute(query1)

query2 = "INSERT INTO budget_rating (tconst,budget,weekend_usa,gross_usa,worldwide_gross,rating) VALUES(%s,%s,%s,%s,%s,%s);"

with open('/home/jm622/budget_ranking_subtitles.csv', 'r') as values:
    cont = 0 #First values is header. We are going to skip
    for line in values:
        print(cont) 
        line = line.split(',')
        tconst = line[1].replace('\"','')
        budget = line[2].replace('"','')
        w_usa = line[3].replace('"','')
        g_usa = line[4].replace('"','')
        world_g = line[5].replace('"','')
        rating = line[6].replace('"','')
        if budget == 'NA':
            budget = None
        else:
            try:
                budget = int(budget)
            except:
                pass
        w_usa = line[3].replace("\'",'')
        if w_usa == 'NA':
            w_usa = None
        else:
            try:
                w_usa = int(w_usa)
            except:
                pass
        g_usa = line[4].replace("\'",'')
        if g_usa == 'NA':
            g_usa = None
        else:
            try:
                g_usa = int(g_usa)
            except:
                pass
        world_g = line[5].replace("\'",'') 
        if world_g == 'NA':
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
            try:
                cur.execute(query2,(tconst,budget,w_usa,g_usa,world_g,rating))
                cnx.commit()
                
            except Exception as e:
                print(e)
                pass
        cont =+ 1
print(cont)            
cnx.close()


