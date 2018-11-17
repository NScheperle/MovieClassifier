'''
This Script creat a table cluster_genres and upload all the values inside a .csv.
The csv called actual_movies.csv came from the R script Scripts_DTM that is the one we use to calculate IT_IDF
'''

import os
import re
import mysql.connector

cnx = mysql.connector.connect(user='jm622', database= 'IMDB')
cur = cnx.cursor()
query1 = 'CREATE TABLE IMDB.cluster_genre(tconst VARCHAR(9),Genre_1 VARCHAR(50),Genre_2 VARCHAR(50),Genre_3 VARCHAR(50),CLUSTER int(1) ,PRIMARY KEY(tconst));'

cur.execute(query1)

query2 = "INSERT INTO cluster_genre (tconst, Genre_1, Genre_2, Genre_3,CLUSTER) VALUES(%s,%s,%s,%s,%s);"

with open('/home/jm622/actual_movies.csv', 'r') as values:
    cont = 0 #First values is header. We are going to skip
    for line in values:
        print(cont) 
        line = line.split(',')
        tconst = line[1].replace('"','')
        Genre_1 = line[2].replace('"','')
        Genre_2 = line[3].replace('"','')
        Genre_3 = line[4].replace('"','')
        CLUSTER = line[5].replace('"','')

        
        Genre_1 = line[2].replace("\'",'')
        if Genre_1 == '':
            Genre_1 = None
        else:
            try:
                Genre_1 = Genre_1.strip()
                Genre_1 = str(Genre_1)
            except:
                pass
        Genre_2 = line[3].replace("\'",'')
        if Genre_2 == '':
            Genre_2 = None
        else:
            try:
                Genre_2 = Genre_1.strip()
                Genre_2 = str(Genre_2)
            except:
                pass
        Genre_3 = line[4].replace("\'",'')
        
        if Genre_3 == '':
            Genre_3 = None
        else:
            try:
                Genre_3 = Genre_3.strip()
                Genre_3 = int(Genre_3)
            except:
                pass
        CLUSTER = line[5].replace("\'",'')
        try:
            CLUSTER = CLUSTER.strip()
        except:
            pass
        if CLUSTER == 'NA':
            CLUSTER = None
        else:
            print(CLUSTER)
            try:
                CLUSTER = int(CLUSTER)
            except:
                pass
        if cont > 0:
            try:
                cur.execute(query2,(tconst,Genre_1, Genre_2, Genre_3,CLUSTER))
                cnx.commit()
                
            except Exception as e:
                print(e)
                pass
        cont =+ 1
print(cont)            
cnx.close()

