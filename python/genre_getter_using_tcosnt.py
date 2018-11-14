import os
import re
import mysql.connector

# It get the genre of a txt file with one column, this column  containt the tconst of the movies to get the genre for a possible topic modeling

cnx = mysql.connector.connect(user='jm622', database= 'IMDB')
cur = cnx.cursor()
query = ("select tconst,genres from titles where tconst = '%s';") 

output = '/home/jm622/tconst_genres.txt'

out = open(output, 'w')
with open('/home/jm622/tconst.txt', 'r') as fn:
    for name in fn:
        title = str(name)
        title = title.strip()
        print(title)
        print(query % title)
        try:
            cur.execute(query % (title))
            result =  cur.fetchall()
            out.write(str(result)+'\n') #need to write the output...would it work??
            #print(query % title + str(result))
            
            
        except:
            print('something wierd happen with the query: ' + title)
            print('\n ------------------------------------ \n')
            
cnx.close()
out.close()