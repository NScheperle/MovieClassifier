import os
import re
import mysql.connector

cnx = mysql.connector.connect(user='jm622', database= 'IMDB')
cur = cnx.cursor()
query = ("SELECT tconst, primaryTitle , startYear FROM titles WHERE titleType = 'movie' AND originalTitle  LIKE '%s' ORDER BY primaryTitle DESC;") 
#idea is to cathc the first and last letter to see If we have lucky

output = '/home/jm622/queries.txt'

out = open(output, 'w')
with open('/home/jm622/names2.txt', 'r') as fn:
    for name in fn:
        title = str(name)
        title = title.strip()
        #print(title)
        try:
            cur.execute(query % (title))
            result =  cur.fetchall()
            out.write(title + '****' + str(result)) #need to write the output...would it work??
            #print(query % title + str(result))
            out.write("----------------------------\n---------------------------\n")
            
        except:
            print('something wierd happen with the query: ' + title)
            print('\n ------------------------------------ \n')
            
cnx.close()
out.close()

