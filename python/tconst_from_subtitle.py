#Get the tconst of the movies on the subtitle table.
#Save this in a file
#Use this file to feed the R_script to scrap budget and ranking
import os
import re
import mysql.connector

cnx = mysql.connector.connect(user='jm622', database= 'IMDB')
cur = cnx.cursor()
query = "select distinct(tconst) from subtitles;"


output = '/home/jm622/tconst_in_sql.csv'
out = open(output, 'w')
try:
	cur.execute(query)
	result =  cur.fetchall()
#print(query % title + str(result))
except:
	print('something wierd happen with the query: ')
	print('\n ------------------------------------ \n')

for tcon in result:
    
    tcon = tcon[0].replace("(\'","")
    tcon = tcon.replace("\',)","")
    out.write(str(tcon) + ',\n')
cnx.close()
out.close()