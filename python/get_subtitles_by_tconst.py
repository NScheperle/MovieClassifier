'''
This scripts has the purpose to query for all the dialogues in the base and save every file in a .txt format 
with the tcosnt as the name of the file. 
In this way is easier to upload files in R.

The file with the tconst names is provided by  the bash command:
    mysql IMDB --execute="select distinct(tconst) from subtitles" > $HOME/tconst.txt
    or with the py script: tconst_from_subtitle.py

'''

import os
import re
import mysql.connector

cnx = mysql.connector.connect(user='jm622', database= 'IMDB')
cur = cnx.cursor()
query = "select dialogue from subtitles where tconst = '%s';"
esc_header = 0

with open('/home/jm622/subtitles/tconst_genre.csv', 'r') as fn: #rename with tconst.txt next time
    for name in fn:
        print(name)
        if esc_header >= 1:
            name = name.split('\t')
            name = name[0]
            name_file = '/home/jm622/subtitles/' + str(name).strip() + '.txt'
            print(name_file)  
            print(name)
            with open(name_file, 'w') as out:
                try:
                    print(query % str(name))
                    cur.execute(query % (name))
                    result =  cur.fetchall()  
                    print(result)
                    out.write(str(result))
                except:
                    print('something wierd happen with the query: ' + name)
                    print('\n ------------------------------------ \n')
                     #need to write the output...would it work??
                    #print(query % title + str(result))
        esc_header += 1
cnx.close()
print('Total queries: Fucking %s' %esc_header)