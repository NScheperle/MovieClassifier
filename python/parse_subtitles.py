import os
import re
import mysql.connector

cnx = mysql.connector.connec(user='jm622', database= 'IMDB')
cur = cnx.cursor()
insert = ("INSERT INTO subtitles (tconst, subID, dialogue, start, end)"
          "VALUES (%s, %s, %s, %s, %s);")


fn = os.path.join(os.path.dirname(__file__), '../Subtitles_files/The Dark Knight.srt')
output = "test_output.txt"

tconst = re.findall(r'[\d\w\s]*\.srt',fn)  # Tconst assumes that the name of the file is the tconst
tuple_const = re.split(r'\.',tconst[0])  # Split to get rid of the dot
tconst = tuple_cosnt[0]

g = open(output, "w")

with open(fn) as f:
	subtitle_start = 0
	subtitle_times = 1
	dialogue = ""
	subtitle = ()
	for line_nbr,line in enumerate(f.readlines()):
		if line == "\n":
			subtitle = (subtitle_id, start_time, end_time, dialogue)
			g.write(str(subtitle))
			g.write("\n")
			subtitle_start = line_nbr + 1
			subtitle_times = line_nbr + 2
			subtitle = ()
			dialogue = ""
			continue
		line = line.replace("<i>","").replace("</i>","")
		line = re.sub(r'[A-Z].*:','',line)
		line = line.lstrip("- ")
		line = 	line.strip("\n")
		
		
		if line_nbr == subtitle_start:
			subtitle_id = int(line)
		elif line_nbr == subtitle_times:
			times = line.split(" --> ")
			start_time = times[0]
			end_time = times[1]
		else:
			dialogue += " " + line
			dialogue = dialogue.strip()
        try:
            cur.execute(insert,(tconst, subtitle_id, dialogue, start_time, end_time))
        except:
            print ('Some problem with the following query: ')
            print (insert,(tconst, subtitle_id, dialogue, start_time, end_time))
cnx.close()
  
g.close()