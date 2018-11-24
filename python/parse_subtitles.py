import os
import re
import mysql.connector
import datetime
import sys

cnx = mysql.connector.connect(user='nes31', database= 'IMDB')
		  
def parse_file(fn):
	tconst = fn.split("/")[-1].split(".")[1]
	cur = cnx.cursor()
	
	check_query = ("SELECT EXISTS(SELECT * from subtitles WHERE tconst = %s);")
	cur.execute(check_query, (tconst,))
	check_exists = cur.fetchone()[0] == 1
	
	if check_exists:
		print("{} already exists in DB".format(fn))
		return
	print(fn)
	print(tconst)
	insert_query = ("INSERT INTO subtitles (tconst, subID, dialogue, start, end)"
          " VALUES (%s, %s, %s, %s, %s);")
	
	with open(fn) as f:
		subtitle_start = 0
		subtitle_times = 1
		dialogue = ""
		subtitle_id = 1
		subtitle = ()
		for line_nbr,line in enumerate(f.readlines()):
			if line == "\n":
				subtitle = (subtitle_id, start_time, end_time, dialogue)
				try:
					cur.execute(insert_query,(tconst, subtitle_id, dialogue, start_time, end_time))
				except Exception as e:
					print ('Some problem with the following query: ')
					print (insert_query,(tconst, subtitle_id, dialogue, start_time, end_time))
					print(line_nbr, line)
					print(e)
					sys.exit()

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
				try:
					subtitle_id += 1
					#subtitle_id = int(line)
				except Exception as e:
					print(line, line_nbr)
					print(e)
					sys.exit()
			elif line_nbr == subtitle_times:
				times_line = line[:29]
				times = times_line.split(" --> ")
				try:
					start_datetime = datetime.datetime.strptime(times[0],'%H:%M:%S,%f')
				except ValueError as e:
					#print(e)
					start_datetime = datetime.datetime.strptime(times[0],'%H:%M:%S.%f')
				start_time = datetime.timedelta(hours=start_datetime.hour,minutes=start_datetime.minute,seconds=start_datetime.second, microseconds=start_datetime.microsecond).total_seconds()
				try:
					end_datetime = datetime.datetime.strptime(times[1],'%H:%M:%S,%f')
				except ValueError as e:
					#print(e)
					end_datetime = datetime.datetime.strptime(times[1],'%H:%M:%S.%f')
				end_time = datetime.timedelta(hours=end_datetime.hour,minutes=end_datetime.minute,seconds=end_datetime.second, microseconds=end_datetime.microsecond).total_seconds()

				
			else:
				dialogue += " " + line
				dialogue = dialogue.strip()
			

	cnx.commit()

i = 1
for file in os.listdir("./Subtitles_files"):
	print(i)
	i += 1
	fn = os.path.join(os.path.dirname(__file__), '../Subtitles_files/'+file)
	try:
		parse_file(fn)
	except Exception as e:
		print(e)
		print("{} fuct up dawg {}".format(fn, file))
		sys.exit()


			
			
cnx.close()