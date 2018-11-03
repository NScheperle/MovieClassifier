import os
import re

fn = os.path.join(os.path.dirname(__file__), '../Subtitles_files/The Dark Knight.srt')
output = "test_output.txt"

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
			
g.close()