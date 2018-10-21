from pythonopensubtitles.opensubtitles import OpenSubtitles
from ost_login import ost_login
import mysql.connector
import pprint
import os
import time

ost = OpenSubtitles()
token = ost_login(ost)

def request_subtitle(imdb_id):
	search = ost.search_subtitles([{'sublanguageid':'eng', 'imdbid':imdb_id}])
	max_downloads = 0
	result = None
	for subtitle in search:
		if subtitle['SubEncoding'] not in ['UTF-8', 'ASCII', 'CP1252']:
			print('excluding this one bc encoding')
			continue
		if int(subtitle['SubBad']) != 0 or (int(subtitle['SubSumVotes']) > 0 and float(subtitle['SubRating']) < 8.0):
			print('excluding this one bc bad')
			print(subtitle['SubBad'], subtitle['SubSumVotes'], subtitle['SubRating'])
			continue
		
		if int(subtitle['SubDownloadsCnt']) > max_downloads:
			max_downloads = int(subtitle['SubDownloadsCnt'])
			result = subtitle
		
	return result

def get_imdb_ids():
	cxn = mysql.connector.connect(user="nes31", database="IMDB")
	cur = cxn.cursor()
	
	query = ("select substr(a.tconst,3) as imdb_id from titles a inner join ratings b on a.tconst = b.tconst "
				"where a.titleType = 'movie' and b.numVotes > 10000 and a.isAdult = 0 and a.startYear > 1998 and a.genres not like '%Documentary%'")
				
	cur.execute(query)
	
	imdb_ids = cur.fetchall()
	return imdb_ids
				

imdb_id = '120338'
imdb_ids = get_imdb_ids()

file_ids = []
file_names = {}
for i,id in enumerate(imdb_ids):
	subtitle = request_subtitle(id)
	file_id = subtitle.get('IDSubtitleFile')
	file_ids.append(file_id)
	file_names[file_id] = subtitle.get('MovieName')
	if i % 20 == 0:
		time.sleep(5)

outdir = os.path.join(os.path.dirname(__file__), '../Subtitles_files/')
for i in range(0,len(file_ids), 20):
	ost.download_subtitles(file_ids[i:min(len(file_ids),i+20)], output_directory=outdir)
#subtitle = request_subtitle(imdb_ids[0][0])
subtitle = request_subtitle(imdb_id)
subtitlefile_id = subtitle.get('IDSubtitleFile')

outdir = os.path.join(os.path.dirname(__file__), '../Subtitles_files/')
ost.download_subtitles([subtitlefile_id], output_directory=outdir)


#imdb_id = '134119'
#movie_id = '31260'
#subtitle_id = '7225676'
#title = 'The Dark Knight'

#data = ost.search_subtitles([{'sublanguageid':'eng', 'imdbid':imdb_id}])

#subtitle_id = data[0].get('IDSubtitle')
#subtitlefile_id = data[0].get('IDSubtitleFile')

#outdir = os.path.join(os.path.dirname(__file__), '../Subtitles_files/')

#print(type(subtitle_id))

#ost.download_subtitles([subtitlefile_id], override_filenames = {subtitle_id:"The Talented Mr Ripley.srt"}, output_directory=outdir)

#pprint.pprint(ost.get_imdb_movie_details(imdb_id), width=100)

#pprint.pprint(data[0], width=100)

#i added some stuff

# i added some more