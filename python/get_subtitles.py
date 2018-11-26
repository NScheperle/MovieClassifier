from pythonopensubtitles.opensubtitles import OpenSubtitles
from ost_login import ost_login
import mysql.connector
import pprint
import os
import time
import sys

ost = OpenSubtitles()
ost.user_agent = "mcduke"
token = ost_login(ost)
start = time.time()
def request_subtitle(imdb_id):
	try:
		search = ost.search_subtitles([{'sublanguageid':'eng', 'imdbid':imdb_id}])
	except Exception as e:
		print("time elapsed = {}".format(time.time() - start))
		print("Encountered rate limit error. Sleeping 5...")
		print(e)
		time.sleep(5)
		return request_subtitle(imdb_id)
	if search == None:
		print(imdb_id)
		return None
	max_downloads = 0
	result = None
	for subtitle in search:
		if subtitle == None:
			continue
		if subtitle['SubEncoding'] not in ['UTF-8', 'ASCII', 'CP1252']:
			continue
		if subtitle['SubFormat'] != 'srt':
			continue
		if int(subtitle['SubBad']) != 0 or (int(subtitle['SubSumVotes']) > 0 and float(subtitle['SubRating']) < 8.0):
			continue
		
		if int(subtitle['SubDownloadsCnt']) > max_downloads:
			max_downloads = int(subtitle['SubDownloadsCnt'])
			result = subtitle
		
	return result
	
def download_batch(batch, file_names):
	print(batch, file_names)
	outdir = os.path.join(os.path.dirname(__file__), '../Subtitles_files/')
	try:
		ost.download_subtitles(batch, output_directory=outdir)
	except Exception as e:
		print("Encountered rate limit error. Sleeping 5...")
		print(e)
		time.sleep(5)
		download_batch(batch, file_names)
		return
	for file_id in batch:
		try:
			os.rename(os.path.join(outdir, file_id + ".srt"), os.path.join(outdir, file_names[file_id]))
		except FileNotFoundError:
			print("File for {} not found. File ID = {}".format(file_names[file_id], file_id))

def get_imdb_ids():
	cxn = mysql.connector.connect(user="nes31", database="IMDB")
	cur = cxn.cursor()
	
	query = ("select substr(a.tconst,3) as imdb_id, primaryTitle from titles a inner join ratings b on a.tconst = b.tconst "
				"where a.titleType = 'movie' and b.numVotes > 10000 and a.isAdult = 0 and a.startYear > 1998 and a.genres not like '%Documentary%' "
				"and a.tconst not in (select distinct tconst from subtitles) "
				"limit 250	")
				
	cur.execute(query)
	
	imdb_ids = cur.fetchall()
	return imdb_ids
				

imdb_ids = get_imdb_ids()
#sys.exit()

file_ids = []
file_names = {}
for i,imdb_info in enumerate(imdb_ids):
	imdb_id = imdb_info[0]
	title = imdb_info[1]
	subtitle = request_subtitle(imdb_id)
	if subtitle == None:
		print("Not found: {}, {}".format(title,imdb_id))
		continue
	file_id = subtitle.get('IDSubtitleFile')
	file_ids.append(file_id)
	file_names[file_id] = ".".join([subtitle.get('MovieName').replace(".",""), "tt" + imdb_id])
	if i % 20 == 0 and i != 0:
		print("i = {}, sleeping...".format(i))
		#time.sleep(10)
		
		
download_limit = 20
for i in range(0,len(file_ids), download_limit):
	id_batch = file_ids[i:min(len(file_ids),i+download_limit)]
	download_batch(id_batch, file_names)
	print("Sleeping a few seconds to wait for download...")
	time.sleep(3)
	if i % (download_limit*30) == 0 and i != 0:
		print("i={}, sleeping...".format(i))
		print("psych")
		#time.sleep(10)
