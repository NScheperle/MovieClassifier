from pythonopensubtitles.opensubtitles import OpenSubtitles
from ost_login import ost_login
import pprint
ost = OpenSubtitles()
token = ost_login(ost)


imdb_id = '134119'
#movie_id = '31260'
#subtitle_id = '7225676'
#title = 'The Dark Knight'

data = ost.search_subtitles([{'sublanguageid':'eng', 'imdbid':imdb_id}])

subtitle_id = data[0].get('IDSubtitle')
subtitlefile_id = data[0].get('IDSubtitleFile')
ost.download_subtitles([subtitlefile_id], override_filenames = {subtitle_id:"The Talented Mr. Ripley"}, output_directory="/home/nes31")

pprint.pprint(ost.get_imdb_movie_details(imdb_id), width=10)

pprint.pprint(data[0], width=2)

#i added some stuff