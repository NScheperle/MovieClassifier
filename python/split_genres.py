import mysql.connector
import time


cxn = mysql.connector.connect(user="nes31", database="IMDB")
cur = cxn.cursor()
def parse_genres(movie_genres):
	genre_dict = {}
	for movie in movie_genres:
		genre_dict[movie[0]] = movie[1].split(",")
	return genre_dict
	
def get_genres(limit):
	genre_query = ("select tconst, genres from titles where tconst not in (select tconst from genres) and genres is not null limit %s ;")
	cur.execute(genre_query, (limit,))
	return cur.fetchall()
	
def insert_genres(genre_dict):
	insert_query = ("insert into genres (tconst, genre) "
						"values (%s, %s)")
	i = 0
	for movie in genre_dict:
		for genre in genre_dict[movie]:
			cur.execute(insert_query, (movie, genre))
		if i % 100000 == 0:
			print("{} records committed".format(i))
			cxn.commit()
		i += 1
	cxn.commit()
	
start = time.time()
current = time.time()

remaining_query = "select count(*) from titles where tconst not in (select tconst from genres) and genres is not null"
cur.execute(remaining_query)
rem = cur.fetchone()[0]

print("*******************************************")
print("{} titles remaining to be parsed".format(str(rem)))

limit = 10000000
print("Getting genres...")
genres = get_genres(limit)
print("Finished getting genres in {}".format(str(time.time() - current)))
current = time.time()

print("Parsing genres...")
genre_dict = parse_genres(genres)
print("Finished parsing genres in {}".format(str(time.time() - current)))
current = time.time()

print("Inserting genres...")
insert_genres(genre_dict)
print("Finished inserting genres in {}".format(str(time.time() - current)))

print("Total time for {} titles = {}".format(str(limit), str(time.time() - start)))
print("*******************************************")