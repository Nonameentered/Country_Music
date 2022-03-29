import sqlite3
import pandas as pd

lyrics_db = sqlite3.connect('mxm_dataset.db')
track_db = sqlite3.connect('track_metadata.db')
compiled_db = sqlite3.connect('song_project.db')

df = pd.read_csv('files/msd_tagtraum_cd2c.cls', sep="\t", header=None, skiprows=7)
df.columns = ["Track ID", "Genre"]
df_filtered = df[df["Genre"] == "Country"]
print(df_filtered.head())

# Tried filtering with sqlite, but following suggests there is a track limit, and I don't have the proper ? parameter
# https://stackoverflow.com/questions/5766230/select-from-sqlite-table-where-rowid-in-list-using-python-sqlite3-db-api-2-0
# df_tracks = pd.read_sql_query(f"SELECT * FROM songs WHERE track_id in ({','.join(df_filtered['Track ID'])})", track_db)
df_tracks = pd.read_sql_query("SELECT * FROM songs", track_db)

df_tracks_country = df_tracks[df_tracks["track_id"].isin(df_filtered["Track ID"])]

print(df_tracks_country.shape)

df_tracks_country.to_sql("songs", compiled_db)

# compiled_db.execute('''CREATE TABLE artist
#                (artist_id text, artist_name text, genre text, gender text, birthyear text)''')

# df = pd.read_sql_query("SELECT * FROM  artist_term WHERE term LIKE '%country%'", artist_db)
# print(df.head())


# Get the contents of a table
# artist_cursor = artist_db.cursor()
# artist_cursor.execute('''SELECT * FROM  artist_term WHERE term LIKE '%country%' ''')
# output = artist_cursor.fetchall()   # Returns the results as a list.

# Insert those contents into another table.
# compiled_cursor = compiled_db.cursor()
# for row in output:
#     print(output)
    # compiled_cursor.execute('INSERT INTO artist VALUES (row)', row)

compiled_db.commit()
lyrics_db.close()
track_db.close()
compiled_db.close()
