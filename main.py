import sqlite3
import pandas as pd

lyrics_db = sqlite3.connect('files/mxm_dataset.db')
track_db = sqlite3.connect('files/track_metadata.db')
compiled_db = sqlite3.connect('files/country_song_project.db')

df = pd.read_csv('files/msd_tagtraum_cd2c.cls', sep="\t", header=None, skiprows=7)
df.columns = ["Track ID", "Genre"]
df_filtered = df[df["Genre"] == "Country"]

df_tracks = pd.read_sql_query("SELECT * FROM songs", track_db)

df_tracks_country = df_tracks[df_tracks["track_id"].isin(df_filtered["Track ID"])]
print(df_tracks_country.shape)
df_tracks_country.to_sql("songs", compiled_db)

df_lyric_words = pd.read_sql_query("SELECT * FROM words", lyrics_db)
print(df_lyric_words.shape)
df_lyric_words.to_sql("words", compiled_db)
df_lyrics = pd.read_sql_query("SELECT * FROM lyrics", lyrics_db)
print(df_lyrics.shape)
df_lyrics_country = df_lyrics[df_lyrics["track_id"].isin(df_filtered["Track ID"])]
df_lyrics_country.to_sql("lyrics", compiled_db)


compiled_db.commit()
lyrics_db.close()
track_db.close()
compiled_db.close()
