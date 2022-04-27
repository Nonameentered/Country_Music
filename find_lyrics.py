#!/usr/bin/env python
# coding: utf-8

# In[2]:

# In[3]:

import sys
import lyricsgenius
genius_token = sys.argv[1]
genius = lyricsgenius.Genius(genius_token)
genius.verbose = False
genius.remove_section_headers = True
genius.skip_non_songs = True


# In[4]:


from tqdm.auto import tqdm
tqdm.pandas()


# In[5]:


import sqlite3
from dotenv import load_dotenv
load_dotenv()
import os
import pickle


# In[6]:
SQL_FILEPATH = sys.argv[2]
lyrics_db = sqlite3.connect(SQL_FILEPATH)


# In[7]:


# Load Data


# In[8]:


import pandas as pd
df = pd.read_sql_query("SELECT * FROM tracks", lyrics_db)


# ## Create Unique Songs Dataframe

# In[9]:


df.rename(columns = {'index':'track_id'}, inplace=True)



# ## Find Lyrics

# In[11]:


def find_lyrics(row):
    try:
        return genius.search_song(row["track"], row["artist"])
    except:
        return "Error"


# 

# In[ ]:


df["lyrics"] = df.progress_apply(find_lyrics, axis=1)
df.to_pickle("22-04-21-lyrics-pickle")


# In[ ]:


df.to_sql("new_tracks", lyrics_db)

