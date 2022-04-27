import musicbrainzngs
from musicbrainzngs import WebServiceError

musicbrainzngs.set_useragent("Nouserentered Song Analysis Project", "0.1.0", contact="matthew.shu@yale.edu"
                             )
artist_id = "04e74624-554c-4689-a811-92e6145f40c0"
try:
    result = musicbrainzngs.get_artist_by_id(artist_id, includes=["tags"])
except WebServiceError as exc:
    print("Something went wrong with the request: %s" % exc)
else:
    artist = result["artist"]
    print("name:\t\t%s" % artist["name"])
    print("sort name:\t%s" % artist["sort-name"])
    print("tags:\t%s" % artist["tag-list"])
    print("gender:\t%s" % artist["gender"])
    print(artist.keys())