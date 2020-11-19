library(spotifyr)
library(lubridate)


Sys.setenv(SPOTIFY_CLIENT_ID = '424f8cebaa33461eb2e2ee3f821291a4')
Sys.setenv(SPOTIFY_CLIENT_SECRET = 'eea82f38c4d44094b8e0c328c9a11885')

access_token <- get_spotify_access_token()

beatles <- get_artist_audio_features('the beatles')

# get_my_recently_played(limit = 5) %>% 
#   mutate(artist.name = map_chr(track.artists, function(x) x$name[1]),
#          played_at = as_datetime(played_at)) %>% 
#   select(track.name, artist.name, track.album.name, played_at) %>% 
#   kable()

# create playlist that is an equal mix of selected genres
categories <- get_categories()

genres <- c("acoustic",
            "alternative", 
            "chill", 
            "classical", 
            "dance", 
            "edm", 
            "funk", 
            "grunge", 
            "hip-hop", 
            "holidays", 
            "indie", 
            "jazz",
            "kids",
            "k-pop", 
            "pop", 
            "punk",
            "r-n-b",
            "rock", 
            "soul", 
            "world-music")

recs <- get_recommendations(limit = 50, seed_genres = c("k-pop", 
                                                        "pop", 
                                                        "punk"))

