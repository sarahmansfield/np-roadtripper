devtools::install_github('charlie86/spotifyr')
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

# function that creates a new playlist for spotify user based on up to 5 genres

playlist <- function(genres, trip_time) {
  
  # get recommendations 
  recs <- get_recommendations(limit = 100, seed_genres = genres) 
  
  # filter amount of songs based on trip_time
  time_sum <- c()
  for (i in 1:length(recs)) {
    
    time_sum[i] <- sum(time_sum) + recs$duration_ms[i]
    
    if (time_sum[i] > trip_time) {
      break
    }
  }
  
  recs[1:length(time_sum), ]
}




g <- c("k-pop", "pop", "punk")
x <- playlist(g, trip_time)


recs <- get_recommendations(limit = 100, seed_genres = c("k-pop", 
                                                        "pop", 
                                                        "punk")) 

artist <- map(recs$artists, `[[`, "name") %>% unlist()



