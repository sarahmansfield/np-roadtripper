devtools::install_github('charlie86/spotifyr')
library(spotifyr)
library(lubridate)
library(knitr)


 Sys.setenv(SPOTIFY_CLIENT_ID = '424f8cebaa33461eb2e2ee3f821291a4')
 Sys.setenv(SPOTIFY_CLIENT_SECRET = 'eea82f38c4d44094b8e0c328c9a11885')

access_token <- get_spotify_access_token()

get_token <- function(){
  clientID <- "424f8cebaa33461eb2e2ee3f821291a4"
  secret <- "eea82f38c4d44094b8e0c328c9a11885"
  
  response = POST(
    'https://accounts.spotify.com/api/token',
    accept_json(),
    authenticate(clientID, secret),
    body = list(grant_type = 'client_credentials'),
    encode = 'form'
  )
  
  token = content(response)$access_token
  authorization.header = paste0("Bearer ", token)
}

authorization.header = get_token()



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

get_playlist_genre <- function(genres) {
  
  # get recommendations 
  recs <- get_recommendations(limit = 100, seed_genres = genres) 
  
  artist <- map(recs$artists, `[[`, "name")
  
  artist_name <- rep(NA, length(artist))
  
  for (i in 1:length(artist)) {
    artist_name[i] <- artist[[i]] %>%
      paste(collapse = ", ")
  }
  
  as.data.frame(cbind(recs, artist_name)) %>%
    select(-artists) 

}



# # filter amount of songs based on trip_time
# time_sum <- c()
# for (i in 1:length(recs)) {
#
#   time_sum[i] <- sum(time_sum) + recs$duration_ms[i]
#
#   if (time_sum[i] > trip_time) {
#     break
#   }
# }
#
# recs[1:length(time_sum), ]

# test - create playlist and add tracks


# g <- c("k-pop", "pop", "punk")
# x <- get_playlist_genre(g)
#
# create_playlist(user_id = "aasha_r", name = "Park Playlist")
#
# track_uris <- x$uri %>% paste(collapse = ",")
#
#
#
# playlist_id <- get_my_playlists() %>%
#   filter(name == "Park Playlist") %>%
#   select(id) %>%
#   unlist()
#
# #https://api.spotify.com/v1/playlists/{playlist_id}/tracks
#
# url = paste0("https://api.spotify.com/v1/playlists/", playlist_id, "/tracks?",
#             "uris=", "spotify:track:5n2BIbMpa2j2CHsOcsFG5V,spotify:track:0C71NJD4BhvPopTwI7a8KV")
#
# fromJSON(url)
#
# p <- content(GET(url = sprintf(url),
#                  config = add_headers(authorization = authorization.header)))
#
#
# #"https://api.spotify.com/v1/playlists/6MUSN8cFMp3UzhUMZi3PiS/tracks?uris=spotify%3Atrack%3A1rFMYAZxBoAKSzXI54brMu%2Cspotify%3Atrack%3A0J3DXNGMAW5ZcspbCB6Is6%2Cspotify%3Atrack%3A4QtiVmuA88tPQiCOHZuQ5b" -H "Accept: application/json" -H "Content-Type: application/json" -H "Authorization: Bearer BQAQLmYhAq9bASZ6r2Lve_8PIIA4NMrIAwzsP0VjdXqv4lCS-LzwE1nqlsJYXPD5B1g8XEKrcNbePC0ea0RLXGaXjD4Y72FeUa6AnGBYUnT8GG8L_2ZjqsHb-u2VLsjNOFVEvlXN8iAvtsRYVvR1K9yiR2g8lSHNfcU9AHRH-gY"
#
# # GET user authorization code#
# code <- get_spotify_authorization_code(client_id = Sys.getenv("SPOTIFY_CLIENT_ID"),
#                                        client_secret = Sys.getenv("SPOTIFY_CLIENT_SECRET"),
#                                        scope = "playlist-modify")
#
# #Save code#
# code2 = code[["credentials"]][["access_token"]]
# usercode <- paste0("Bearer ", code2)
#
# #Add track to playlist#
# POST("https://api.spotify.com/v1/playlists/6MUSN8cFMp3UzhUMZi3PiS/tracks?uris=spotify%3Atrack%3A1rFMYAZxBoAKSzXI54brMu%2Cspotify%3Atrack%3A0J3DXNGMAW5ZcspbCB6Is6%2Cspotify%3Atrack%3A4QtiVmuA88tPQiCOHZuQ5b",
#      encode="json",
#      add_headers(Authorization = usercode),
#      body = "{\"texts\":[\"A simple string\"]}")
#
#
# z <- get_category_playlists(category_id = "pop", limit = 50)
# # select random ID
# sample(z$id, 1)
# track <- get_playlist_tracks(sample(z$id, 1))
#
# f <- get_categories()
#
# get_f