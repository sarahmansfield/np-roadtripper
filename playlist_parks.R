library(jsonlite)
library(tidyverse)
library(httr)
library(plyr)
library(data.table)
library(rvest)
library(jsonlite)
library(httr)
library(magrittr)
library(tidyverse)
library(dplyr)
library(plyr)
library(stringr)

library(data.table)
library(purrr)
library(httr)
library(assertthat)


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

get_parks_project_songs <- function(playlistID) {
  
  url = str_c("https://api.spotify.com/v1/playlists/", playlistID, "/tracks")
  
  p <- content(GET(url = sprintf(url),
              config = add_headers(authorization = authorization.header)))
  
  toJSON(p$items) -> df1
  fromJSON(df1) %>% as.data.frame -> df2
  df2$track -> df_track
  df_track$album_name <- df_track$album$name
  
  df_track$artists -> track_artists
  lapply(track_artists, "[[", 4) -> track_artist_name
  lapply(track_artist_name, `[[`, 1) -> track_artist_name
  lapply(track_artists, "[[", 3) -> track_artist_id
  lapply(track_artist_id, `[[`, 1) -> track_artist_id
  
  df_track$artist_name <- track_artist_name
  
  df_track %>%
    select(name, uri, popularity, artist_name, track_number, id, href, album_name, duration_ms)
}

p <- get_parks_project_songs("117MQyf7iOjLaUVN7zcJw6")


p1 <- get_playlist_cover_image("117MQyf7iOjLaUVN7zcJw6")

