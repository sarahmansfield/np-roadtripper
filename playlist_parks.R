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

 client_id <- '424f8cebaa33461eb2e2ee3f821291a4'
 client_secret <- 'eea82f38c4d44094b8e0c328c9a11885'
 token <- POST('https://accounts.spotify.com/api/token',
               accept_json(), 
               authenticate(client_id, client_secret),
               body = list(grant_type = 'client_credentials'),
               encode = 'form', 
               httr::config(http_version = 2)) %>% content %>% .$access_token 



get_cover_art <- function(playlistID) {
  
  url = str_c("https://api.spotify.com/v1/playlists/", playlistID, "/images")
  

  #p <- GET(url, query = list(access_token = token))
  res <- RETRY('GET', url, query = list(access_token = token), encode = 'json')
  stop_for_status(res)
  res <- fromJSON(content(res, as = 'text', encoding = 'UTF-8'), flatten = TRUE)
  return(res)

}




get_parks_project_songs <- function(playlistID) {
  
  url = str_c("https://api.spotify.com/v1/playlists/", playlistID, "/tracks")
  
  # p <- content(GET(url = sprintf(url),
  #             config = add_headers(authorization = authorization.header)))
  p <- GET(url, query = list(access_token = token)) %>% content
  
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





