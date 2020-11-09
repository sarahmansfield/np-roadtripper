# spotify scraping tutorial
# https://machinelearningknowledge.ai/tutorial-how-to-use-spotipy-api-to-scrape-spotify-data/
# https://rayheberer.ai/archive/spotifyapi/

clientID <- "424f8cebaa33461eb2e2ee3f821291a4"
secret <- "eea82f38c4d44094b8e0c328c9a11885"

weekly.top.songs = read_html("https://spotifycharts.com/regional/global/weekly/latest") %>%
  html_nodes("#content > div > div > div > span > table > tbody > tr > td.chart-table-image > a")