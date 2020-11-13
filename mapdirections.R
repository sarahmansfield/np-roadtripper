# function to return a leaflet map showing the travel route
get_route <- function(startpoint, park) {
  x <- ors_geocode(startpoint, boundary.country = "US")
  
  # user inputted location coordinates
  input_long <- x$bbox[1]
  input_lat <- x$bbox[2]
  
  parkinfo <- readRDS("data/parkfeatures.rds")
  park_long <- parkinfo %>%
    filter(parkname == park) %>%
    select(longitude) %>%
    pull()
  park_lat <- parkinfo %>%
    filter(parkname == park) %>%
    select(latitude) %>%
    pull()
  
  # map
  coordinates <- list(c(input_long, input_lat), c(park_long, park_lat))
  radius = c(10000, 10000)
  directions <- ors_directions(coordinates, radiuses = radius)
  
  leaflet() %>%
    addTiles() %>%
    addGeoJSON(directions, fill = FALSE) %>%
    fitBBox(directions$bbox) %>%
    addMarkers(directions, lng = park_long, lat = park_lat)
}