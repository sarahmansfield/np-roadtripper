# function to return geoJSON object containing route info
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
  
  coordinates <- list(c(input_long, input_lat), c(park_long, park_lat))
  radius = c(10000, 10000)
  directions <- ors_directions(coordinates, radiuses = radius)
  directions
}

# function to return step by step directions (tibble)
get_steps <- function(directions) {
  steps <- directions$features[[1]]$properties$segments[[1]]$steps
  
  distance <- round(map_dbl(steps, `[[`, "distance") / 1609, 2) 
  duration <- round(map_dbl(steps, `[[`, "duration") / 60, 2)
  instruction <- map_chr(steps, `[[`, "instruction")
  name <- map_chr(steps, `[[`, "name")
  
  directions.df <- tibble(instruction, distance, duration, name) %>%
    rename(Direction = instruction, 
           `Distance (mi)` = distance,
           `Duration (min)` = duration,
           Road = name)
  directions.df
}