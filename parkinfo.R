# function to compute distance in miles between user specified location and each park
calcDistance <- function(address) {
  query <- str_c("https://api.openrouteservice.org/geocode/search?api_key=5b3ce3597851110001cf6248ddae92a05a2c44bc9da60dcbccdfcbaa&text=",
               address)
  query <- URLencode(query)
  
  # user inputted location coordinates
  input_long <- fromJSON(query)$bbox[1]
  input_lat <- fromJSON(query)$bbox[2]
  
  parkfinal <- readRDS("data/parkfeatures.rds")
  parkfinal <- parkfinal %>% 
    rowwise() %>%
    mutate(distance = distHaversine(cbind(input_long, input_lat),
                                    cbind(longitude, latitude))/1609)
  parkfinal
}


### random forest model ###

# function to return recommendation from rf model
get_parkrec <- function(parkdata, maxdistance, activities, fee, season) {
  # filter on max distance input
  parkdata <- parkdata %>%
    filter(distance <= maxdistance) %>%
    select(-c(longitude, latitude, distance))
  parkdata$parkname <- factor(parkdata$parkname)
  
  rf.mod <- randomForest(parkname~., data = parkdata, mtry = 6, importance = TRUE)
  
  act_names <- c("Astronomy", "Stargazing", "Biking", "Boating", 
                     "Camping", "Climbing", "Fishing", "Hiking", "Paddling",
                     "Canoeing", "Kayaking", "Skiing", "Swimming", "Scenic Driving")
  ind <- c()
  for (i in seq_along(act_names)) {
    # check if an activity was selected
    if (act_names[i] %in% activities) {
      ind[i] <- 1
    } else {
      ind[i] <- 0
    }
  }
  # new data based on user inputs
  new_data <- tibble(astronomy = ind[1], stargazing = ind[2], biking = ind[3], boating = ind[4], 
                     camping = ind[5], climbing = ind[6], fishing = ind[7], hiking = ind[8], 
                     paddling = ind[9], canoeing = ind[10], kayaking = ind[11], skiing = ind[12], 
                     swimming = ind[13], scenicdriving = ind[14], free = fee, bestseason = season)
  
  park <- predict(rf.mod, newdata = new_data)
  parkinfo <- readRDS("data/parks.rds") 
  # return row of info about park
  parkinfo %>%
    filter(parkname == park)
}
