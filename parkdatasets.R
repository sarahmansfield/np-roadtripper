url <- "https://developer.nps.gov/api/v1/parks?limit=500&api_key=irtQQOJwoFUExahGal9k7UgLRwHrH1RpLoP6QR9d"

parks <- fromJSON(url) %>%
  as_tibble() %>%
  flatten() %>%
  as_tibble() %>%
  filter(data.designation == "National Park") %>%
  select(-c(total, limit, start, data.id, data.latLong, data.entrancePasses,
            data.fees, data.addresses, data.name, data.designation, 
            data.contacts.phoneNumbers, data.contacts.emailAddresses)) %>%
  rename(url = data.url, parkname = data.fullName, code = data.parkCode,
         parkdesc = data.description, latitude = data.latitude,
         longitude = data.longitude, activities = data.activities,
         topics = data.topics, state = data.states, fees = data.entranceFees,
         directioninfo = data.directionsInfo, directionurl = data.directionsUrl,
         hours = data.operatingHours, images = data.images, 
         weatherinfo = data.weatherInfo) %>%
  mutate(longitude = as.numeric(longitude),
         latitude = as.numeric(latitude))
parks[25, "parkname"] <- "Haleakala National Park"

# save general parks dataset
saveRDS(parks, "data/parks.rds")


### dataset with features to be used in random forest model ###

parkfeatures <- parks %>%
  unnest_wider(activities) %>% 
  select(parkname, latitude, longitude, name, fees)

# different activities and whether the park has them or not,
# might modify/add to this later on
parkfeatures <- parkfeatures %>%
  rowwise() %>%
  mutate(astronomy = ifelse("Astronomy" %in% unlist(name), 1, 0),
         stargazing = ifelse("Stargazing" %in% unlist(name), 1, 0), 
         biking = ifelse("Biking" %in% unlist(name), 1, 0), 
         boating = ifelse("Boating" %in% unlist(name), 1, 0), 
         camping = ifelse("Camping" %in% unlist(name), 1, 0), 
         climbing = ifelse("Climbing" %in% unlist(name), 1, 0), 
         fishing = ifelse("Fishing" %in% unlist(name), 1, 0), 
         hiking = ifelse("Hiking" %in% unlist(name), 1, 0), 
         paddling = ifelse("Paddling" %in% unlist(name), 1, 0),
         canoeing = ifelse("Canoeing" %in% unlist(name), 1, 0), 
         kayaking = ifelse("Kayaking" %in% unlist(name), 1, 0), 
         skiing = ifelse("Skiing" %in% unlist(name), 1, 0), 
         swimming = ifelse("Swimming" %in% unlist(name), 1, 0), 
         scenicdriving = ifelse("Scenic Driving" %in% unlist(name), 1, 0)) %>%
  select(-name)

parkfeatures <- parkfeatures %>%
  unnest_wider(fees) %>%
  select(-c(description, title))

# if a park costs money or not
parkfeatures <- parkfeatures %>%
  rowwise() %>%
  mutate(free = ifelse(length(unlist(cost)) == 1 & "0.00" %in% unlist(cost), 1, 0)) %>%
  select(-cost)
parkfeatures[27, "free"] <- 1

# the best season to visit each park - I just googled this info
# some listed multiple seasons but I only picked one per park for simplicity
parkfeatures$bestseason <- c("Fall", "Spring", "Fall", "Spring", "Winter",
                             "Summer", "Summer", "Spring", "Fall", "Fall",
                             "Fall", "Spring", "Summer", "Fall", "Spring",
                             "Winter", "Winter", "Summer", "Summer", "Spring",
                             "Summer", "Summer", "Summer", "Fall", "Winter",
                             "Spring", "Summer", "Spring", "Summer", "Fall",
                             "Summer", "Summer", "Summer", "Summer", "Summer",
                             "Summer", "Summer", "Spring", "Fall", "Spring",
                             "Summer", "Spring", "Spring", "Fall", "Spring",
                             "Fall", "Spring", "Summer", "Spring", "Summer",
                             "Spring")

# set new coordinates using openroute service api
parkfeatures <- parkfeatures %>% 
  rowwise() %>%
  mutate(query = URLencode(str_c("https://api.openrouteservice.org/geocode/search?api_key=5b3ce3597851110001cf6248ddae92a05a2c44bc9da60dcbccdfcbaa&text=",
                                 parkname)),
         longitude = fromJSON(query)$bbox[1],
         latitude = fromJSON(query)$bbox[2]) %>%
  select(-query)

# save this dataset
saveRDS(parkfeatures, "data/parkfeatures.rds")

