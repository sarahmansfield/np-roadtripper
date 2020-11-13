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


## change coordinates to be compatible with ORS API
parks[5, "longitude"] <- -80.37532; parks[5, "latitude"] <- 25.46426   #biscayne
parks[8, "longitude"] <- -109.57969; parks[8, "latitude"] <- 38.55615   #canyonlands
parks[11, "longitude"] <- -119.26659; parks[11, "latitude"] <- 34.24844   #channel islands
parks[15, "longitude"] <- -116.85238; parks[15, "latitude"] <- 36.45503   #death valley
parks[17, "longitude"] <- -82.44890; parks[17, "latitude"] <- 25.44055   #everglades
parks[22, "longitude"] <- -114.12662; parks[22, "latitude"] <- 39.01516   #great basin
parks[23, "longitude"] <- -83.79200; parks[23, "latitude"] <- 35.66323   #great smoky mountains
parks[24, "longitude"] <- -105.09273; parks[24, "latitude"] <- 31.74373   #guadalupe mountains
parks[25, "longitude"] <- -156.15515; parks[25, "latitude"] <- 20.72040   #haleakala
parks[26, "longitude"] <- -155.69925; parks[26, "latitude"] <- 19.17875   #hawaii volcanoes
parks[29, "longitude"] <- -88.6; parks[29, "latitude"] <- 47.1   #isle royale
parks[31, "longitude"] <- -150.67887; parks[31, "latitude"] <- 59.67858   #kenai fjords
parks[32, "longitude"] <- -162.60712; parks[32, "latitude"] <- 66.89165   #kobuk valley
parks[36, "longitude"] <- -122.04555; parks[36, "latitude"] <- 46.74387   #mount rainier
parks[38, "longitude"] <- -123.43291; parks[38, "latitude"] <- 48.09916   #olympic
parks[41, "longitude"] <- -105.58944; parks[41, "latitude"] <- 40.33114   #rocky mountain
parks[46, "longitude"] <- -93.46531; parks[46, "latitude"] <- 48.56177   #voyageurs
parks[47, "longitude"] <- -105.98274; parks[47, "latitude"] <- 32.86573   #white sands
parks[49, "longitude"] <- -110.74563; parks[49, "latitude"] <- 44.96537   #yellowstone
parks[51, "longitude"] <- -113.00340; parks[51, "latitude"] <- 37.18258   #zion

# remove inaccessible islands
parks <- parks %>%
  filter(!(parkname %in% c("Dry Tortugas National Park", "Virgin Islands National Park")))

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
parkfeatures[26, "free"] <- 1

# the best season to visit each park - I just googled this info
# some listed multiple seasons but I only picked one per park for simplicity
parkfeatures$bestseason <- c("Fall", "Spring", "Fall", "Spring", "Winter",
                             "Summer", "Summer", "Spring", "Fall", "Fall",
                             "Fall", "Spring", "Summer", "Fall", "Spring",
                             "Winter", "Summer", "Summer", "Spring",
                             "Summer", "Summer", "Summer", "Fall", "Winter",
                             "Spring", "Summer", "Spring", "Summer", "Fall",
                             "Summer", "Summer", "Summer", "Summer", "Summer",
                             "Summer", "Summer", "Spring", "Fall", "Spring",
                             "Summer", "Spring", "Spring", "Fall",
                             "Fall", "Spring", "Summer", "Spring", "Summer",
                             "Spring")

# save this dataset
saveRDS(parkfeatures, "data/parkfeatures.rds")

