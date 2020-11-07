library(tidyverse)
library(jsonlite)

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
         weatherinfo = data.weatherInfo)

### dataset with info about features pertaining to each national park ###

parkfeatures <- parks %>%
  unnest_wider(activities) %>% 
  select(parkname, name, state, fees)

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

# add new observations for parks located in multiple states
parkfeatures <- rbind(parkfeatures, parkfeatures[15,], parkfeatures[23,],
                      parkfeatures[49,], parkfeatures[49,])
parkfeatures[15, "state"] <- "CA"
parkfeatures[52, "state"] <- "NV"
parkfeatures[23, "state"] <- "NC"
parkfeatures[53, "state"] <- "TN"
parkfeatures[49, "state"] <- "ID"
parkfeatures[54, "state"] <- "MT"
parkfeatures[55, "state"] <- "WY"


### test setup for random forest model ###

library(randomForest)

# random forest only takes up to 32 levels for categorical variables, 
# so filtering on some condition is necessary in order for the model to run
# most likely will use distance user is willing to travel as the filter condition,
# but for now we can use season as a condition just to demonstrate

# if user specifies season = Summer
summerparks <- parkfeatures %>%
  filter(bestseason == "Summer") %>%
  select(-bestseason)
summerparks$parkname <- factor(summerparks$parkname)

rf.mod <- randomForest(parkname~., data = summerparks, 
                         mtry = 9, importance = TRUE)

# new data based on user inputs
new_data <- tibble(state = "CO", astronomy = 1, stargazing = 1, 
                   biking = 0, boating = 0, camping = 1, climbing = 0, 
                   fishing = 0, hiking = 1, paddling = 0, canoeing = 0, 
                   kayaking = 0, skiing = 0, swimming = 0, scenicdriving = 1,
                   free = 0)
predict(rf.mod, newdata = new_data) # returns hot springs national park

## will probably remove the state predictor from this model
