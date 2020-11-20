library(shinydashboard)
library(dashboardthemes)
library(shinyBS)
library(shinyWidgets)
library(shinyalert)
library(tidyverse)
library(purrr)
library(jsonlite)
library(cowplot)
library(magick)
library(geosphere)
library(randomForest)
library(leaflet)
library(remotes)
remotes::install_github("GIScience/openrouteservice-r")
library(openrouteservice)
ors_api_key("5b3ce3597851110001cf6248ddae92a05a2c44bc9da60dcbccdfcbaa") #api key for openroute service api

# load api helper functions
source("parkinfo.R")
source("mapdirections.R")
source("playlist.R")
source("playlist_parks.R")

# user interface
ui <- fluidPage(
  # change header font
  tags$head(
    tags$style(HTML("
      @import url('//fonts.googleapis.com/css?family=Lobster|Cabin:400,700');
      
      .main-header .logo {
      font-family: 'Lobster', cursive;
      font-weight: bold;
      font-size: 24px;
      }
      
    "))
  ),
  useShinyalert(),
  
  # directions pop up window
  bsModal(id = "directModal", 
          title = "Directions", 
          trigger = "getsteps", 
          size = "large",
          tableOutput(outputId = "steps_tbl")
  ),
  
  dashboardPage(
    dashboardHeader(title = "NP Roadtripper"),
    dashboardSidebar(
      sidebarMenu(id = "sidebar",
        menuItem("User Guide", tabName = "userguide", 
                 icon = icon("book-open")),
        menuItem("Find a Park", tabName = "findpark", 
                 icon = icon("tree")), 
        menuItem("Plan Your Trip", tabName = "directions", 
                 icon = icon("map-marked-alt")),
        menuItem("Roadtrip Playlist", tabName = "playlist", 
                 icon = icon("music"), 
                 menuSubItem("Playlist by Park", 
                             tabName = "playlist_park"), 
                 menuSubItem("Playlist by Genre",
                             tabName = "playlist_genre")),
        menuItem("Camping Packing List", tabName = "packing", 
                 icon = icon("newspaper")),
        menuItem("Source Code", icon = icon("file-code-o"), 
                 href = "https://github.com/sta523-fa20/project-same")
      )
    ),
    dashboardBody(
      ### apply theme
      shinyDashboardThemes(theme = "blue_gradient"),
      tabItems(
        # user guide tab
        tabItem(tabName = "userguide",
                fluidRow(
                  column(12, align = "center",
                         h2(strong("Welcome to the National Park Roadtrip Planner!")),
                         br(),
                         h4("Imagine you need a break. Life can be hard, so you decide to take a vacation. But 
                         where should you go? International travel can be stressful, expensive, and difficult. Perhaps
                         you're even scared of planes. What should you do? Well, we have the answer for you! Take a 
                         road trip to one of 50 of America's beautiful national parks!
                         Planning road trips can be time consuming, but we can help make it easier -
                            Welcome to the National Park Roadtripper app! "),
                         tags$div(
                           "Created using the ",
                           tags$a(href="https://www.nps.gov/subjects/developer/index.htm", "NPS API,"),
                           tags$a(href="https://openrouteservice.org/", "Openroute Service API,"),
                           "and the ",
                           tags$a(href="https://developer.spotify.com/documentation/web-api/", "Spotify Web API")
                         ),
                         hr(),
                         h3(strong("How to use the app:"))
                         
                         )
                  )
                ),
        
        # find a park tab
        tabItem(tabName = "findpark",
                fluidRow(width = 12, align = "center",
                         valueBox("Not sure which national park to visit first?", 
                        "Let us make a recommendation! We'll try to match you to a park you might like, even if it doesn't exactly match all of your preferences", 
                        icon = icon("tree"), color = "teal", width = 12)
                        ),
                fluidRow(
                  column(width = 3,
                         box(width = NULL, status = "primary",
                             textInput(inputId = "startloc",
                                       label = "Starting from:",
                                       placeholder = "e.g. 1234 Duke Drive, Durham NC"),
                             numericInput(inputId = "distance",
                                          label = "Maximum Travel Distance (mi):",
                                          value = 500,
                                          min = 0),
                             selectInput(inputId = "season",
                                         label = "Travel Season:",
                                         choices = c("Spring", "Summer", "Fall", "Winter")),
                             selectInput(inputId = "activities",
                                         label = "Activities:",
                                         choices = c("Astronomy", "Stargazing", "Biking", "Boating", 
                                                     "Camping", "Climbing", "Fishing", "Hiking", "Paddling",
                                                     "Canoeing", "Kayaking", "Skiing", "Swimming", "Scenic Driving"),
                                         multiple = TRUE), # returns a character vector
                             prettyCheckbox(inputId = "fee", 
                                            label = "Free (no entrance fee)",
                                            shape = "round", bigger = TRUE, status = "info",
                                            icon = icon("check"), animation = "jelly"
                                            ),
                             div(align = "right",
                                 actionButton(inputId = "getrec", 
                                              label = strong("Find a Match"), 
                                              icon = icon("pagelines")
                                              )
                                 )
                             )
                         ),
                  column(width = 9,
                         # output park rec banner
                         uiOutput("parkBox"),
                         # tab box w/info about recommended park
                         uiOutput("parkinfobox")
                         )
                  )
                ),
        
        # directions tab
        tabItem(tabName = "directions",
                box(width = 4, status = "primary",
                    textInput(inputId = "startlocmap",
                              label = "Starting from:",
                              placeholder = "e.g. 1234 Duke Drive, Durham NC"),
                    selectInput(inputId = "parkdest",
                                label = "Destination:",
                                choices = c("Acadia National Park", "Arches National Park", "Badlands National Park", "Big Bend National Park",
                                            "Biscayne National Park", "Black Canyon Of The Gunnison National Park", "Bryce Canyon National Park", 
                                            "Canyonlands National Park", "Capitol Reef National Park", "Carlsbad Caverns National Park",
                                            "Channel Islands National Park", "Congaree National Park", "Crater Lake National Park", 
                                            "Cuyahoga Valley National Park", "Death Valley National Park", "Everglades National Park",
                                            "Gateway Arch National Park", "Glacier National Park", "Grand Canyon National Park", "Grand Teton National Park",
                                            "Great Basin National Park", "Great Smoky Mountains National Park", "Guadalupe Mountains National Park",
                                            "Haleakala National Park", "Hawai'i Volcanoes National Park", "Hot Springs National Park",
                                            "Indiana Dunes National Park", "Isle Royale National Park", "Joshua Tree National Park", "Kenai Fjords National Park",
                                            "Kobuk Valley National Park", "Lassen Volcanic National Park", "Mammoth Cave National Park", "Mesa Verde National Park",
                                            "Mount Rainier National Park", "North Cascades National Park", "Olympic National Park", "Petrified Forest National Park",
                                            "Pinnacles National Park", "Rocky Mountain National Park", "Saguaro National Park", "Sequoia & Kings Canyon National Parks", 
                                            "Shenandoah National Park", "Theodore Roosevelt National Park", "Voyageurs National Park", "White Sands National Park", 
                                            "Wind Cave National Park", "Yellowstone National Park", "Yosemite National Park", "Zion National Park")),
                    div(style = "display:inline-block", width = 6,
                        actionButton(inputId = "getdirections", 
                                     label = strong("Map Your Route"),
                                     icon = icon("compass")
                                     )
                        ),
                    div(style = "display:inline-block",
                        conditionalPanel(condition = "input.getdirections > 0",
                                         # action button to get directions
                                         actionButton(inputId = "getsteps", 
                                                      label = strong("Directions"), 
                                                      icon = icon("route")
                                                      )
                                         )
                        )
                    ),
                
                # valuebox showing total distance
                uiOutput("distanceBox"),
                # valuebox showing total duration
                uiOutput("durationBox"),
                # another valuebox showing weather maybe?
                
                # output map
                leafletOutput("map")
                ),
        
        # playlist parks
        tabItem(tabName = "playlist_park", 
                fluidRow(
                box(width = 8, status = "primary", 
                    selectInput(inputId = "parkdest_playlist", 
                                label = "Choose Park Destination:",
                                choices = c("Acadia National Park" = "117MQyf7iOjLaUVN7zcJw6", 
                                            "Arches National Park" = "0kGeNA9vutRnoispZLvWOA", 
                                            "Channel Islands National Park" = "5bpuJAPKK6SXG1Helc1TsB", 
                                            "Glacier National Park" = "3WUcsMCdeFJgYEinv2MiYS", 
                                            "Grand Teton National Park" = "51KaBvMKCEmlc1bwPd0Amb",
                                            "Great Smoky Mountains National Park" = "70g2uez7L1UavQ6jCuV5Ps", 
                                            "Joshua Tree National Park" = "5LrXMXO5HSsVLVrqU4PouM", 
                                            "Mount Rainier National Park" = "1tiTFQFTMLuS1GOR2gMxK4", 
                                            "Olympic National Park" = "7pv5aJWeY3jYBmmFx5uOXz", 
                                            "Rocky Mountain National Park" = "44cvJuJMnUUyRcP519QzXs", 
                                            "Sequoia and Kings Canyon National Park" = "6lIqG5vA4WEuqYjelgn8iV",
                                            "Shenandoah National Park" = "2TDsIDS7fHYNSOlkaF16Dh",
                                            "Yellowstone National Park" = "4X43PiVJL1cGwxYnioeyHU", 
                                            "Yosemite National Park" = "4Te6Eha65DlRTwfO5O8iJD", 
                                            "Zion National Park" = "5HIMOLC7zwxmy2C3NJJcXc")
                    ))
                    ), 
                fluidRow(
                  box(width = 8,htmlOutput("picture"))),
                fluidRow(
                  box(htmlOutput("play")))

                ),
        
        # playlist genre
        tabItem(tabName = "playlist_genre",
                box(width = 3, status = "primary", 
                    selectInput(inputId = "genre", 
                                label = "Choose Genres (Up to 5)",
                                choices = c("acoustic", "alternative", "chill",
                                            "classical", "dance", "edm", "funk", 
                                            "grunge", "hip-hop", "holidays", 
                                            "indie", "jazz", "kids", "k-pop", 
                                            "pop", "punk", "r-n-b", "rock", 
                                            "soul", "world-music"), 
                                multiple = TRUE)
                )
                ),
        
        # Packing List tab
        tabItem(tabName = "packing", 
                box(width = 4, status = "primary",
                    selectInput(inputId = "season",
                              label = "What time of year are you camping?",
                              choices = c("Winter!", "Spring", "Summer", "Autumn")),
                    selectInput(inputId = "cooking",
                                label = "Do you plan on cooking while you camp?:",
                                choices = c("Yes!", "No!")),
                    div(style = "display:inline-block", width = 6,
                        actionButton(inputId = "getList", 
                                     label = strong("Get Your Camping Packing List")
                        )
                    ),
                    div(style = "display:inline-block",
                        conditionalPanel(condition = "input.getdirections > 0",
                                         # action button to get directions
                                         actionButton(inputId = "getsteps", 
                                                      label = strong("Directions"), 
                                                      icon = icon("route")
                                         )
                        )
                    )
                )
              )
        )
      )
    )
  )

# server function
server <- function(input, output) {
  # get park recommendation
  recData <- eventReactive(input$getrec, {
    # Create 0-row data frame which will be used to store data
    dat <- data.frame(x = numeric(0), y = numeric(0))
    
    withProgress(message = 'Finding a park...', value = 0, {
      # Number of times we'll go through the loop
      n <- 10
      
      for (i in 1:n) {
        # Each time through the loop, add another row of data. This is
        # a stand-in for a long-running computation.
        dat <- rbind(dat, data.frame(x = rnorm(1), y = rnorm(1)))
        
        # Increment the progress bar
        incProgress(1/n)
        
        # Pause for 0.1 seconds to simulate a long computation.
        Sys.sleep(0.1)
      }
    })
    
    parkfinal <- calcDistance(input$startloc)
    rec <- get_parkrec(parkdata = parkfinal, maxdistance = input$distance, activities = input$activities,
                fee = as.numeric(input$fee), season = input$season)
    if (is.null(rec)) {
      shinyalert(title = "ERROR",
                 text  = "There are no national parks within the specified distance from your starting point. Please expand your search.",
                 type  = "error")
    }
    rec
  })
  # output park rec
  output$parkBox <- renderUI({
    if (!is.null(recData())) {
      park_name <- recData()$parkname
      location <- recData()$state
      url <- recData()$url
      
      infoBox("Our Recommendation", 
              park_name, 
              str_c("Location(s): ", location),
              icon = icon("map-pin"), color = "aqua", width = 12,
              href = url
      )
    }
    })
  # park image1
  output$image1 <- renderPlot({
    if (!is.null(recData())) {
      imageurl <- recData()$images[[1]]$url[1]
      ggdraw() + draw_image(imageurl)
    }
  })
  # park image2
  output$image2 <- renderPlot({
    if (!is.null(recData())) {
      if (nrow(recData()$images[[1]]) >= 2) {
        imageurl <- recData()$images[[1]]$url[2]
        ggdraw() + draw_image(imageurl)
      }
    }
  })
  # park image3
  output$image3 <- renderPlot({
    if (!is.null(recData())) {
      if (nrow(recData()$images[[1]]) >= 3) {
        imageurl <- recData()$images[[1]]$url[3]
        ggdraw() + draw_image(imageurl)
      }
    }
  })
  # park image4
  output$image4 <- renderPlot({
    if (!is.null(recData())) {
      if (nrow(recData()$images[[1]]) >= 4) {
        imageurl <- recData()$images[[1]]$url[4]
        ggdraw() + draw_image(imageurl)
      }
    }
  })
  # table of activities
  output$act_tbl <- renderTable({
    if (!is.null(recData())) {
      recData()$activities[[1]] %>% 
        select(name) %>% 
        rename(`Activities Offered` = name)
    }
  })
  # table of hours
  output$hours_tbl <- renderTable({
    if (!is.null(recData())) {
      tbl <- recData()$hours[[1]]$standardHours[1,] %>%
        rename(Monday = monday,
               Tuesday = tuesday,
               Wednesday = wednesday,
               Thursday = thursday,
               Friday = friday,
               Saturday = saturday,
               Sunday = sunday)
      col_order <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")
      tbl[, col_order] %>%
        pivot_longer(cols = Monday:Sunday, names_to = "Day", values_to = "Hours")
    }
  })
  # table of fees
  output$fees_tbl <- renderTable({
    if (!is.null(recData())) {
      tbl <- recData()$fees[[1]] %>%
        rename(Type = title, 
               Description = description, 
               Cost = cost)
      col_order <- c("Type", "Cost", "Description")
      tbl[, col_order]
    }
  })
# --------------------------------------------------------------------------------------------------  
  # Mady working
  data <- eventReactive(input$parkdest_playlist, {
    get_parks_project_songs(
      playlistID = input$parkdest_playlist) }
  )
  
  # Mady working 2
  picture <- eventReactive(input$parkdest_playlist, {
    get_playlist_cover_image(
      input$parkdest_playlist) %>%
      select(url) %>%
      mutate(Image = str_c("<img src='", url, "' height = '300'></img"))}
  )
  
  output$picture<-renderText(picture()$Image)
  
  output$play <- renderUI({
    tags$iframe(src='https://open.spotify.com/embed/playlist/6lIqG5vA4WEuqYjelgn8iV', width='300', height='380', frameborder='0', allowtransparency='true', allow='encrypted-media')
    })
  
    
    ### "<iframe src='https://open.spotify.com/embed/playlist/6lIqG5vA4WEuqYjelgn8iV' width='300' height='380' frameborder='0' allowtransparency='true' allow='encrypted-media'></iframe>")
  
# ----------------------------------------------------------------------------------------------------
  # output info about recommended park in tabBox
  output$parkinfobox <- renderUI({
    if (!is.null(recData())) {
      parkdesc <- recData()$parkdesc #park description
      weatherdesc <- recData()$weatherinfo #weather description
      hoursdesc <- recData()$hours[[1]]$description[1] #hours description
      
      tabBox(
        title = tagList(shiny::icon("info-circle"), "Info"),
        # The id lets us use input$tabset1 on the server to find the current tab
        id = "tabset1", width = 12, side = "right",
        tabPanel(title = tagList(shiny::icon("info"), "General Info"),
                 sidebarLayout(
                   sidebarPanel(width = 7,
                                plotOutput("image1")
                   ),
                   mainPanel(width = 5,
                             br(),
                             parkdesc,
                             br(), br(),
                             weatherdesc
                   )
                 )
        ),
        tabPanel(title = tagList(shiny::icon("hiking"), "Activities"),
                 sidebarLayout(
                   sidebarPanel(width = 7,
                                plotOutput("image2")
                   ),
                   mainPanel(width = 5, 
                             fluidRow(br(),
                                      column(width = 12, align = "center",
                                             tableOutput("act_tbl"))
                             )
                   )
                 )
        ),
        tabPanel(title = tagList(shiny::icon("clock"), "Hours"),
                 sidebarLayout(
                   sidebarPanel(width = 7,
                                plotOutput("image3")
                   ),
                   mainPanel(width = 5, 
                             fluidRow(br(),
                                      hoursdesc,
                                      br(), br(),
                                      column(width = 12, align = "center",
                                             tableOutput("hours_tbl"))
                             )
                   )
                 )
        ),
        tabPanel(title = tagList(shiny::icon("dollar-sign"), "Fees"),
                 sidebarLayout(
                   sidebarPanel(width = 7,
                                plotOutput("image4")
                   ),
                   mainPanel(width = 5, 
                             fluidRow(br(),
                                      column(width = 12, align = "center",
                                             tableOutput("fees_tbl"))
                             )
                   )
                 )
        )
      )
    }
  })
  
  # directions geoJSON object
  directobj <- eventReactive(input$getdirections, {
    # Create 0-row data frame which will be used to store data
    dat <- data.frame(x = numeric(0), y = numeric(0))
    
    withProgress(message = 'Getting directions...', value = 0, {
      # Number of times we'll go through the loop
      n <- 10
      
      for (i in 1:n) {
        # Each time through the loop, add another row of data. This is
        # a stand-in for a long-running computation.
        dat <- rbind(dat, data.frame(x = rnorm(1), y = rnorm(1)))
        
        # Increment the progress bar
        incProgress(1/n)
        
        # Pause for 0.1 seconds to simulate a long computation.
        Sys.sleep(0.1)
      }
    })
    
    route <- get_route(startpoint = input$startlocmap, park = input$parkdest)
    if (is.null(route)) {
      shinyalert(title = "ERROR",
                 text  = "This app only supports route distances up to 3000 miles. Please select another park closer to your starting location.",
                 type  = "error")
    }
    route
  })
  
  # total distance of trip in miles
  totaldist <- renderText({
    if (!is.null(directobj())) {
      round(directobj()$features[[1]]$properties$segments[[1]]$distance / 1609, 2)
    }
  })
  # total duration of trip in hours
  totaldur <- renderText({
    if (!is.null(directobj())) {
      round(directobj()$features[[1]]$properties$segments[[1]]$duration / 3600, 2)
    }
  })
  
  output$distanceBox <- renderUI({
    if (!is.null(directobj())) {
      valueBox(
        paste0(totaldist(), " miles"), "Trip Distance", icon = icon("car"),
        color = "aqua", width = 4
      )
    }
  })
  output$durationBox <- renderUI({
    if (!is.null(directobj())) {
      valueBox(
        paste0(totaldur(), " hours"), "Trip Duration", icon = icon("clock"),
        color = "teal", width = 4
      )
    }
  })
  
  # table of step by step route instructions
  output$steps_tbl <- renderTable({
    if (!is.null(directobj())) {
      get_steps(directions = directobj())
    }
  })
  
  # map with route
  output$map <- renderLeaflet({
    if (!is.null(directobj())) {
      home_long <- directobj()[["metadata"]][["query"]][["coordinates"]][1,1]
      home_lat <- directobj()[["metadata"]][["query"]][["coordinates"]][1,2]
      park_long <- directobj()[["metadata"]][["query"]][["coordinates"]][2,1]
      park_lat <- directobj()[["metadata"]][["query"]][["coordinates"]][2,2]
      
      # Create 0-row data frame which will be used to store data
      dat <- data.frame(x = numeric(0), y = numeric(0))
      
      withProgress(message = 'Generating map...', value = 0, {
        # Number of times we'll go through the loop
        n <- 10
        
        for (i in 1:n) {
          # Each time through the loop, add another row of data. This is
          # a stand-in for a long-running computation.
          dat <- rbind(dat, data.frame(x = rnorm(1), y = rnorm(1)))
          
          # Increment the progress bar
          incProgress(1/n)
          
          # Pause for 0.1 seconds to simulate a long computation.
          Sys.sleep(0.1)
        }
      })
      
      leaflet() %>%
        addTiles() %>%
        addGeoJSON(directobj(), fill = FALSE) %>%
        fitBBox(directobj()$bbox) %>%
        addAwesomeMarkers(lng = home_long, lat = home_lat, icon = awesomeIcons("home")) %>%
        addMarkers(lng = park_long, lat = park_lat)
    }
  })
}

# run the application 
shinyApp(ui = ui, server = server)
