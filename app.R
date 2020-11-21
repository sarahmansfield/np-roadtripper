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
library(DT)
library(spotifyr)
library(lubridate)
library(knitr)
library(httr)
library(remotes)
#remotes::install_github("GIScience/openrouteservice-r")
library(openrouteservice)
ors_api_key("5b3ce3597851110001cf6248ddae92a05a2c44bc9da60dcbccdfcbaa") #api key for openroute service api

# load api helper functions
source("parkinfo.R")
source("mapdirections.R")
source("playlist.R")
source("playlist_parks.R")


# user interface
ui <- fluidPage(
  title = "National Park Roadtripper",
  
  # change header font
  tags$head(
    tags$style(HTML("
      @import url('//fonts.googleapis.com/css?family=Lobster|Cabin:400,700');
      
      #apptitle {
      font-family: 'Lobster', cursive;
      font-weight: bold;
      font-size: 30px;
      text-align: center;
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
    dashboardHeader(#title = "NP Roadtripper"
                    titleWidth='100%',
                    title = shiny::span(
                      tags$img(src="https://www.travelyosemite.com/media/820617/adobestock_196063806_1000x500.jpg", width = "34%", align = "left"),
                      tags$img(src="https://media.deseretdigital.com/file/fdd8867843?type=jpeg&quality=55&c=15&a=4379240d", width = '34%', align = "center"),
                      tags$img(src="https://www.yellowstonepark.com/.image/t_share/MTUxMzk3NjQ1MjMzOTU2MDk1/teton-bison_andrecostantini_700.jpg", width = '32%', align = "right")
                   #   column(12, class="title-box", 
                   #          tags$h1(class="primary-title", style='margin-top:10px;', 'National Park Roadtripper!')
                    
                 #  https://www.usnews.com/dims4/USNEWS/c780ac6/2147483647/resize/1200x%3E/quality/85/?url=http%3A%2F%2Fmedia.beam.usnews.com%2F02%2F5a%2F9f703dee4990843b40902de4c617%2Fyosemite2-getty-loic-lagarde.jpg
                    
                    )),
    dashboardSidebar(
      sidebarMenu(id = "sidebar",
        textOutput("apptitle"),
        br(),
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
                 icon = icon("list-ul"))
      )
    ),
    dashboardBody(
      tags$style(type="text/css", "
/*    Move everything below the header */
    .content-wrapper {
        margin-top: 50px;
    }
    .content {
        padding-top: 60px;
    }
/*    Format the title/subtitle text */
    .title-box {
        position: absolute;
        text-align: center;
        top: 50%;
        left: 50%;
        transform:translate(-50%, -50%);
    }
    @media (max-width: 590px) {
        .title-box {
            position: absolute;
            text-align: center;
            top: 10%;
            left: 10%;
            transform:translate(-5%, -5%);
        }
    }
    @media (max-width: 767px) {
        .primary-title {
            font-size: 1.1em;
        }
        .primary-subtitle {
            font-size: 1em;
        }
    }
/*    Make the image taller */
    .main-header .logo {
        height: 145px;
    }
/*    Override the default media-specific settings */
    @media (max-width: 5000px) {
        .main-header {
            padding: 0 0;
            position: relative;
        }
        .main-header .logo,
        .main-header .navbar {
            width: 100%;
            float: none;
        }
        .main-header .navbar {
            margin: 0;
        }
        .main-header .navbar-custom-menu {
            float: right;
        }
    }
/*    Move the sidebar down */
    .main-sidebar {
        position: absolute;
    }
    .left-side, .main-sidebar {
        padding-top: 205px;
    }"
      ),
      ### apply theme
      shinyDashboardThemes(theme = "blue_gradient"),
      tabItems(
        # user guide tab
        tabItem(tabName = "userguide",
                fluidRow(
                  column(2, align = "left", 
                         tags$img(src = "treefinal.png", width = "50%", align = "right", opacity = "0.4")
                  ),
                  column(8, align = "center",
                         div(h2(("Welcome to the National Park Roadtrip Planner!")), style = "color:#1f78b4 ; border-bottom:3px solid #1f78b4")), 
                  column(2, align = "right", 
                         tags$img(src = "treefinal.png", width = "50%", align = "left"))),
                  fluidRow(
                    column(12, align = "center", 
                           h4("Imagine you need a break. Life can be hard, so you decide to take a vacation. But 
                         where should you go? International travel can be stressful, expensive, and difficult. Perhaps
                         you're even scared of planes. What should you do? Well, we have the answer for you! Take a 
                         road trip to one of 50 of America's beautiful national parks!
                         Planning road trips can be time consuming, but we can help make it easier -
                            Welcome to the National Park Roadtripper app! "),
                           )),
                  fluidRow(column(12, align = "center", 
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
                         )),
        
        # find a park tab
        tabItem(tabName = "findpark",
                fluidRow(width = 12, align = "center",
                    
                         valueBox(tags$p("Not sure which national park to visit first?", style = "font-size: 70%;"), 
                        "Let us make a recommendation! We'll try to match you to a park you might like, even if it doesn't exactly match all of your preferences", 
                        icon = icon("tree"), color = "teal", width = 12)
                        ),
                fluidRow(
                  column(width = 4,
                         box(width = NULL, status = "primary",
                             textInput(inputId = "startloc",
                                       label = "Starting from:",
                                       placeholder = "e.g. Duke University"),
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
                  column(width = 8,
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
                              placeholder = "e.g. Duke University"),
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
                
                # output map
                leafletOutput("map")
                ),
        
        # playlist parks
        tabItem(tabName = "playlist_park", 
                fluidRow(width = 12, align = "center",
                         
                         valueBox(tags$p("Need the perfect playlist for your adventure?", style = "font-size: 70%;"), 
                                  "Choose a National Park to see a curated Spotify playlist courtesy of Parks Project!", 
                                  icon = icon("guitar"), color = "teal", width = 12)
                ),
                fluidRow(
                  column(width = 4, align = "center",
                         box(width = NULL, status = "primary", 
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
                                         ),
                             htmlOutput("picture"), 
                             br(),
                             tags$img(src = "MyImage.jpg", width = "30%", align = "left")
                             )
                         ),
                  column(width = 8, align = "center",
                         htmlOutput("play"))
                    )
                ),
        
        # playlist genre
        tabItem(tabName = "playlist_genre",
                fluidRow(width = 12, align = "center",
                         
                         valueBox(tags$p("Need the perfect playlist for your adventure?", style = "font-size: 70%;"),
                                  "Choose your favorite music genre for our recommendation!", 
                                  icon = icon("guitar"), color = "teal", width = 12)
                ),
                fluidRow(
                  column(width = 4, align = "center", 
                         box(width = NULL, status = "primary", 
                             selectInput(inputId = "genre", 
                                         label = "Choose Genre", 
                                         choices = c("Pop" = "pop", 
                                                     "Rock" = "rock", 
                                                     "Party" = "party", 
                                                     "Chill" = "chill", 
                                                     "Hip hop" = "hiphop", 
                                                     "EDM" = "edm_dance", 
                                                     "Jazz" = "jazz", 
                                                     "R&B" = "rnb", 
                                                     "Country" = "country", 
                                                     "Latin" = "latin",
                                                     "Holidays" = "holidays", 
                                                     "Indie / Alternative" = "indie_alt")), 
                             htmlOutput("picture_genre"),
                             br(),
                             tags$img(src = "MyImage.jpg", width = "30%", align = "left")
                             )
                         ), 
                  column(width = 8, align = "center", 
                         htmlOutput("playlist_genre"))
                )
                ),
        
        # Packing List tab
        tabItem(tabName = "packing", 
                fluidRow(width = 12, align = "center",
                         
                         valueBox(tags$p("Camping Checklist for the Forgetful Traveller", style = "font-size: 70%;"), 
                                  "Never forget the camping essentials using our checklist tool!", 
                                  icon = icon("tree"), color = "teal", width = 12)
                ),
                fluidRow(
                  box(width = 4, status = "primary",
                      selectInput(inputId = "packseason",
                                  label = "What time of year are you camping?",
                                  choices = c("April - August", "September - March (I'm brave!)")),
                      selectInput(inputId = "cooking",
                                  label = "Do you plan on cooking while you camp?:",
                                  choices = c("Yes!", "No!")),
                      selectInput(inputId = "glamping",
                                  label = "Let's Be Real: Is this a Glamping Trip or Not?",
                                  choices = c("No we want to do real camping!","Yeah, we are glamping")),
                      selectInput(inputId = "bears",
                                  label = "Are you worried about bears?",
                                  choices = c("Obviously", "No (I really need to self reflect)")),
                      div(align = "right",
                          actionButton(inputId = "getList", 
                                       label = strong("Get Your Packing List"), 
                                       icon = icon("box")
                          )
                      )
                  ),
                  
                  mainPanel(
                    column(
                      width = 12,
                      DT::dataTableOutput(outputId = "packlist")
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
  # sidebar title
  output$apptitle <- renderText("NP Roadtripper")
  
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
        dplyr::rename(`Activities Offered` = name)
    }
  })
  # table of hours
  output$hours_tbl <- renderTable({
    if (!is.null(recData())) {
      tbl <- recData()$hours[[1]]$standardHours[1,] %>%
        dplyr::rename(Monday = monday,
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
        dplyr::rename(Type = title, 
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
    url <- str_c("https://open.spotify.com/embed/playlist/", input$parkdest_playlist)
    HTML(paste0('<iframe src="', url,'" width="700" height="800" frameborder="0" allowtransparency="true" allow="encrypted-media"></iframe>'))
  })
  
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
  
  # Get playlist_genre ID
  playlist_id_genre <- eventReactive(input$genre, {
    get_category_playlists(category_id = input$genre, limit = 50) %>%
      sample_n(1) %>%
      select(id) %>%
      unlist()
  })

  
  # create picture of album art
  picture_genre <- reactive({
    get_playlist_cover_image(
      playlist_id_genre()) %>%
      select(url) %>%
      mutate(Image = str_c("<img src='", url, "' height = '300'></img"))
  })
  
  output$picture_genre <- renderText(picture_genre()$Image)
  
  # output playlist for specified genre
  output$playlist_genre <- renderUI({
    url <- str_c("https://open.spotify.com/embed/playlist/", playlist_id_genre())
    HTML(paste0('<iframe src="', url,'" width="700" height="800" frameborder="0" allowtransparency="true" allow="encrypted-media"></iframe>'))
  })
  
  #############################################################################
  # Camping List
  pack_data <- eventReactive(input$getList, {
    full_table =  read_csv("data/packingitems.csv")
    always_on = full_table[1:15,]
    
    if (input$packseason == "September - March (I'm brave)!"){
      season_additional = full_table[20:22,]
    } else {
      season_additional = full_table[18:19,]
    }
    
    if (input$cooking == "Yes!") {
      cooking_additional = full_table[23:27,]
    } else{
      cooking_additional = NULL
    }
    
    if (input$glamping == "Ya we are glamping") {
      glamp_additional = full_table[28:33,]
    } else{
      glamp_additional = NULL
    }
    
    if (input$bears == "Obviously") {
      bear_additional = full_table[16:17,]
    } else{
      bear_additional = NULL
    }
    
    tibble(rbind(always_on, season_additional, 
                 glamp_additional, bear_additional)) %>%  mutate(
                   Buy = stringr::str_c("<a href='", Buy, "'>", Buy, "</a>")
                 )

  })
  
  output$packlist <- DT::renderDataTable({
    DT::datatable(pack_data(),
                  escape = F, rownames = F,
                  # caption = htmltools::tags$caption(
                  #   style = "caption-side: top; text-align: center; 
                  #   color:black; font-size:150% ;",
                  #   "Your Packing List!"
                  # )
    )
  })
  
}

# run the application 
shinyApp(ui = ui, server = server)


