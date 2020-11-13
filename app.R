library(devtools)
library(remotes)
# remotes::install_github("GIScience/openrouteservice-r")
# devtools::install_github("nik01010/dashboardthemes")
library(dashboardthemes)
library(shinydashboard)
library(tidyverse)
library(jsonlite)
library(geosphere)
library(randomForest)
library(leaflet)
library(rgdal)
library(httr)
library(openrouteservice)
ors_api_key("5b3ce3597851110001cf6248ddae92a05a2c44bc9da60dcbccdfcbaa") #api key for openroute service api

# load api helper functions
source("parkinfo.R")
source("mapdirections.R")

# user interface
ui <- fluidPage(
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
                 icon = icon("music")),
        menuItem("National Park News", tabName = "articles", 
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
                         h5("Description"),
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
                         valueBox("Not sure which national park to visit first? Let us make a recommendation!", 
                        "We'll try to match you to a park that best suits your interests, even if it doesn't exactly match all of your preferences", 
                        icon = icon("tree"), color = "blue", width = 12)
                        ),
                box(width = 3, status = "primary",
                    textInput(inputId = "startloc",
                              label = "Starting from:",
                              placeholder = "e.g. 1234 Duke Drive, Durham NC"),
                    numericInput(inputId = "distance",
                                 label = "Maximum Travel Distance (mi):",
                                 value = 500,
                                 min = 0,
                                 max = 3000),
                    selectInput(inputId = "season",
                                label = "Travel Season:",
                                choices = c("Spring", "Summer", "Fall", "Winter")),
                    selectInput(inputId = "activities",
                                label = "Activities:",
                                choices = c("Astronomy", "Stargazing", "Biking", "Boating", 
                                            "Camping", "Climbing", "Fishing", "Hiking", "Paddling",
                                            "Canoeing", "Kayaking", "Skiing", "Swimming", "Scenic Driving"),
                                multiple = TRUE), # returns a character vector
                    checkboxInput(inputId = "fee",
                                  label = "Free (no entrance fee)"),
                    div(align = "right",
                        actionButton(inputId = "getrec", 
                                 label = strong("Find a Match"), 
                                 icon = icon("pagelines"))
                        )
                    ),
                # output park rec banner
                uiOutput("parkBox"),
                # tab box w/info about recommended park
                uiOutput("parkinfobox")
                ),
        
        # directions tab
        tabItem(tabName = "directions",
                box(width = 3, status = "primary",
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
                                            "Pinnacles National Park", "Rocky Mountain National Park", "Saguaro National Park", "Shenandoah National Park",
                                            "Theodore Roosevelt National Park", "Voyageurs National Park", "White Sands National Park", "Wind Cave National Park",
                                            "Yellowstone National Park", "Yosemite National Park", "Zion National Park")),
                    div(align = "right",
                        actionButton(inputId = "getdirections", 
                                     label = strong("Get Directions"), 
                                     icon = icon("compass"))
                        )
                    ),
                # output map
                leafletOutput("map")
                ),
        
        # playlist tab
        tabItem(tabName = "playlist"),
        
        # articles tab
        tabItem(tabName = "articles")
        )
      )
    )
  )

# server function
server <- function(input, output) {
  # get park recommendation
  recData <- eventReactive(input$getrec, {
    parkfinal <- calcDistance(input$startloc)
    get_parkrec(parkdata = parkfinal, maxdistance = input$distance, activities = input$activities,
                fee = as.numeric(input$fee), season = input$season)
  })
  # output park rec
  output$parkBox <- renderUI({
    park_name <- recData() %>%
      select(parkname) %>%
      pull()
    location <- recData() %>%
      select(state) %>%
      pull()
    infoBox("Our Recommendation", 
            park_name, 
            str_c("Location(s): ", location),
            icon = icon("map-pin"), color = "blue", width = 9
            )
    })
  # output info about recommended park in tabBox
  output$parkinfobox <- renderUI({
    tabBox(
      title = tagList(shiny::icon("info-circle"), "Info"),
      # The id lets us use input$tabset1 on the server to find the current tab
      id = "tabset1", width = 9, side = "right",
      tabPanel("Hours"),
      tabPanel("Weather"),
      tabPanel("General Info")
    )
  })
  
  # directions map
  leafletmap <- eventReactive(input$getdirections, {
    get_route(startpoint = input$startlocmap, park = input$parkdest)
  })
  output$map <- renderLeaflet({
    leafletmap()
  })
}

# run the application 
shinyApp(ui = ui, server = server)
