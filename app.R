# packages
library(devtools)
# devtools::install_github("nik01010/dashboardthemes")
library(dashboardthemes)
library(shinydashboard)
library(tidyverse)
library(jsonlite)
library(geosphere)
library(randomForest)

# load api helper functions
source("parkinfo.R")

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
                        icon = icon("leaf"), color = "blue", width = 12)
                        ),
                box(width = 3, status = "primary",
                    textInput(inputId = "startloc",
                              label = "Starting from:",
                              placeholder = "e.g. 1234 Duke Drive, Durham NC"),
                    numericInput(inputId = "distance",
                                 label = "Maximum Travel Distance (mi):",
                                 value = 10,
                                 min = 0,
                                 max = 3700),
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
                # output park rec
                uiOutput("parkBox")
                ),
        
        # directions tab
        tabItem(tabName = "directions"),
        
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
}

# run the application 
shinyApp(ui = ui, server = server)
