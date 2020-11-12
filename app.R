# packages
library(devtools)
devtools::install_github("nik01010/dashboardthemes")
library(dashboardthemes)
library(shinydashboard)

# load api helper functions
#source("api_wrappers.R")

# user interface
ui <- fluidPage(
  dashboardPage(
    dashboardHeader(title = "NP Roadtripper"),
    dashboardSidebar(
      sidebarMenu(
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
      shinyDashboardThemes(theme = "purple_gradient"),
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
        tabItem(tabName = "findpark"),
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

}

# run the application 
shinyApp(ui = ui, server = server)
