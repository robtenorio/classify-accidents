library(dplyr)
library(leaflet)
library(leaflet.extras)
library(lubridate)
library(plotly)
library(purrr)
library(readr)
library(shiny)
library(shinydashboard)

twitter_sites <- readRDS("data/feb_may2018_clustered.rds") %>%
  dplyr::select(lat.wgs84, lon.wgs84, date, year, month, day, hour) %>%
  rename(lat = lat.wgs84, long = lon.wgs84)

# Define UI for application that draws dashboard
header <- dashboardHeader(disable = TRUE)
sidebar <- dashboardSidebar(disable = TRUE)
body <- dashboardBody(
  tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "style.css")),
  fluidRow(
           box(width = 9, height = 500, title = "Map of Tweeted Accidents",
               leafletOutput("map", height = 400)
               ),
           box(width = 3, height = 500, title = "Hour of Day",
               checkboxGroupInput("hours", label = NULL,
                                  choices = c(
                                    "00:00" = 0,
                                    "01:00" = 1,
                                    "02:00" = 2,
                                    "03:00" = 3,
                                    "04:00" = 4,
                                    "05:00" = 5,
                                    "06:00" = 6,
                                    "07:00" = 7,
                                    "08:00" = 8,
                                    "09:00" = 9,
                                    "10:00" = 10,
                                    "11:00" = 11,
                                    "12:00" = 12,
                                    "13:00" = 13,
                                    "14:00" = 14,
                                    "15:00" = 15,
                                    "16:00" = 16,
                                    "17:00" = 17,
                                    "18:00" = 18,
                                    "19:00" = 19,
                                    "20:00" = 20,
                                    "21:00" = 21,
                                    "22:00" = 22,
                                    "23:00" = 23,
                                    "24:00" = 24
                                  ),
                                  selected = c(7, 8, 9, 10)
               )
           )
    ),
  fluidRow(
    box(width = 12, height = 300, title = "Daily Accidents by Data Source",
        plotlyOutput("line", height = 250)
        )
    )
  )

ui <- dashboardPage(
  header,
  sidebar,
  body
    )

# Define server logic
server <- function(input, output) {
   
  output$map <- renderLeaflet({
    
    leaflet() %>% setView(lng = 36.8219, lat = -1.2921, zoom = 11) %>% 
      addTiles() %>% 
      addFullscreenControl(position = "topleft", pseudoFullscreen = TRUE)

  })
  
  observe({
    twitter_sites_geo <- twitter_sites %>%
      filter(!is.na(lat) & !is.na(long)) %>%
      filter(hour %in% input$hours)
    
    leafletProxy("map", data = twitter_sites_geo) %>% 
      clearGroup("markers") %>%
      addCircleMarkers(~long, ~lat, clusterOptions = markerClusterOptions(), group = "markers")
      #addCircles(~long, ~lat)
    
  })
  
  output$line <- renderPlotly({
    all_ts <- readRDS("data/all_ts.rds")
    
    plot_ly(data = all_ts, x = ~date) %>%
      add_lines(y = ~twitter, name = 'Twitter') %>%
      add_lines(y = ~matatu, name = 'Matatus') %>%
      add_lines(y = ~govt, name = 'Government') %>%
      layout(
        yaxis = list(title = "Number of Accidents"),
        xaxis = list(title = ""),
        margin = list(b = 100)
      )
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

