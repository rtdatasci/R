# Alien sighting
# The National UFO Reporting Center (NUFORC) has collected sightings data throughout the last century. This app is going to allow users to select a U.S. state and a time period in which the sightings occurred.
#Use leaflet to build map
#The plot should show the number sighted, by shape, for the selected state and time period.
#The table should show, for the selected state and time period, the number sighted, plus the average, median, minimum, and maximum duration (duration_sec) of the sightings

library(shiny)
library(dplyr)
library(ggplot2)
library(DT)
library(leaflet)

## global 
# Data downloaded from: https://www.kaggle.com/datasets/NUFORC/ufo-sightings
usa_ufo_sightings <- read.csv("~/rtdatasci_github/R/alienz/scrubbed.csv") # for local test
#usa_ufo_sightings <- read.csv("scrubbed.csv") # path in project directory for publishing

## Pre-processing and cleanup
# remove rows with no state data (to keep US only data for this case)
usa_ufo_sightings <- usa_ufo_sightings %>% 
  filter(!is.na(state)) %>% 
  filter(state != "") %>% 
  mutate(state = toupper(state)) %>% 
  # format colname for ease of use
  dplyr::rename(duration_sec = 'duration..seconds.') %>% 
  # convert to numeric for metrics
  mutate(duration_sec = as.numeric(duration_sec),
         latitude = as.numeric(latitude),
         longitude = as.numeric(longitude)) %>% 
  # Remove rows with non-numeric duration_sec and latitude
  filter(is.numeric(duration_sec)) %>% 
  filter(is.numeric(latitude)) %>% 
  # Remove rows with missing latitude longitude
  filter(!is.na(latitude)) %>% 
  filter(!is.na(longitude)) 
  
# create date only column
usa_ufo_sightings <- usa_ufo_sightings %>% 
  mutate(date_sighted = str_extract(datetime, "^[^\\s]+")) %>% 
  # format date
  mutate(date_sighted = as.character(as.Date(date_sighted, "%m/%d/%Y")))

# check data min and max dates
min(usa_ufo_sightings$date_sighted)
max(usa_ufo_sightings$date_sighted)

ui <- fluidPage(
  titlePanel("UFO Sightings"),
  sidebarPanel(
    selectInput("state", "Choose a U.S. state:", choices = unique(usa_ufo_sightings$state)),
    dateRangeInput("dates", "Choose a date range:",
                   start = "1920-01-01",
                   end = "1950-01-01"
    )
  ),
  # MODIFY CODE BELOW: Create a tab layout for the dashboard
  mainPanel(
    tabsetPanel(
      tabPanel("MAP", leafletOutput("map")),
      tabPanel('SHAPES',plotOutput("shapes")),
      tabPanel('DURATION', tableOutput("duration_table")
      )
    )
  )
)

server <- function(input, output) {
  output$map <- renderLeaflet({
    usa_ufo_sightings %>%
          filter(
            state == input$state,
            date_sighted >= input$dates[1],
            date_sighted <= input$dates[2]
          ) %>%
      leaflet() %>%
      addTiles() %>%
      addMarkers(lng =  ~longitude,
                 lat =  ~latitude,
                 popup = ~as.character(usa_ufo_sightings$shape))
  })
  
  output$shapes <- renderPlot({
    usa_ufo_sightings %>%
      filter(
        state == input$state,
        date_sighted >= input$dates[1],
        date_sighted <= input$dates[2]
      ) %>%
      ggplot(aes(shape)) +
      geom_bar() +
      labs(
        x = "Shape",
        y = "# Sighted"
      )
  })
  
  output$duration_table <- renderTable({
    usa_ufo_sightings %>%
      filter(
        state == input$state,
        date_sighted >= input$dates[1],
        date_sighted <= input$dates[2]
      ) %>%
      group_by(shape) %>%
      summarize(
        nb_sighted = n(),
        avg_duration_min = mean(duration_sec) / 60,
        median_duration_min = median(duration_sec) / 60,
        min_duration_min = min(duration_sec) / 60,
        max_duration_min = max(duration_sec) / 60
      )
  })
}

shinyApp(ui, server)
