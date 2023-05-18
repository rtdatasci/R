# explore and visualize cuisines


library(shiny)
library(shinyWidgets)
library(shinythemes)
library(ggplot2)
library(plotly)
library(tidyr)
library(dplyr)
library(DT)
library(tidytext) # for tf_idf
library(forcats)
library(jsonlite)
library(wordcloud2)
library(leaflet)


# Data from: https://www.kaggle.com/datasets/zusmani/us-mass-shootings-last-50-years?select=Mass+Shootings+Dataset.csv
setwd('~/rtdatasci_github/R/mass_shootings')
mass_shootings <- read.csv("Mass Shootings Dataset.csv")

# from Kaggle info on dataset
text_about <- "Mass Shootings in the United States of America (1966-2017)
The US has witnessed 398 mass shootings in last 50 years that resulted in 1,996 deaths and 2,488 injured. The latest and the worst mass shooting of October 2, 2017 killed 58 and injured 515 so far. The number of people injured in this attack is more than the number of people injured in all mass shootings of 2015 and 2016 combined.
The average number of mass shootings per year is 7 for the last 50 years that would claim 39 lives and 48 injured per year."


## Pre-processing and cleanup
mass_shootings <- mass_shootings %>% 
  mutate(Date = as.character(as.Date(Date, "%m/%d/%Y")))


## full page with no margins (instead of fulidpage)
ui <- bootstrapPage(
  theme = shinythemes::shinytheme('simplex'),
  leaflet::leafletOutput(outputId = 'map', width = '100%', height = '100%'),
  
  
  # input on top right
  absolutePanel(top = 10, right = 10, id = 'controls',
                sliderInput('nb_fatalities', 'Minimum fatalities', 1,40,10),
                dateRangeInput('date_range', 'Select Date', "2011-09-06","2015-01-01"),
                actionButton('show_about', 'About')
                ),
  # custom white style
  tags$style(type = "text/css","
  html, body{ width:100%; height:100%}
  controls{background-color:white; padding=20px;}
             ")
  
  
)


server <- function(input, output, session){
  
  # about info
  observeEvent(input$show_about,{
    showModal(modalDialog(text_about, title = 'About'))
  })
  

  observeEvent(input$date_range, {
    if (input$date_range[2] < input$date_range[1]) {
      # validate(
      #   need(input$date_range[2] > input$date_range[1] ,
      #        "Select second date later than the first date")
      # )
      
      # If end date is earlier than start date, update the end date to match the start date
      updateDateRangeInput(session, "date_range", start = input$date_range[1], end = input$date_range[1])
    }
  })
  

  
  # add reactive exp
  rval_mass_shootings <- reactive({
    mass_shootings %>% 
      filter(
        Date >= input$date_range[1],
        Date <= input$date_range[2],
        Fatalities >= input$nb_fatalities
          
      )
  })
  
  output$map <- leaflet::renderLeaflet({
    
    rval_mass_shootings() %>% 
      leaflet() %>% 
      addTiles() %>% 
      setView(-98,39,zoom = 5) %>% 
      addCircleMarkers(
        popup = ~Summary,
        radius = ~Fatalities,
        fillColor = 'red', color = 'red', weight = 1
      )
  })
  
}


shinyApp(ui = ui, server= server)
