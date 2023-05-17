# 2016 Mental health in tech survey app

# utilize shinyWidgetsGallery()

library(shiny)
library(shinyWidgets)
library(ggplot2)
library(dplyr)
library(DT)



#Data
# Open Sourcing Mental Illness
# Downloaded from: https://www.kaggle.com/datasets/osmi/mental-health-in-tech-survey
setwd("~/rtdatasci_github/R/mental_health_in_tech/")
data <- read.csv("survey.csv")

## Pre-processing and cleaning
mental_health_survey <- data

# non negative ages Age>0
mental_health_survey <- mental_health_survey %>% filter(Age > 0)
#


ui <- fluidPage(
  titlePanel("2014 Mental Health in Tech Survey"),
  sidebarPanel(
    sliderTextInput(
      inputId = "work_interfere",
      label = "If you have a mental health condition, do you feel that it interferes with your work?", 
      grid = TRUE,
      force_edges = TRUE,
      choices = c("Never", "Rarely", "Sometimes", "Often")
    ),
    checkboxGroupInput(
      inputId = "mental_health_consequence",
      label = "Do you think that discussing a mental health issue with your employer would have negative consequences?", 
      choices = c("Maybe", "Yes", "No"),
      selected = "Maybe"
    ),
    pickerInput(
      inputId = "mental_vs_physical",
      label = "Do you feel that your employer takes mental health as seriously as physical health?", 
      choices = c("Don't Know", "No", "Yes"),
      multiple = TRUE
    )    
  ),
  mainPanel(
    plotOutput("age")  
  )
)

server <- function(input, output, session) {
  output$age <- renderPlot({
    # Add validation that user selected a 3rd input
    validate(
      need(input$mental_vs_physical , "Select an option in all the questions to plot")
    )
    
    
    mental_health_survey %>%
      filter(
        work_interfere == input$work_interfere,
        mental_health_consequence %in% input$mental_health_consequence,
        mental_vs_physical %in% input$mental_vs_physical
      ) %>%
      ggplot(aes(Age)) +
      geom_histogram()
  })
}

shinyApp(ui, server)
