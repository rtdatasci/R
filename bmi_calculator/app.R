library(shiny)
library(ggplot2)
library(tidyverse)
library(bslib)  # for themes
library(rsconnect) # for deploying apps
library(DT) # htmlwidgets package for interactive data table output eg # DToutput renderDT datatable()
library(ggplot2)  
library(plotly)


ui <- fluidPage(
  titlePanel('BMI Calculator'),
  theme = shinythemes::shinytheme('cosmo'),
  sidebarLayout(
    sidebarPanel(
      numericInput('height', 'Enter your height in meters', 1.5, 1, 2),
      numericInput('weight', 'Enter your weight in Kilograms', 60, 45, 120)
    ),
    mainPanel(
      textOutput("bmi"),
      textOutput("bmi_status")
    )
  )
)

server <- function(input, output, session) {
  rval_bmi <- reactive({
    input$weight/(input$height^2)
  })
  # CODE BELOW: Add a reactive expression rval_bmi_status to 
  # return health status as underweight etc. based on inputs
  rval_bmi_status <- reactive({
    cut(rval_bmi(), 
        breaks = c(0, 18.5, 24.9, 29.9, 40),
        labels = c('underweight', 'healthy', 'overweight', 'obese')
    )
  })
  
  output$bmi <- renderText({
    bmi <- rval_bmi()
    paste("Your BMI is", round(bmi, 1))
  })
  output$bmi_status <- renderText({
    # MODIFY CODE BELOW: Replace right-hand-side with 
    # reactive expression rval_bmi_status
    bmi_status <- rval_bmi_status()
    paste("You are", bmi_status)
  })
}

shinyApp(ui, server)
