# Load required libraries
library(shiny)
library(tidyverse)
library(ggplot2)
library(scales)
library(lubridate)
library(DT)


# Read the CDC growth chart data from the CSV file (stature data from https://www.cdc.gov/growthcharts/percentile_data_files.html) where 1:male 2:female, age in months, percentiles in cm

# local test
#setwd("~/rtdatasci_github/R/growthchart") 
#cdc_growth_data <- read_csv("app/statage.csv") %>% 

# deployment ready file
cdc_growth_data <- read_csv("statage.csv") %>% 
  #rename Sex for clarity
  mutate(Sex = ifelse(Sex=="1", "Boys", "Girls")) %>%
  rename(Age = Agemos) %>% 
  # remove LMS (Lambda Mu and Sigma method) method used to smooth out percentile data -- not relevant here
  select(-L, -M, -S)

# Define the UI
ui <- fluidPage(
  titlePanel("CDC Growth Data Plot"),
  sidebarLayout(
    sidebarPanel(
      selectInput("sex", "Select Sex:", choices = c("Boys", "Girls"), selected = "Girls"),
      textInput("birthday", "Enter Birthday (YYYY-MM-DD):", value = "2017-03-04"),
      numericInput("height", "Enter Today's Height (cm):", value = 110, min = 0),
      actionButton("addButton", "Add Data")
    ),
    mainPanel(
      plotOutput("growthPlot"),
      tabsetPanel(
        tabPanel("Data Table", DTOutput("dataTable"))
      )
    )
  )
)

# Define the server logic
server <- function(input, output) {
  # Reactive values to store user data and data table
  data <- reactiveVal(cdc_growth_data)
  user_data_table <- reactiveVal(data.frame(Date = as.Date(character()),
                                            Sex = character(),
                                            Height = double(),
                                            PredictedHeight = double()))
  
  # Reactive value to store the current height entered by the user, the default 110 is replaced with user input later on
  current_height <- reactiveVal(110)
  
  # Function to convert birthday to age in months
  calc_age <- function(birthday) {
    today <- Sys.Date()
    as.integer(difftime(today, as.Date(birthday), units = "weeks") / 4.348)
  }
  
  # Observe changes in input values and update the plot
  observe({
    birthday_age <- calc_age(input$birthday)
    new_data <- data.frame(
      Sex = input$sex,
      Age = birthday_age,
      Height = input$height
    )
    data(bind_rows(cdc_growth_data, new_data))
    current_height(input$height) # Update current_height with the new user input
  })
  
  # Create the growth plot
  output$growthPlot <- renderPlot({
    filtered_data <- data() %>%
      filter(Sex == input$sex)
    
    user_data <- filtered_data %>%
      filter(Age == calc_age(input$birthday))
    
    ggplot(filtered_data, aes(x = Age)) +
      geom_line(aes(y = P3), color = "purple") +
      geom_line(aes(y = P5), color = "cyan") +
      geom_line(aes(y = P10), color = "blue") +
      geom_line(aes(y = P25), color = "green") +
      geom_line(aes(y = P50), color = "lightgreen") +
      geom_line(aes(y = P75), color = "darkgreen") +
      geom_line(aes(y = P90), color = "yellow") +
      geom_line(aes(y = P95), color = "orange") +
      geom_line(aes(y = P97), color = "red") +
      geom_point(data = user_data, aes(x = Age, y = Height), color = "red", size = 3) +
      geom_segment(data = user_data, aes(x = min(filtered_data$Age), xend = max(filtered_data$Age),
                                         y = Height, yend = Height),
                   color = "red", linetype = "dashed") +
      labs(
        title = "CDC Growth Data Plot",
        x = "Age (months)",
        y = "Height (cm)"
      )
  })
  
  # Add user data to data table and predict height at age 240 months
  observeEvent(input$addButton, {
    new_age <- calc_age(input$birthday)
    predicted_height_240 <- predict_height_at_240(new_age, current_height())
    new_entry <- data.frame(
      Date = as.Date(Sys.Date()),
      Sex = input$sex,
      Height = input$height,
      PredictedHeight = predicted_height_240
    )
    # Check if there is any existing entry for today and remove it
    user_data_table(user_data_table() %>%
                      filter(Date != Sys.Date()))
    # Add the new entry
    user_data_table(bind_rows(user_data_table(), new_entry))
  })
  
  # Function to predict height at age 240 months using the fitted spline curve for P25
  predict_height_at_240 <- function(age, current_height) {
    cdc_growth_data_P25 <- data() %>%
      filter(Sex == input$sex) %>%
      select(Age, P25)
    spline_function_P25 <- splinefun(x = cdc_growth_data_P25$Age, y = cdc_growth_data_P25$P25, method = "natural")
    predicted_height <- spline_function_P25(240) + (current_height - spline_function_P25(age))
    return(predicted_height)
  }
  
  # Render data table
  output$dataTable <- renderDT({
    user_data_table() %>%
      mutate(
        `Predicted Height (cm)` = format(PredictedHeight, nsmall = 2),
        `Predicted Height (ft)` = paste(floor(PredictedHeight / 30.48), "ft", round((PredictedHeight %% 30.48) / 2.54, 2), "in")
      ) %>%
      select(-PredictedHeight) %>%
      datatable(options = list(pageLength = 10))
  })
}


# Run the Shiny app
shinyApp(ui, server)
