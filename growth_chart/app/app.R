# Load required libraries
library(shiny)
library(tidyverse)
library(ggplot2)
library(scales)
library(lubridate)
library(DT)
library(jsonlite)

# Read the CDC growth chart data from the CSV file
cdc_growth_data <- read_csv("statage.csv") %>% 
  mutate(Sex = ifelse(Sex=="1", "Boys", "Girls")) %>%
  rename(Age = Agemos) %>% 
  select(-L, -M, -S)

# Define the UI
ui <- fluidPage(
  tags$head(
    tags$script(HTML("
      Shiny.addCustomMessageHandler('saveData', function(data) {
        localStorage.setItem('userDataTable', JSON.stringify(data));
      });
      
      Shiny.addCustomMessageHandler('loadData', function(message) {
        var data = localStorage.getItem('userDataTable');
        Shiny.setInputValue('loadedData', data);
      });
    "))
  ),
  titlePanel("CDC Growth Data Plot"),
  sidebarLayout(
    sidebarPanel(
      selectInput("sex", "Select Sex:", choices = c("Boys", "Girls"), selected = "Girls"),
      dateInput("birthday", "Select Birthday (YYYY-MM-DD):", value = as.Date("2020-01-30")),
      numericInput("height", "Enter Today's Height (cm):", value = 100, min = 0),
      actionButton("addButton", "Add Data"),
      downloadButton("downloadData", "Download Data")
    ),
    mainPanel(
      plotOutput("growthPlot"),
      plotOutput("historyPlot"),
      tabsetPanel(
        tabPanel("Data Table", DTOutput("dataTable"))
      )
    )
  )
)

# Define the server logic
server <- function(input, output, session) {
  # Reactive values to store user data and data table
  data <- reactiveVal(cdc_growth_data)
  
  user_data_table <- reactiveVal(data.frame(
    Date = as.Date(character()),
    Sex = character(),
    Height = double(),
    PredictedHeight = double()
  ))
  
  # Load data from browser's local storage
  observe({
    session$sendCustomMessage("loadData", list())
  })
  
  observeEvent(input$loadedData, {
    if (!is.null(input$loadedData) && input$loadedData != "") {
      loaded_data <- fromJSON(input$loadedData)
      user_data_table(loaded_data %>% 
                        mutate(Date = as.Date(Date)))
    }
  })
  
  current_height <- reactiveVal(100)
  
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
    current_height(input$height)
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
    updated_data <- bind_rows(user_data_table(), new_entry)
    user_data_table(updated_data)
    
    # Save updated data to browser's local storage
    session$sendCustomMessage("saveData", toJSON(updated_data, dataframe = "rows"))
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
  
  # Create history plot
  output$historyPlot <- renderPlot({
    req(nrow(user_data_table()) > 0)
    ggplot(user_data_table(), aes(x = Date, y = Height, color = Sex)) +
      geom_point() +
      geom_line() +
      labs(title = "Height History", x = "Date", y = "Height (cm)") +
      theme_minimal()
  })
  
  # Download handler for user data
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("user_data_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write_csv(user_data_table(), file)
    }
  )
}

# Run the Shiny app
shinyApp(ui, server)
