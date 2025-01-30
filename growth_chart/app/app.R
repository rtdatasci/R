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
        Shiny.setInputValue('loadedData', data ? data : '');
      });
    "))
  ),
  titlePanel("CDC Growth Data Plot"),
  sidebarLayout(
    sidebarPanel(
      selectInput("sex", "Select Sex:", choices = c("Boys", "Girls"), selected = "Girls"),
      dateInput("birthday", "Select Birthday (YYYY-MM-DD):", value = as.Date("2020-01-30")),
      tabsetPanel(
        tabPanel("Today's Measurement",
                 numericInput("height", "Enter Today's Height (cm):", value = 100, min = 0),
                 actionButton("addTodayButton", "Add Today's Data")
        ),
        tabPanel("Past Measurement",
                 dateInput("pastMeasurementDate", "Measurement Date:", value = Sys.Date()),
                 numericInput("pastHeight", "Enter Height (cm):", value = 100, min = 0),
                 actionButton("addPastButton", "Add Past Data")
        )
      ),
      downloadButton("downloadData", "Download Data"),
      actionButton("clearData", "Clear All Data")
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
  data <- reactiveVal(cdc_growth_data)
  
  user_data_table <- reactiveVal(data.frame(
    Date = as.Date(character()),
    Sex = character(),
    Age = double(),
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
  
  # Function to convert birthday to age in months
  calc_age <- function(birthday, measurement_date) {
    as.numeric(difftime(measurement_date, as.Date(birthday), units = "weeks") / 4.348)
  }
  
  # Function to add data point
  add_data_point <- function(date, height) {
    age <- calc_age(input$birthday, date)
    predicted_height_240 <- predict_height_at_240(age, height, input$sex)
    new_entry <- data.frame(
      Date = as.Date(date),
      Sex = input$sex,
      Age = age,
      Height = height,
      PredictedHeight = predicted_height_240
    )
    updated_data <- bind_rows(user_data_table(), new_entry) %>%
      arrange(Date)
    user_data_table(updated_data)
    
    # Save updated data to browser's local storage
    session$sendCustomMessage("saveData", toJSON(updated_data, dataframe = "rows"))
  }
  
  # Add today's data
  observeEvent(input$addTodayButton, {
    add_data_point(Sys.Date(), input$height)
  })
  
  # Add past data
  observeEvent(input$addPastButton, {
    add_data_point(input$pastMeasurementDate, input$pastHeight)
  })
  
  # Clear all data
  observeEvent(input$clearData, {
    user_data_table(data.frame(
      Date = as.Date(character()),
      Sex = character(),
      Age = double(),
      Height = double(),
      PredictedHeight = double()
    ))
    session$sendCustomMessage("saveData", toJSON(data.frame(), dataframe = "rows"))
  })
  
  # Function to predict height at age 240 months using the fitted spline curve for P25
  predict_height_at_240 <- function(age, current_height, sex) {
    cdc_growth_data_P25 <- data() %>%
      filter(Sex == sex) %>%
      select(Age, P25)
    spline_function_P25 <- splinefun(x = cdc_growth_data_P25$Age, y = cdc_growth_data_P25$P25, method = "natural")
    predicted_height <- spline_function_P25(240) + (current_height - spline_function_P25(age))
    return(predicted_height)
  }
  
  # Create the growth plot
  output$growthPlot <- renderPlot({
    req(nrow(user_data_table()) > 0)
    
    last_entry <- user_data_table() %>% slice_max(Date)
    
    filtered_data <- data() %>%
      filter(Sex == last_entry$Sex)
    
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
      geom_point(data = user_data_table(), aes(x = Age, y = Height), color = "red", size = 3) +
      geom_line(data = user_data_table(), aes(x = Age, y = Height), color = "red", linetype = "dashed") +
      labs(
        title = "CDC Growth Data Plot",
        x = "Age (months)",
        y = "Height (cm)"
      )
  }) 
  
  # Render data table with delete buttons
  output$dataTable <- renderDT({
    req(nrow(user_data_table()) > 0)
    data_with_buttons <- user_data_table() %>%
      mutate(
        `Predicted Height (cm)` = format(PredictedHeight, nsmall = 2),
        `Predicted Height (ft)` = paste(floor(PredictedHeight / 30.48), "ft", round((PredictedHeight %% 30.48) / 2.54, 2), "in"),
        Delete = paste('<button class="btn btn-danger btn-sm delete-btn" data-date="', Date, '">Delete</button>')
      ) %>%
      select(-PredictedHeight)
    
    datatable(
      data_with_buttons,
      options = list(pageLength = 10, order = list(list(0, 'desc'))),
      escape = FALSE,
      callback = JS("
        table.on('click', '.delete-btn', function() {
          var date = $(this).data('date');
          Shiny.setInputValue('delete_row', date, {priority: 'event'});
        });
      ")
    )
  })
  
  # Handle row deletion
  observeEvent(input$delete_row, {
    date_to_delete <- as.Date(input$delete_row)
    updated_data <- user_data_table() %>%
      filter(Date != date_to_delete)
    user_data_table(updated_data)
    session$sendCustomMessage("saveData", toJSON(updated_data, dataframe = "rows"))
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

