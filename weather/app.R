#
# API key generated in developer.accuweather.com and stored in env
# Sys.setenv(ACCUWEATHER_KEY = <your_api_key>)
#
#

library(shiny)
library(httr)
library(jsonlite)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("AccuWeather Weather App"),
    sidebarLayout(
        sidebarPanel(
            textInput("city", "City", "Palo Alto"),
            textInput("state", "State", "CA"),
            actionButton("submit", "Submit")
        ),
        mainPanel(
            plotOutput("weather")
        )
    )
)


# Define server logic required to draw a histogram
server <- function(input, output) {
    
    
    getWeather <- function(city, state) {
        url <- paste0("http://dataservice.accuweather.com/locations/v1/cities/search",
                      "?apikey=", Sys.getenv("ACCUWEATHER_KEY"),
                      "&q=", URLencode(paste(city, state, sep = ",")))
        res <- GET(url)
        location <- jsonlite::fromJSON(rawToChar(res$content))
        if (length(location) == 0) {
            stop("Location not found.")
        }
        url <- paste0("http://dataservice.accuweather.com/forecasts/v1/daily/1day/",
                      location$Key,
                      "?apikey=", Sys.getenv("ACCUWEATHER_KEY"))
        res <- GET(url)
        weather <- jsonlite::fromJSON(rawToChar(res$content))
        weather
    }
    
    output$weather <- renderPlot({
        daily <- getWeather(input$city, input$state)$DailyForecasts
        
        # Create vectors to store the forecast data
        dates <- sapply(daily, function(x) x$Date)
        temperatures <- sapply(daily, function(x) x$Temperature$Minimum$Value)
        realFeelTemperatures <- sapply(daily, function(x) x$RealFeelTemperature$Minimum$Value)
        
        # Create a data frame with the forecast data
        forecast_data <- data.frame(Date = dates,
                                    Temperature = temperatures,
                                    RealFeelTemperature = realFeelTemperatures)
        
        # Create the bar graph
        barplot(forecast_data$Temperature, names.arg = forecast_data$Date,
                col = "blue", main = "7-Day Forecast",
                xlab = "Date", ylab = "Temperature (°C)")
    })
    
}


# Run the application 
shinyApp(ui = ui, server = server)
