## Publish button next to Run App
# requires to connect to shinyapps.io Tools -> Gloabl Options -> Publishing -> Connect

library(shiny)
library(ggplot2)
library(tidyverse)
library(bslib)  # for themes
library(rsconnect) # for deploying apps
library(DT) # htmlwidgets package for interactive data table output eg # DToutput renderDT datatable()
library(ggplot2)  
library(plotly)# to render an interactive plot, use plotly::renderPlotly(), and display it using plotly::plotlyOutput()


#global
babynames <- babynames::babynames

ui <- fluidPage(
  titlePanel("Popular baby names"),
  sidebarLayout(
    sidebarPanel(
      selectInput("sex", "Select Sex","M", choices=c("M","F")),
      sliderInput("year", "Select Year",value=1880,max=2017, min=1880)
    ),
    mainPanel(
      tabsetPanel(
        tabPanel('PLOT_TAB',
          plotOutput("popular_babyname")
        ),
        tabPanel('TABLE_TAB',
          DTOutput('top_ten')
        )
      )
      
    )
  )
)

server <- function(input, output, session) {
  # define get_top_names()
  get_top_names <- function(){
      babynames %>%
        filter(sex ==  input$sex) %>%
        filter(year == input$year) %>%
        slice_max(prop, n=10)
  }
  
  output$popular_babyname <- renderPlot({
    ggplot(get_top_names(),
           aes(x=name, y=prop))+geom_col()
    
  })
  output$top_ten <- renderDT({
    get_top_names() %>% datatable()
      })
}

shinyApp(ui = ui, server = server)

