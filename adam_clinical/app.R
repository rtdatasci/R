#
# Clinical data ADaM or analysis data 
# dataset adsl.xpt (SL=Subject Level) from CDISC



library(shiny)   
library(shinyWidgets)
library(shinythemes)
library(haven)   # Read sas
library(ggplot2) # viz
library(scales)  
library(bslib)   
library(plotly)

# Read in Data -------------------------------
#setwd("~/rtdatasci_github/R/adam_clinical")
adsl <- read_xpt("adsl.xpt")

# User Interface -----------------------------
ui <- fluidPage(
    
    titlePanel("ADaM Subject-Level Analysis"),
    
    sidebarLayout(
      sidebarPanel(
        selectInput(inputId = "subject_data",
                    label = "Subject Data",
                    choices = c(
                      "Age" = "AGE",
                      "Baseline BMI" = "BMIBL",
                      "Baseline Height" = "HEIGHTBL",
                      "Baseline Weight" = "WEIGHTBL",
                      "Years of Education" = "EDUCLVL"
                      )
                    )
      ),
      mainPanel(
        plotlyOutput("boxplot")
          
        )
      )
    
)


# Server Function ---------------------------
server <- function(input, output, session) {
    
    # Create Plot
    output$boxplot <- renderPlotly({
        the_plot <- ggplot(data = adsl, aes(x = TRT01A,
                                            y = .data[[input$subject_data]],
                                            fill = TRT01A)) +
            geom_boxplot() +
            geom_jitter(width = 0.3, alpha = 0.4) +
            theme_minimal() +
            theme(legend.position = "none",
                  text = element_text(size = 15)) +
            labs(
                title = "ADSL Data",
                subtitle = "Comparing Treatment Groups",
                x = "",
                y = attributes(adsl[[input$subject_data]])
            ) +
            scale_x_discrete(labels = label_wrap(10))
        
        ggplotly(the_plot)
    })
}


# Run the application 
shinyApp(ui = ui, server = server)
