# explore and visualize cuisines


library(shiny)
library(shinyWidgets)
library(ggplot2)
library(plotly)
library(tidyr)
library(dplyr)
library(DT)
library(tidytext) # for tf_idf
library(forcats)
library(jsonlite)
library(wordcloud2)
#library(d3wordcloud) # for word cloud


# Data from: https://www.kaggle.com/datasets/kaggle/recipe-ingredients-dataset
setwd('~/rtdatasci_github/R/cuisines')
data <- fromJSON("train.json") %>% as.data.frame
# format to separate each igredients to rows
# df <- separate_rows(data, ingredients, sep = ",") # did not work -- missing values

recipes <- data %>% 
  mutate(ingredients = strsplit(as.character(ingredients), ",")) %>%
  unnest(ingredients) 

# remove special characters
recipes <- recipes %>% mutate(ingredients = stringr::str_remove_all(ingredients, "^c\\(|\\W"))


ui <- fluidPage(
  titlePanel('Explore Cuisines'),
  
  sidebarLayout(
    sidebarPanel(
      selectInput('cuisine', 'Select cuisine', unique(recipes$cuisine)),
      sliderInput('nb_ingredients', 'Number of ingredients', 5,100, 20)
      
      
      
    ), 
    
    mainPanel(
      tabsetPanel(
        tabPanel('Word Cloud', wordcloud2::wordcloud2Output('wc_ingredients')),
        tabPanel('Plot', plotly::plotlyOutput('plot_top_ingredients')),
        tabPanel('Table', DT::DTOutput('dt_top_ingredients'))
        
      )
    )
  )
)


server <- function(input, output, session){
  
  output$dt_top_ingredients <- DT::renderDT({
    recipes %>% 
      filter(cuisine == input$cuisine) %>% 
      count(ingredients, name = 'nb_recipes') %>% 
      arrange(desc(nb_recipes)) %>% 
      head(input$nb_ingredients)
  })
  
  # compute TFIDF (term frequency inverse document frequency) -- to find the real cases in a sea of cases
  recipes_enriched <- recipes %>% 
    count(cuisine, ingredients, name = 'nb_recipes') %>% 
    tidytext::bind_tf_idf(ingredients, cuisine, nb_recipes)
    
  
  # word cloud
  # reactive input
  rval_top_ingredients <- reactive({
    recipes_enriched %>% 
      filter(cuisine == input$cuisine) %>% 
      arrange(desc(tf_idf)) %>% 
      head(input$nb_ingredients) %>% 
      mutate(ingredients = forcats::fct_reorder(ingredients, tf_idf))
  })
  
  
  output$plot_top_ingredients <- plotly::renderPlotly({
    rval_top_ingredients() %>% 
      ggplot(aes(x = ingredients, y = tf_idf))+
      geom_col()+
      coord_flip()  # to make horizontal bar
  })
  
  output$wc_ingredients <- wordcloud2::renderWordcloud2({
    d <- rval_top_ingredients() %>% 
      select(ingredients, nb_recipes) %>% # ingredient and its frequency in recipes
      mutate(ingredients = as.character(ingredients))  # changed to char based on error on factors
    wordcloud2::wordcloud2(d, size=1.6, color='random-dark')
  })
  
  # output$wc_ingredients <- d3wordcloud::renderD3wordcloud({
  #   d <- rval_top_ingredients()
  #   d3wordcloud(d$ingredients, d$nb_recipes, tooltip=TRUE)
  # })
  
}

shinyApp(ui = ui, server= server)
