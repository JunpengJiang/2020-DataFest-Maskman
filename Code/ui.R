library(plotly)
library(gapminder)
library(tidyverse)
library(janitor)
library(ggplot2)
library(gganimate)
library(devtools)
library(tidyselect)
library(gifski)
library(dplyr)
library(lubridate)
library(giphyr)


### Shiny App ####
ui <- fluidPage(
  # App title ----
  titlePanel("UCLA 2020 DataFest - COVID-19 Analysis"),
  
  ### select bars ####
  sidebarLayout(
    sidebarPanel(
      selectInput("action", label = h4("Select Action"), 
                  choices = c("About","Google Trend & News","Product Selection Overview","Price/Trend Analysis","Category Summary","Product Correlation Overview",
                              "Out of Stock Percentage","Summary"), 
                  selected = 1),
      uiOutput('ui1'),
      uiOutput('ui2'),
      uiOutput('ui_category'),
      hr()
    ),
    
    # end ####
    mainPanel(
     
      
      uiOutput('result1'),
      uiOutput('result2'),
      uiOutput('condition'),
      uiOutput('result3'),
      
      uiOutput('result4'),

      uiOutput('result5'),
  
      uiOutput('result6'),
      uiOutput('result7')
      
      
      
    )
  )
)
