combine <- read.csv("Rescaled.csv")
stock_formatted <- read_csv('stock_formatted.csv')
health_gt <- read.csv("health_google_trend.csv")
cpt <- read.csv("Rescaled.csv")
data1 <- read.csv("avgcombinedc.csv")

#### News #######
health_gt$Date <- mdy(as.character(health_gt$Date))
health_gt <- health_gt[order(health_gt$Date),]
colnames(health_gt)
health_gt_tidy <- pivot_longer(health_gt,
                               cols = 2:6,
                               names_to = "health_type",
                               values_to = "Search_rate")
health_gt_tidy1 <- health_gt_tidy[health_gt_tidy["health_type"]==c("Hand.Sanitizer","Disinfectant.Wipes"),]
new_row0 <- data.frame("Date"=mdy("1/30/2020"),"health_type"="Hand.Sanitizer","Search_rate" = 2)
new_row1 <- data.frame("Date"=mdy("03/13/2020"),"health_type"="Disinfectant.Wipes","Search_rate" = 81)
new_row2 <- data.frame("Date"=mdy("03/21/2020"),"health_type"="Disinfectant.Wipes","Search_rate" = 67.5)
new_row3 <- data.frame("Date"=mdy("04/03/2020"),"health_type"="Hand.Sanitizer","Search_rate" = 34.5)

health_gt_tidy1 <-rbind(health_gt_tidy1,new_row0,new_row1,new_row2,new_row3)
health_gt_tidy1 <- health_gt_tidy1 %>% arrange(Date)
## Annotation 
data <- data.frame("Date"= c(mdy("1/30/2020"),mdy("2/25/2020"),mdy("3/13/2020"),mdy("3/21/2020"),mdy("04/03/2020")),
                   "Word" = c('WHO declares coronavirus outbreak a global health emergency\nUS Confirmed Cases: 5\nWorld Confirmed Cases: 8234',
                              'US Confirmed Cases: 15\nWorld Confirmed Cases: 80,370',
                              'US Confirmed Cases: 2,100\nWorld Confirmed Cases: 145,187',
                              'Day 2 of Stay-at-Home Order of CA, NY, and IL\nUS Confirmed Cases: 25,700\nWorld Confirmed Cases: 304,680',
                              'US Confirmed Cases: 275,600\nWorld Confirmed Cases: 1,0960,000'))

##### Heat Map ##############
col_names <- c("Health Care", "Household", "Food", "Others")
row_names <- c("Others", "Food", "Household", "Health Care")

healthcare_price <- combine$price[combine$group == "Health Care"]
healthcare_price <- c(healthcare_price, rep(mean(healthcare_price), 129))
healthcare_trend <- combine$trend[combine$group == "Health Care"]
healthcare_trend <- c(healthcare_trend, rep(mean(healthcare_trend), 129))
#healthcare_trend <- healthcare_trend * min(healthcare_price)

household_price <- combine$price[combine$group == "Personal Care/Household"]
household_price <- c(household_price, mean(household_price))
household_trend <- combine$trend[combine$group == "Personal Care/Household"]
household_trend <- c(household_trend, mean(household_trend))
#household_trend <- household_trend * min(household_price)

unperishablefood_price <- combine$price[combine$group == "Non-Perishable Food"]
unperishablefood_price <- c(unperishablefood_price, rep(mean(unperishablefood_price), 14))

unperishablefood_trend <- combine$trend[combine$group == "Non-Perishable Food"]
unperishablefood_trend <- c(unperishablefood_trend, rep(mean(unperishablefood_trend), 14))
#unperishablefood_trend <- unperishablefood_trend * min(unperishablefood_price)

others_price <- combine$price[combine$group == "Others"]
others_trend <- combine$trend[combine$group == "Others"]
#others_trend <- others_trend * min(others_price)





#### Group ######
rownames(data1) <- as.character(data1$date)
data <- data1[, -1]

colnames(data) <- rev(sort(c("Bottled Water", "Canned Soup", "Disinfectant Spray", "Disinfectant Wipes",
                             "Dumbell", "Hair Clipper", "Hair Color", "Hand Sanitizer", "Instant Noodles",   
                             "N95 Mask", "Nintendo Switch", "Olive Oil", "Pet Supply", "Printer Ink",       
                             "Rice", "Shampoo", "Toilet Paper", "Trash Bag", "Yoga Mat" )))



new_data <- t(data)

nrow(new_data)
ncol(new_data)
a <- c(1:19)
b <- c(2:116)
for (i in a){
  upperq <- quantile(as.vector(new_data[12, ]))[[4]] + sd(as.vector(new_data[12, ])) * 1.5
  lowerq <- quantile(as.vector(new_data[12, ]))[[2]] - sd(as.vector(new_data[12, ])) * 1.5
  
  
  kk <- which(as.vector(new_data[12, ]) < lowerq | as.vector(new_data[12, ]) > upperq)
  
  new_data[12, kk] <- new_data[12, kk - 1]
  
  for (j in b) {
    if ((new_data[i, j] > (new_data[i, j - 1] * 1.03) && new_data[i, j] > (new_data[i, j + 1] * 1.03)) || (new_data[i, j] < (new_data[i, j - 1] * 0.97) && new_data[i, j] < (new_data[i, j + 1] * 0.97))) {
      new_data[i, j] <- new_data[i, j - 1]
    }
  }
}


#####  Server #################

server <- function(input, output){
  
  output$ui1 <- renderUI({
    if (is.null(input$action))
      return()
    switch (input$action,
            'Price/Trend Analysis' = selectInput("category", label = h4("Select Category"), 
                                                 choices = c("Overall","Health Care", "Non-Perishable Food","Personal Care/Household",
                                                             "Others"), 
                                                 selected = 1) 
    ) 
  })
  

  output$result1 <- renderUI({
    switch (input$action,
            'About' = imageOutput('about'),
            "Google Trend & News" = uiOutput("news_title1"),
            'Product Selection Overview' = uiOutput("pie_title"),
            'Price/Trend Analysis' = uiOutput("scatter_title"),
            "Category Summary" = uiOutput('summary_title'),
            "Product Correlation Overview"=tabsetPanel(id = "cor",
                                                       tabPanel("Google Search Trend", imageOutput("trend_heatmap")),
                                                       tabPanel("Product Prices", imageOutput("price_heatmap"))),
            'Out of Stock Percentage' = imageOutput('gif')
    ) 
  })
  
  output$result2 <- renderUI({

    switch (input$action,
            'Product Selection Overview' = plotlyOutput("pie"),
            "Google Trend & News" = plotlyOutput("trendnews1"),
            "Category Summary" = tabsetPanel(id = 'group',
                                             tabPanel("Category Overview Scatter Plot",plotlyOutput("group_scatter")),
                                             tabPanel("Category Overview Animated Plot",plotlyOutput("group_scatter_move"))
                                             ),
            'Summary' = uiOutput('summary')
              
           
    ) 
  })
  
  output$condition <- renderUI({
    if(is.null(input$category)) return()
    if(input$category == "Overall") return()
    switch(
      input$action,
        'Price/Trend Analysis'= plotlyOutput("product_scatter")
      )
    #& input$action == 'Price/Trend Analysis'
    
  })
  
  output$result3 <- renderUI({
    switch (input$action,
            "Google Trend & News" = plotlyOutput("trendnews2"),
            'Price/Trend Analysis' = uiOutput("space"),
            "Category Summary" = uiOutput("summary_title2"),
            'Summary' = imageOutput('last')
            
            
    ) 
  })
  output$result4 <- renderUI({
    switch (input$action,
            "Category Summary" = plotlyOutput("spider")
    ) 
  })
  output$result5 <- renderUI({
    switch (input$action,
    "Category Summary" = uiOutput("summary_title3")
    )
  })
  
  output$result7 <- renderUI({
    switch (input$action,
            "Category Summary" = plotlyOutput("groupheatmap_ttpp")
    )
  })
  
  
  output$result6 <- renderUI({
    if(is.null(input$category)) return()
    if(is.null(input$action)) return()
    if (input$action != "Price/Trend Analysis")
      return()
    switch (input$category,
            "Overall" = plotlyOutput("all_product"),
            "Health Care"=tabsetPanel(tabPanel("Animated Line Chart", imageOutput('health_gif')),
                                      tabPanel("Heat Map", plotlyOutput("healthcare_heatmap")),
                                      tabPanel("Correlation - Product Elasticity", plotlyOutput("healthcare_spider"))),
            "Non-Perishable Food"=tabsetPanel(tabPanel("Animated Line Chart", imageOutput('food_gif')),
                                              tabPanel("Heat Map", plotlyOutput("food_heatmap")),
                                              tabPanel("Correlation - Product Elasticity", plotlyOutput("food_spider"))),
            "Personal Care/Household"=tabsetPanel(tabPanel("Animated Line Chart", imageOutput('household_gif')),
                                                  tabPanel("Heat Map", plotlyOutput("personal_heatmap")),
                                                  tabPanel("Correlation - Product Elasticity", plotlyOutput("personal_spider"))),
            "Others"=tabsetPanel(tabPanel("Animated Line Chart", imageOutput('others_gif')),
                                 tabPanel("Heat Map", plotlyOutput("other_heatmap")),
                                 tabPanel("Correlation - Product Elasticity", plotlyOutput("other_spider")))
            
    )
    
  })
  
  
  
  output$pie_title <- renderUI({ 
    h3("Product Selection Overview")
  })
  
  output$summary <- renderUI({ 
    h2("Summary")
  })
  
  output$summary_title <- renderUI({ 
    h3("Category Overview")
  })
  
  output$summary_title2 <- renderUI({ 
    h3("Price & Trend Correlation - Category Elasticity")
  })
  
  output$summary_title3 <- renderUI({ 
    h3("Price vs Price & Trend vs Trend Correlation")
  })
  
  output$scatter_title <- renderUI({ 
    if(input$category == "Overall") return()
    h3("Standardized Price vs. Google Search Trend:", input$category)
  })
  output$news_title1 <- renderUI({ 
    h3("Health Care Products Google Search Trend")
  })
  output$spider_title <- renderUI({ 
    h3("Health Care Products Google Search Trend")
  })
  
  output$space <- renderUI({ 
    br()
    h3("Standardized Price Over Time")
  })

 
 
 output$trendnews1 <- renderPlotly({
   fig3 <- plot_ly(health_gt_tidy1, x = ~Date, y = ~Search_rate,color = ~health_type)
   fig3 <- fig3 %>% add_lines()
   fig3 <- fig3 %>% add_trace(
     type = 'scatter',
     name = 'Disinfectant_Wipes',
     mode = 'lines+markers',
     x = mdy("1/30/2020"),
     y = 4,
     color = 'green',
     text = 'WHO declares coronavirus outbreak a global health emergency\nUS Confirmed Cases: 5\nWorld Confirmed Cases: 8234',
     showlegend = FALSE
   )%>% add_trace(
     type = 'scatter',
     mode = 'lines+markers',
     name = 'Hand_Sanitizer',
     x = mdy("1/30/2020"),
     y = 2,
     color = 'orange',
     text = 'WHO declares coronavirus outbreak a global health emergency\nUS Confirmed Cases: 5\nWorld Confirmed Cases: 8234',
     showlegend = FALSE
   )%>% add_trace(
     type = 'scatter',
     name = 'Disinfectant_Wipes',
     mode = 'lines+markers',
     color = 'green',
     x = mdy("2/25/2020"),
     y = 2,
     text = 'San Francisco mayor declares state of emergency\nUS Confirmed Cases: 15\nWorld Confirmed Cases: 80,370',
     showlegend = FALSE
   )%>% add_trace(
     type = 'scatter',
     name = 'Hand_Sanitizer',
     mode = 'lines+markers',
     color = 'orange',
     x = mdy("2/25/2020"),
     y = 1,
     text = 'San Francisco mayor declares state of emergency\nUS Confirmed Cases: 15\nWorld Confirmed Cases: 80,370',
     showlegend = FALSE
   )%>% add_trace(
     type = 'scatter',
     name = 'Disinfectant_Wipes',
     mode = 'lines+markers',
     color = 'green',
     x = mdy("3/12/2020"),
     y = 84.0,
     text = 'UCLA suspended all in-person classes two days ago\nUS Confirmed Cases: 1,600\nWorld Confirmed Cases: 128,246',
     showlegend = FALSE
   )%>% add_trace(
     type = 'scatter',
     name = 'Hand_Sanitizer',
     mode = 'lines+markers',
     color = 'orange',
     x = mdy("3/13/2020"),
     y = 93.0,
     text = 'UCLA suspended all in-person classes three days ago\nUS Confirmed Cases: 2,100\nWorld Confirmed Cases: 145,187',
     showlegend = FALSE
   )%>% add_trace(
     type = 'scatter',
     name = 'Disinfectant_Wipes',
     mode = 'lines+markers',
     color = 'green',
     x = mdy("3/22/2020"),
     y = 75.0,
     text = 'Day 3 of Stay-at-Home Order of CA, NY, and IL\nUS Confirmed Cases: 33,600\nWorld Confirmed Cases: 337,376',
     showlegend = FALSE
   )%>% add_trace(
     type = 'scatter',
     name = 'Hand_Sanitizer',
     mode = 'lines+markers',
     color = 'orange',
     x = mdy("3/21/2020"),
     y = 45.0,
     text = 'Day 2 of Stay-at-Home Order of CA, NY, and IL\nUS Confirmed Cases: 25,700\nWorld Confirmed Cases: 304,680',
     showlegend = FALSE
   )%>% add_trace(
     type = 'scatter',
     name = 'Disinfectant_Wipes',
     mode = 'lines+markers',
     color = 'green',
     x = mdy("4/3/2020"),
     y = 88.0,
     text = 'NYC reports more than 6500 coronavirus cases\nUS Confirmed Cases: 275,600\nWorld Confirmed Cases: 1,0960,000',
     showlegend = FALSE
   )%>% add_trace(
     type = 'scatter',
     name = 'Hand_Sanitizer',
     mode = 'lines+markers',
     color = 'orange',
     x = mdy("4/4/2020"),
     y = 36.0,
     text = 'US marks record for most new coronavirus deaths (1224)\nUS Confirmed Cases: 308,800\nWorld Confirmed Cases: 1,1760,000',
     showlegend = FALSE
   )%>% add_trace(
     type = 'scatter',
     name = 'Disinfectant_Wipes',
     mode = 'lines+markers',
     color = 'green',
     x = mdy("4/15/2020"),
     y = 52.0,
     text = 'The Los Angeles Mayor has unveiled a plan to reopen the city\nUS Confirmed Cases: 275,600\nWorld Confirmed Cases: 1,0960,000',
     showlegend = FALSE
   )%>% add_trace(
     type = 'scatter',
     name = 'Hand_Sanitizer',
     mode = 'lines+markers',
     color = 'orange',
     x = mdy("4/12/2020"),
     y = 16.0,
     text = 'Total number of hospitalizations are down in New York, governor says\nUS Confirmed Cases: 308,800\nWorld Confirmed Cases: 1,1760,000',
     showlegend = FALSE
   )%>% layout(yaxis = list(title = "Google Search Trend - US"))
   fig3 
 })
 
 output$trendnews2 <- renderPlotly({
   ##### N95 Mask and Disinfectant Spray
   health_gt_tidy2 <- health_gt_tidy[health_gt_tidy["health_type"]==c("N95.Mask","Disinfectant.Spray"),]
   row1 <- data.frame("Date"=mdy("03/13/2020"),"health_type"="Disinfectant.Wipes","Search_rate" = 82)
   fig4 <- plot_ly(health_gt_tidy2, x = ~Date, y = ~Search_rate,color = ~health_type)
   fig4 <- fig4 %>% add_lines()
   fig4 <- fig4 %>% add_trace(
     type = 'scatter',
     mode = 'lines+markers',
     name = 'N95 Mask',
     color = '6666FF',
     x = mdy("1/30/2020"),
     y = 13.5,
     text = 'WHO declares coronavirus outbreak a global health emergency\nUS Confirmed Cases: 5\nWorld Confirmed Cases: 8234',
     showlegend = FALSE
   ) %>% add_trace(
     type = 'scatter',
     mode = 'lines+markers',
     name = 'Disinfectant Spray',
     color = '66FFB2',
     x = mdy("1/31/2020"),
     y = 3,
     text = 'Day 2 of WHO declares coronavirus outbreak a global health emergency\nUS Confirmed Cases: 5\nWorld Confirmed Cases: 8234',
     showlegend = FALSE
   )%>% add_trace(
     type = 'scatter',
     mode = 'lines+markers',
     name = 'N95 Mask',
     color = '6666FF',
     x = mdy("2/26/2020"),
     y = 72,
     text = 'Orange County declares state of emergency\nUS Confirmed Cases: 15\nWorld Confirmed Cases: 81,346',
     showlegend = FALSE
   ) %>% add_trace(
     type = 'scatter',
     mode = 'lines+markers',
     name = 'Disinfectant Spray',
     color = '66FFB2',
     x = mdy("3/7/2020"),
     y = 13,
     text = '30 States of US has found coronavirus cases\nUS Confirmed Cases: 336\nWorld Confirmed Cases: 105,781',
     showlegend = FALSE
   )%>% add_trace(
     type = 'scatter',
     mode = 'lines+markers',
     name = 'N95 Mask',
     color = '6666FF',
     x = mdy("3/13/2020"),
     y = 25,
     text = 'UCLA suspended all in-person classes three days ago\nUS Confirmed Cases: 2,100\nWorld Confirmed Cases: 145,187',
     showlegend = FALSE
   ) %>% add_trace(
     type = 'scatter',
     mode = 'lines+markers',
     name = 'Disinfectant Spray',
     color = '66FFB2',
     x = mdy("3/13/2020"),
     y = 44,
     text = 'UCLA suspended all in-person classes three days ago\nUS Confirmed Cases: 2,100\nWorld Confirmed Cases: 145,187',
     showlegend = FALSE
   )%>% add_trace(
     type = 'scatter',
     mode = 'lines+markers',
     name = 'N95 Mask',
     color = '6666FF',
     x = mdy("3/21/2020"),
     y = 63,
     text = 'Day 2 of Stay-at-Home Order of CA, NY, and IL\nUS Confirmed Cases: 25,700\nWorld Confirmed Cases: 304,680',
     showlegend = FALSE
   ) %>% add_trace(
     type = 'scatter',
     mode = 'lines+markers',
     name = 'Disinfectant Spray',
     color = '66FFB2',
     x = mdy("3/21/2020"),
     y = 46,
     text = 'Day 2 of Stay-at-Home Order of CA, NY, and IL\nUS Confirmed Cases: 25,700\nWorld Confirmed Cases: 304,680',
     showlegend = FALSE
   )%>% add_trace(
     type = 'scatter',
     mode = 'lines+markers',
     name = 'N95 Mask',
     color = '6666FF',
     x = mdy("4/4/2020"),
     y = 100,
     text = 'US marks record for most new coronavirus deaths (1224)\nUS Confirmed Cases: 308,800\nWorld Confirmed Cases: 1,1760,000',
     showlegend = FALSE
   ) %>% add_trace(
     type = 'scatter',
     mode = 'lines+markers',
     name = 'Disinfectant Spray',
     color = '66FFB2',
     x = mdy("4/4/2020"),
     y = 100,
     text = 'US marks record for most new coronavirus deaths (1224)\nUS Confirmed Cases: 308,800\nWorld Confirmed Cases: 1,1760,000',
     showlegend = FALSE
   ) %>% add_trace(
     type = 'scatter',
     mode = 'lines+markers',
     name = 'Disinfectant Spray',
     color = '66FFB2',
     x = mdy("4/12/2020"),
     y = 37,
     text = 'New Hampshire governor receives 7 million pieces of PPE to combat coronavirus\nUS Confirmed Cases: 308,800\nWorld Confirmed Cases: 1,1760,000',
     showlegend = FALSE
   ) %>% add_trace(
     type = 'scatter',
     mode = 'lines+markers',
     name = 'N95 Mask',
     color = '6666FF',
     x = mdy("4/12/2020"),
     y = 26,
     text = 'Temporary hospital facility to be built in Atlanta ahead of potential coronavirus surge\nUS Confirmed Cases: 308,800\nWorld Confirmed Cases: 1,1760,000',
     showlegend = FALSE
   )%>% layout(yaxis = list(title = "Google Search Trend - US"))
   fig4
 })
 
 output$healthcare_heatmap <- renderPlotly({
   data_healthcare <- new_data[c(10, 12, 16, 17), c(T, T, F, F, F, F, F, F, F, F)]
   p_healthcare <- plot_ly(y = rownames(data_healthcare),
                           x = colnames(data_healthcare),
                           z = data_healthcare,
                           type = "heatmap",
                           colors = colorRamp(c("cadetblue1", "cornflowerblue"))
   )
   p_healthcare <- p_healthcare %>% layout(autosize = T, title = "Health Care")
   p_healthcare
 })
 
 output$food_heatmap <- renderPlotly({
   data_food <- new_data[c(5, 8, 11, 18, 19), c(T, T, F, F, F, F, F, F, F, F)]
   p_food <- plot_ly(y = rownames(data_food),
                     x = colnames(data_food),
                     z = data_food,
                     type = "heatmap",
                     colors = colorRamp(c("cadetblue1", "cornflowerblue"))
   )
   p_food <- p_food %>% layout(autosize = T, title = "Non Perishable Food")
   p_food
 })
 
 output$personal_heatmap <- renderPlotly({
   data_personalcare <- new_data[c(2, 3, 4, 13, 14), c(T, T, F, F, F, F, F, F, F, F)]
   p_personalcare <- plot_ly(y = rownames(data_personalcare),
                             x = colnames(data_personalcare),
                             z = data_personalcare,
                             type = "heatmap",
                             colors = colorRamp(c("cadetblue1", "cornflowerblue"))
   )
   p_personalcare <- p_food %>% layout(autosize = T, title = "Personal Care / Household")
   p_personalcare
 })
 
 output$other_heatmap <- renderPlotly({
   data_others <- new_data[c(1, 6, 7, 9, 15), c(T, T, F, F, F, F, F, F, F, F)]
   p_others <- plot_ly(y = rownames(data_others),
                       x = colnames(data_others),
                       z = data_others,
                       type = "heatmap",
                       
                       colors = colorRamp(c("cadetblue1", "cornflowerblue"))
   )
   p_others <- p_others %>% layout(autosize = T, title = "Others")
   p_others
 })
 
 
 output$healthcare_spider <- renderPlotly({
   cor_value =  c(cor(combine[combine$category == "N95 Mask",]$trend,combine[combine$category == "N95 Mask",]$price),
                  cor(combine[combine$category == "Hand Sanitizer",]$trend,combine[combine$category == "Hand Sanitizer",]$price),
                  cor(combine[combine$category == "Disinfectant Spray",]$trend,combine[combine$category == "Disinfectant Spray",]$price),
                  cor(combine[combine$category == "Disinfectant Wipes",]$trend,combine[combine$category == "Disinfectant Wipes",]$price)
   )
   cor_names = c("N95 Mask","Hand Sanitizer","Disinfectant Spray","Disinfectant Wipes")
   
   
   spider <- plot_ly(
     type = 'scatterpolar',
     r = cor_value,
     theta = cor_names,
     fill = 'toself'
   )
   
   spider
   
 })
 output$food_spider <- renderPlotly({
   cor_value =  c(cor(combine[combine$category == "Bottled Water",]$trend,combine[combine$category == "Bottled Water",]$price),
                  cor(combine[combine$category == "Instant Noodles",]$trend,combine[combine$category == "Instant Noodles",]$price),
                  cor(combine[combine$category == "Rice",]$trend,combine[combine$category == "Rice",]$price),
                  cor(combine[combine$category == "Canned Soup",]$trend,combine[combine$category == "Canned Soup",]$price),
                  cor(combine[combine$category == "Olive Oil",]$trend,combine[combine$category == "Olive Oil",]$price)
   )
   cor_names = c("Bottled Water","Instant Noodles","Rice","Canned Soup","Olive Oil")
   
   
   spider <- plot_ly(
     type = 'scatterpolar',
     r = cor_value,
     theta = cor_names,
     fill = 'toself'
   )
   
   spider
   
   
   
 })
 
 output$personal_spider <- renderPlotly({
   cor_value =  c(cor(combine[combine$category == "Hair Color",]$trend,combine[combine$category == "Hair Color",]$price),
                  cor(combine[combine$category == "Hair Clipper",]$trend,combine[combine$category == "Hair Clipper",]$price),
                  cor(combine[combine$category == "Shampoo",]$trend,combine[combine$category == "Shampoo",]$price),
                  cor(combine[combine$category == "Trash Bag",]$trend,combine[combine$category == "Trash Bag",]$price),
                  cor(combine[combine$category == "Toilet Paper",]$trend,combine[combine$category == "Toilet Paper",]$price)
   )
   cor_names = c("Hair Color","Hair Clipper","Shampoo","Trash Bag","Toilet Paper")
   
   
   spider <- plot_ly(
     type = 'scatterpolar',
     r = cor_value,
     theta = cor_names,
     fill = 'toself'
   )
   
   spider
   
 })
 output$other_spider <- renderPlotly({
   cor_value =  c(cor(combine[combine$category == "Dumbbell",]$trend,combine[combine$category == "Dumbbell",]$price),
                  cor(combine[combine$category == "Printer Ink",]$trend,combine[combine$category == "Printer Ink",]$price),
                  cor(combine[combine$category == "Pet Supply",]$trend,combine[combine$category == "Pet Supply",]$price),
                  cor(combine[combine$category == "Nintendo Switch",]$trend,combine[combine$category == "Nintendo Switch",]$price),
                  cor(combine[combine$category == "Yoga Mat",]$trend,combine[combine$category == "Yoga Mat",]$price)
   )
   cor_names = c("Dumbbell","Printer Ink","Pet Supply","Nintendo Switch","Yoga Mat")
   
   
   spider <- plot_ly(
     type = 'scatterpolar',
     r = cor_value,
     theta = cor_names,
     fill = 'toself'
   )
   
   spider
 })
 
 
 
 output$pie <- renderPlotly({
 fig <- plot_ly(
   labels = c("Health Care", "Non-Perishable Food", "Personal Care/Household", "Others",
              "N95 Mask","Hand Sanitizer", "Disinfectant Spray", "Disinfectant Wipes",
              
              "Olive Oil", "Canned Soup", "Rice", "Instant Noodles", "Bottled Water",
              "Hair Color", "Hair Clipper", "Shampoo", "Trash Bag", "Toilet Paper",
              "Dumbbell", "Yoga Mat", "Nintendo Switch", "Pet Supply", "Printer Ink"),
   parents =c(rep("", 4),
              rep("Health Care", 4),
              rep("Non-Perishable Food", 5),
              rep("Personal Care/Household", 5),
              rep("Others", 5)),
   values = c(5, 5, 5, 5, rep(1.25,4),rep(1, 15)),
   type = "sunburst",
   branchvalues = "total",
   hoverinfo = "none"
 )
 fig
 })
 
 output$all_product <- renderPlotly({
   p <- plot_ly(y = rownames(new_data), x = colnames(new_data),
                z = new_data,
                type = "heatmap"
                # colorscale= "magma",
                #showscale = F) %>% layout(margin = list(l = 120)
   )
   p <- p %>% layout(autosize = T, title = "All Products")
   p
 })
 
 output$about <- renderImage({
   list(src = "p1.png",
        contentType = 'png',
        width = 699,
        height = 393
   ) 
 },  deleteFile = FALSE)
 
 
 output$trend_heatmap <- renderImage({
   list(src = "trend_hm.png",
        contentType = 'png',
        width = 800,
        height = 800
   ) 
 },  deleteFile = FALSE)
 
 output$food_gif <- renderImage({
   list(src = "food.gif",
        contentType = 'image/gif',
        width = 600,
        height = 500
   )
 },  deleteFile = FALSE)
 
 output$household_gif <- renderImage({
   list(src = "household.gif",
        contentType = 'image/gif',
        width = 600,
        height = 500
   )
 },  deleteFile = FALSE)
 
 
 output$health_gif <- renderImage({
   list(src = "health.gif",
        contentType = 'image/gif',
        width = 600,
        height = 500
   )
 },  deleteFile = FALSE)
 
 
 output$others_gif <- renderImage({
   list(src = "others.gif",
        contentType = 'image/gif',
        width = 600,
        height = 500
   )
 },  deleteFile = FALSE)
 
 
 output$last <- renderImage({
   
   list(src = "last.png",
        contentType = 'png',
        width = 698.4,
        height = 393
   ) 
 },  deleteFile = FALSE)
 
 
 output$price_heatmap <- renderImage({
   
   list(src = "price_hm.png",
        contentType = 'png',
        width = 800,
        height = 800
   ) 
 },  deleteFile = FALSE)
 
 output$groupheatmap1 <- renderPlotly({
  
   cor_tp <- c(cor(others_trend, healthcare_price),
               cor(unperishablefood_trend, healthcare_price),
               cor(household_trend, healthcare_price),
               cor(healthcare_trend, healthcare_price),
               cor(others_trend, household_price),
               cor(unperishablefood_trend, household_price),
               cor(household_trend, household_price),
               cor(healthcare_trend, household_price),
               cor(others_trend, unperishablefood_price),
               cor(unperishablefood_trend, unperishablefood_price),
               cor(household_trend, unperishablefood_price),
               cor(healthcare_trend, unperishablefood_price),
               cor(others_trend, others_price),
               cor(unperishablefood_trend, others_price),
               cor(household_trend, others_price),
               cor(healthcare_trend, others_price))
   
   
   price_vs_trend <- matrix(data = cor_tp, nrow = 4, ncol = 4)
   rownames(price_vs_trend) <- row_names
   colnames(price_vs_trend) <- col_names
   
   p3 <- plot_ly(y = rownames(price_vs_trend), x = colnames(price_vs_trend),
                 z = price_vs_trend,
                 type = "heatmap",
                 colors = colorRamp(c("darkseagreen1", "darkseagreen4")),
                 colorbar = list(title = "Price vs. Trend")
                 #showscale = F) %>% layout(margin = list(l = 120)
   )
   p3
   
 })
 
 output$groupheatmap_ttpp <- renderPlotly({
   
   cor_pp <- c(cor(others_price, healthcare_price),
               cor(unperishablefood_price, healthcare_price),
               cor(household_price, healthcare_price),
               cor(healthcare_price, healthcare_price),
               cor(others_price, household_price),
               cor(unperishablefood_price, household_price),
               cor(household_price, household_price),
               cor(healthcare_price, household_price),
               cor(others_price, unperishablefood_price),
               cor(unperishablefood_price, unperishablefood_price),
               cor(household_price, unperishablefood_price),
               cor(healthcare_price, unperishablefood_price),
               cor(others_price, others_price),
               cor(unperishablefood_price, others_price),
               cor(household_price, others_price),
               cor(healthcare_price, others_price))
   
   
   price_vs_price <- matrix(data = cor_pp, nrow = 4, ncol = 4)
   rownames(price_vs_price) <- row_names
   colnames(price_vs_price) <- col_names
   
   
   p1 <- plot_ly(y = rownames(price_vs_price), x = colnames(price_vs_price),
                 z = price_vs_price,
                 type = "heatmap",
                 colors = colorRamp(c("cadetblue1", "cornflowerblue")),
                 colorbar = list(title = "Price vs. Price")
                 #showscale = F) %>% layout(margin = list(l = 120)
   )
   
   
   cor_tt <- c(cor(others_trend, healthcare_trend),
               cor(unperishablefood_trend, healthcare_trend),
               cor(household_trend, healthcare_trend),
               cor(healthcare_trend, healthcare_trend),
               cor(others_trend, household_trend),
               cor(unperishablefood_trend, household_trend),
               cor(household_trend, household_trend),
               cor(healthcare_trend, household_trend),
               cor(others_trend, unperishablefood_trend),
               cor(unperishablefood_trend, unperishablefood_trend),
               cor(household_trend, unperishablefood_trend),
               cor(healthcare_trend, unperishablefood_trend),
               cor(others_trend, others_trend),
               cor(unperishablefood_trend, others_trend),
               cor(household_trend, others_trend),
               cor(healthcare_trend, others_trend))
   
   
   trend_vs_trend <- matrix(data = cor_tt, nrow = 4, ncol = 4)
   rownames(trend_vs_trend) <- row_names
   colnames(trend_vs_trend) <- col_names
   
   p2 <- plot_ly(y = rownames(trend_vs_trend), x = colnames(trend_vs_trend),
                 z = trend_vs_trend,
                 type = "heatmap",
                 colors = colorRamp(c("darkolivegreen1", "darkolivegreen")),
                 colorbar = list(title = "Trend vs. Trend")
                 #showscale = F) %>% layout(margin = list(l = 120)
   )
   subplot(p1, p2,margin = 0.08)
   
   
   
 })
 
 output$spider <- renderPlotly({
   
   cor_value =  c(cor(combine[combine$group == "Health Care",]$trend,combine[combine$group == "Health Care",]$price),
                  cor(combine[combine$group == "Non-Perishable Food",]$trend,combine[combine$group == "Non-Perishable Food",]$price),
                  cor(combine[combine$group == "Personal Care/Household",]$trend,combine[combine$group == "Personal Care/Household",]$price),
                  cor(combine[combine$group == "Others",]$trend,combine[combine$group == "Others",]$price)
   )
   cor_names = c("Health Care","Non-Perishable Food","Personal Care/Household","Others")
   
   
   spider <- plot_ly(
     type = 'scatterpolar',
     r = cor_value,
     theta = cor_names,
     fill = 'toself'
   )
   
   spider
 })
 
 output$group_scatter <- renderPlotly({
   fig1 =  combine %>%
     plot_ly( x = ~trend, 
              y = ~price, type = 'scatter',color = ~group, mode = 'markers')%>% layout (
                xaxis = list(title = "Rescaled Google Search Trend - US"),
                yaxis = list(title = "Standardized Price"))
   fig1
 })
 
 output$group_scatter_move <- renderPlotly({
   p <- combine %>%
     plot_ly(
       x = ~trend, 
       y = ~price, 
       #size = ~pop, 
       color = ~group, 
       frame = ~date, 
       #text = ~country, 
       hoverinfo = "text",
       type = 'scatter',
       mode = 'markers'
       ) %>% layout (
       xaxis = list(title = "Rescaled Google Search Trend - US"),
       yaxis = list(title = "Standardized Price")
       )  %>% animation_opts(
       frame = 100,
       transition = 10,
       redraw = FALSE
     )
   p
 })
 
 
 

 output$product_scatter <- renderPlotly({
   if(input$category == "Overall") return()
  fig1 =  combine[combine$group == input$category,] %>%
    plot_ly( x = ~trend, 
             y = ~price, type = 'scatter',color = ~category, mode = 'markers') %>% layout (
            xaxis = list(title = "Rescaled Google Search Trend - US"),
             yaxis = list(title = "Standardized Price"))
  fig1
}) 
 
 output$gif <- renderImage({ 
  list(src = "gganim.gif",
       contentType = 'image/gif',
       width = 825,
       height = 550
      ) 
},  deleteFile = FALSE)

}

