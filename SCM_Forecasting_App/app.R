getwd()
# setwd("SCM_Forecasting_App")
# getwd()



library(tidyverse)
library(lubridate)
library(party)
library(rpart)
library(randomForest)
library(caret)


# install.packages("readr")
library(readr)

library(shiny)
# install.packages("shinythemes")
library(shinythemes)
# install.packages("shinyWidgets")
library(shinyWidgets)

## The package DT provides an R interface to the JavaScript library DataTables with filtering, pagination, sorting...
# install.packages("DT")
library(DT)

# install.packages("shinyjs")
library(shinyjs)

## The rsconnect package deploys applications to the shinyapps.io service.
# install.packages("rsconnect")
library(rsconnect)



## Read saved dataset:
load("salesAug.trainData_tbl.Rda")



## Read tranined models:
rpartModel <- readRDS("topSeller_rpart.Rds")
# ctreeModel <- readRDS("topSeller_ctree.Rds")
lmModel <- readRDS("qtyAug_lm.Rds")
regTreeModel <- readRDS("qtyAug_regTree_rpart.Rds")


# count(salesAug.trainData$goods_season)

## Make lists for input selections:
## goods_season definitions:
## 1 = "Spring", 2 = "Summer", 3 = "Fall", 4 = "Winter", 5 = "Spring-Summer", 6 = "Spring-Fall", 7 = "Spring-Winter",
## 8 = "Summer-Fall", 9 = "Sunmmer-Winter", 10 = "Fall-Winter", 0 = "All-Season"

# goodSeasons <- as.list(c("All-Season", "Spring", "Summer", "Fall", "Winter", "Spring-Summer"))
# goodSeasons

# goodSeasons <- data.frame("goods_season" = c(0, 1, 2, 3, 4, 5),
#                           "description" = c("All-Season", "Spring", "Summer", "Fall", "Winter", "Spring-Summer"))


goodSeasons <- as.list(unique(salesAug.trainData$goods_season))

goodSeasons
str(goodSeasons)
goodSeasons[1]

levels(salesAug.trainData$top_selling_goods[1])

# salesAug.trainData %>% 
#   filter(top_selling_goods == as.factor(factor(c(1), levels = c(0, 1)))) %>% 
#   View()

salesAug.trainData$goods_season[1]
str(salesAug.trainData$goods_season[1])
salesAug.trainData$goods_season[2]
str(salesAug.trainData$goods_season[2])
salesAug.trainData$goods_season[10]
str(salesAug.trainData$goods_season[10])

str(salesAug.trainData$goods_season)

## -------------- Shiny UI --------------

## Create fields needed in the prediction models to display in the saved input table:
field <- c("goods_price", "orginal_shop_price", "goods_season", 
           "goods_click", "cart_click", "favorites_click", "onsale_days")
field


ui <- fluidPage(
  
  ## Enable Shinyjs's feature to reset input elements to their original values:
  ## shinyjs must be initialized with a call to useShinyjs() in the app's ui.
  useShinyjs(),
  
  div(
    id = "form",
    
  ## Use Shiny theme:
  theme = shinytheme("cerulean"),
  
  ## App title:
  titlePanel("Ecommerce Top Selling Product Prediction and Demand Forecasting"),
  
  ## Sidebar layout with input and output definitions:
  sidebarLayout(
    
    ## Sidebar panel for inputs:
    sidebarPanel(
      
      numericInput(
        inputId = "goods_price",
        label = "Enter the sales price of the product",
        value = 0.99,
        min = 0.00,
        max = 5000.00
      ),
      numericInput(
        inputId = "orginal_shop_price",
        label = "Enter the original marked price of the product",
        value = 1.99,
        min = 0.00,
        max = 5000.00
      ),
      selectInput(
        inputId = "goods_season",
        label = "Select the season that the product belongs to",
        choices = goodSeasons
      ),
      numericInput(
        inputId = "goods_click",
        label = "Enter the number of user clicks of the product",
        value = 0,
        min = 0
      ),
      numericInput(
        inputId = "cart_click",
        label = "Enter the number of Add to Cart clicks of the product",
        value = 0,
        min = 0
      ),
      numericInput(
        inputId = "favorites_click",
        label = "Enter the number of clicks of Add to Your Favorites",
        value = 0,
        min = 0
      ),
      numericInput(
        inputId = "onsale_days",
        label = "Enter the number of onsale days of the product",
        value = 0,
        min = 0
      ),
      
      checkboxInput("showModel1", "Show/Hide Model 1", value = TRUE),
      checkboxInput("showModel2", "Show/Hide Model 2", value = TRUE),
      checkboxInput("showModel3", "Show/Hide Model 3", value = TRUE),
      
      actionButton(
        inputId = "run",
        label = "Run Prediction"
      ),
      br(),
      br(),
      actionButton(
        inputId = "save",
        label = "Add Input to Table"
      ),
      br(),
      br(),
      actionButton(
        inputId = "resetData",
        label = "Reset Input"
      )
      
    ),
    
    ## Main panel for displaying a distribution plot of as output:
    mainPanel(
      
      mainPanel(
        div(
        DT::dataTableOutput("datasetInput", width = "60%"), tags$hr(), style = "font-size:90%"
        ),   ## Adds a horizontal line (e.g., horizontal rule) via HTML tag.
        
        div(
          fluidRow(
            ## Display prediction results:
            # column(12, h1(span(textOutput("pred"), style = "color: blue")))
            h3("Top Seller Classification Prediction from Model 1 - Decision Tree:"),
            textOutput("pred1"),
            h3("Demand Prediction from Model 2 - Multiple Linear Regression:"),
            textOutput("pred2"),
            h3("Demand Prediction from Model 3 - Regression Tree:"),
            textOutput("pred3")
          )
        )
        
      )
      
    )
    
  )
  
  )   ## End of div
)




## -------------- Shiny Server --------------

server <- function(input, output, session) {

  ## ------------ Method 1: Save single input session ------------
  
  ## Decision Tree Model:
  prediction1 <- reactive({

    # is.double(salesAug.trainData$goods_price)
    # is.double(salesAug.trainData$orginal_shop_price)

    goods_price <- as.double(input$goods_price)
    orginal_shop_price <- as.double(input$orginal_shop_price)

    ## Flatten list-like objects (the inputs) into vector-like objects. Input from selections is factor already.
    goods_season <- unlist(input$goods_season)
    levels(goods_season) <- levels(salesAug.trainData$goods_season)

    goods_click <- input$goods_click
    cart_click <- input$cart_click
    favorites_click <- input$favorites_click
    onsale_days <- input$onsale_days

    inputDataset <- data.frame(goods_price, orginal_shop_price, goods_season,
                               goods_click, cart_click, favorites_click, onsale_days) %>%
      as_tibble()

    # print(str(salesAug.trainData))
    print(str(inputDataset))   ## Print the data struture to check if the input data has same data type as the training dataset in order to fit in the model.
    
    print("rpart - input")   
    print(inputDataset$goods_season)
    

    ## Predict input data with the rpart Decision Tree model:
    inputTopSellerPred <- predict(rpartModel, newdata = inputDataset, type = "class")
    print(str(inputTopSellerPred))
    print(" ")
    print(inputTopSellerPred)
    print(class(inputTopSellerPred))
    
    # ## The ctree() Decision Tree size is beyond the Shiny memeory limitation of 1GB. 
    # ## Can't publish online with free plan but work fine locally.
    # inputTopSellerPred <- predict(ctreeModel, newdata = inputDataset, type = "response")
    # # print(inputTopSellerPred)
    # # print(as.character(factor(inputTopSellerPred)))
    # 
    
    inputTopSellerPred <- as.character(factor(inputTopSellerPred))
    print(inputTopSellerPred)  ## Print the prediction value in console.
    print(class(inputTopSellerPred))   ## Print prediction result type in console for debugging.


    if (inputTopSellerPred == "0") {
      print("NOT a Top Selling Product. Try something else.")
    }
    else if (inputTopSellerPred == "1") {
      print("Projected as one of the TOP SELLING PRODUCTS of the year!!")
    }
    else {
      print("Error.")
    }
    
    
    

  }

  )

  
  

  ## Multiple Linear Regression Model:
  prediction2 <- reactive({

    # is.double(salesAug.trainData$goods_price)
    # is.double(salesAug.trainData$orginal_shop_price)

    goods_price <- as.double(input$goods_price)
    orginal_shop_price <- as.double(input$orginal_shop_price)

    ## Flatten list-like objects (the inputs) into vector-like objects. Input from selections is factor already.
    goods_season <- unlist(input$goods_season)
    levels(goods_season) <- levels(salesAug.trainData$goods_season)

    goods_click <- input$goods_click
    cart_click <- input$cart_click
    favorites_click <- input$favorites_click
    onsale_days <- input$onsale_days

    inputDataset <- data.frame(goods_price, orginal_shop_price, goods_season,
                               goods_click, cart_click, favorites_click, onsale_days) %>%
      as_tibble()

    # print(str(salesAug.trainData))
    print(str(inputDataset))   ## Print the data struture to check if the input data has same data type as the training dataset in order to fit in the model.

    # levels(inputDataset$goods_season) <- levels(salesAug.trainData$goods_season)

    print("rpart - LM - input")
    print(inputDataset$goods_season)


    ## Predict input data:
    predict(lmModel, newdata = inputDataset)

    print("LM Prediction: ")
    print(predict(lmModel, newdata = inputDataset))   ## Print prediction result in console for debugging.

  }

  )




  ## Regression Tree Model:
  prediction3 <- reactive({

    # is.double(salesAug.trainData$goods_price)
    # is.double(salesAug.trainData$orginal_shop_price)

    goods_price <- as.double(input$goods_price)
    orginal_shop_price <- as.double(input$orginal_shop_price)

    ## Flatten list-like objects (the inputs) into vector-like objects. Input from selections is factor already.
    goods_season <- unlist(input$goods_season)
    levels(goods_season) <- levels(salesAug.trainData$goods_season)

    goods_click <- input$goods_click
    cart_click <- input$cart_click
    favorites_click <- input$favorites_click
    onsale_days <- input$onsale_days

    inputDataset <- data.frame(goods_price, orginal_shop_price, goods_season,
                               goods_click, cart_click, favorites_click, onsale_days) %>%
      as_tibble()

    # print(str(salesAug.trainData))
    print(str(inputDataset))   ## Print the data struture to check if the input data has same data type as the training dataset in order to fit in the model.

    # levels(inputDataset$goods_season) <- levels(salesAug.trainData$goods_season)

    print("rpart -regTree - input")
    print(inputDataset$goods_season)

    ## Predict input data:
    predict(regTreeModel, newdata = inputDataset)

    print(predict(regTreeModel, newdata = inputDataset))   ## Print prediction result in console for debugging.

  }

  )

  
  
  
  output$pred1 <- renderText({
    if (input$run == 0) {
      return()
    }   ## Prevent the function from running automatically unless the Run button is clicked.
    else {
      prediction1()
    }

  }
  )
  
  
  output$pred2 <- renderText({
    if (input$run == 0) {
      return()
    }   ## Prevent the function from running automatically unless the Run button is clicked. 
    else {
      prediction2()
    }
    
  }
  )
  
  
  output$pred3 <- renderText({
    if (input$run == 0) {
      return()
    }   ## Prevent the function from running automatically unless the Run button is clicked. 
    else {
      prediction3()
    }
    
  }
  )
 
  
   
  
  ## ------------ Method 2: Save all previous input session ------------


  saveData <- function(userData) {
    ## t() function transposes a matrix or data.frame:
    userData <- as.data.frame(t(userData))
    if(exists("datasetInput")) {
      datasetInput <<- rbind(datasetInput, userData)
    }
    else {
      datasetInput <<- userData
    }
  }

  loadData <- function() {
    if(exists("datasetInput")) {
      datasetInput
    }
  }

  ## User the formData reactive function to aggregate all form data once a field is filled:
  formData <- reactive({
    userData <- sapply(field, function(x) input[[x]])
    userData
  })

  observeEvent(input$save, {
    saveData(formData())
  })


  # Show the previous datasetInput & update with current datasetInput when save is clicked:
  output$datasetInput <- DT::renderDataTable({
    input$save
    loadData()
  })

  
  ## Reset user input:
  observeEvent(input$resetData, {
      reset("form")
    }
  )

}


# rm(datasetInput)
# gc()


## Launch the app:
shinyApp(ui = ui, server = server)


####################################


# ## Remove all data to release memory:
# rm(ctreeModel, rpartModel, lmModel, regTreeModel, datasetInput, goodSeasons, salesAug.trainData, field)
# gc()