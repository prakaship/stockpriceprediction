
library(shiny)
comp<-read.csv("company.csv")
shinyUI(pageWithSidebar(
headerPanel("stock price prediction"),
  sidebarPanel(
    
    selectInput("sin",label="company name",choices = comp[,1]),
    radioButtons("dat",label = "choose the predictors",choices =c( "date"="1")),
    checkboxGroupInput("predictors",label = "",
                       choices =c("Return On Asset"="2","profit margin"="3") ),
   submitButton(text="Apply")
  ),
  mainPanel(
    tabsetPanel(
      
    tabPanel("Data set",
             h4("Stock Data"),
             dataTableOutput("data_table")
    ),  
    tabPanel("Prediction",  
   plotOutput("graph"),
   h3("The next day high stock  price is :"),
   h1(textOutput("highvar")),
   h3("The next day low stock  price is :"),
   h1(textOutput("lowvar"))
    )
    )
  )
)
  
)