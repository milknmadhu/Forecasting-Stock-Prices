#Import Data from Yahoo Finance
library(quantmod)
Stock_Symbols<- c("MSFT", "AAPL", "FB", "GOOG")
Stock_Close_Price <- NULL

for(i in Stock_Symbols){
  df1 <- getSymbols(i, from = "2016-01-01",
                    to = Sys.Date(), auto.assign = FALSE)
  Close <- df1[,4] 
  Stock_Close_Price <- cbind(Stock_Close_Price, Close) 
}
Stock_Close_Price
TS<-ts(Stock_Close_Price, frequency = 260)
microsoft<-TS[,1]
apple<-TS[,2]
facebook<-TS[,3]
google<-TS[,4]

#########################################################################################################
#########################################################################################################
#########################################################################################################
#########################################################################################################
##App Codes
library(shiny)
library(shinydashboard)

ui <- dashboardPage(
  dashboardHeader(title = "Forecasting Stock Price"),
  dashboardSidebar(selectInput("company", "Select Company Name",
                               list( "Microsoft", "Apple", "Facebook", "Google"
                                     
                               )),
                   numericInput("ahead", "Select number of days to be forecasted", 260),
                   
                   submitButton("Update Prediction"),
                   br(),br(),br(),
                   dateInput("date",
                             label = "Enter date after current date up to which forecating is needed.",
                             value = Sys.Date() + 10, format = "yyyy-mm-dd"),
                   submitButton("Get Point Predictions")
  ),
  dashboardBody(
    h3(textOutput("caption")),
    fluidRow(
      box(plotOutput("arimaForecastPlot")),
      box(dataTableOutput("Predictiontable"))
    )
  )
)

library(tidyr)
library(forecast)
server <- function(input, output, session) {
  
  getDataset <- reactive({
    if (input$company=="Microsoft")
    {return(microsoft)}
    if (input$company=="Apple")
    {return(apple)}
    if (input$company=="Facebook")
    {return(facebook)}
    if (input$company=="Google")
    {return(google)}
  })
  
  getDays<-reactive({as.numeric(round(returnValue(extract_numeric(data.frame(difftime(as.Date(input$date), as.Date(Sys.Date()),
                                                                                      units = c("days")))))))
  })
  
  output$caption <- renderText({
    paste("Company:", input$company)
  })

  output$arimaForecastPlot <- renderPlot({
    f <- forecast(auto.arima(getDataset(),D=1),h=getDays(), level=95)
    plot(f)
  })
  
  output$Predictiontable<-renderDataTable({
    f <- forecast(auto.arima(getDataset(),D=1),h=getDays(), level=95)
    
    format(returnValue(data.frame(f)), digits = 2, nsmall  = 2)
    
  })
}

shinyApp(ui, server)
