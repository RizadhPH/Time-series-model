
# UI
library(shiny)
data <- read.csv("..../Retail_Sales_Adjusted.csv")
Y <- ts(data[,2],start = c(1992,1), frequency = 12)
# Define UI 
ui <- shinyUI(
  pageWithSidebar(
  
  # Application title
  headerPanel("Timeseries Forecasting"),
  
  # Sidebar with controls to select the dataset and forecast ahead duration
  sidebarPanel(
    
    numericInput("ahead", "Months to Forecast Ahead:", 24),
    
    submitButton("Update View")
  ),
  
  # Show the caption and forecast plots
  mainPanel(
    h3(textOutput("caption")),
    tabsetPanel(
       
      tabPanel("Arima Forecast", plotOutput("arimaForecastPlot"))
      
    )
  )
))

# Server
library(shiny)
library(forecast)
library(fpp2)
library(ggplot2)


server <- shinyServer(function(input, output) {
  
    output$arimaForecastPlot <- renderPlot({
    fit <- auto.arima(Y,d=1,D=1,stepwise = FALSE,approximation = FALSE,trace = TRUE)
    autoplot(forecast(fit, h=input$ahead))
  })
  
})

# Call the app
shinyApp(ui = ui, server = server)

# Note: It takes time to give the initial output and to give the updated plot, since the model runs through all combinations to find the best fit.



