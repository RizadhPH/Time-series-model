##############################################################################################
# Retail Sales Regression Model. 
# Forecast for next two years sales per day
##############################################################################################

# Load the forecasting package
library(fpp2)
library(ggplot2)

#Load the data
data <- read.csv("....../Retail_Sales_Adjusted.csv")

# Declare the sales column as time series 
Y <- ts(data[,2],start = c(1992,1), frequency = 12)

##########################################################################################
# Initial Analysis
##########################################################################################
# Time plot
autoplot(Y) + 
  ggtitle("Time Plot")+
  ylab("Sales in Millions")

# Major components of a time series are Cyclic + Noise + Trend + Seasonality 

# Taking the year on year difference can handle the trend 
DY <- diff(Y)

# Time plot for time series component after handling for trend
autoplot(DY) + 
  ggtitle("Time Plot on difference")+
  ylab("Millions in sales")
# No more trends now, so we can see if there is Seasonality now

# Seasonal subseries to understand the seasonality
ggsubseriesplot(DY)+
  ggtitle("Sesonal Plot change in daily retail sales")+
  ylab("Millions in sales")

# Yes, there is seasonality; one of the major instances is.. 
# when we can see the sales is high on december and low on January.

#########################################################################
# Forecast with various method
#########################################################################
# Time Series Models 

# Model 1 - Seasonal Naive method
model1 <- snaive(DY)  
print(summary(model1))
summary(DY)
checkresiduals(model1)
#Notable Result: Residual Standard Deviation  =  287.0647

# Model 2 - Exponential smothing model
model2 <- ets(Y) #sigma:  218.8133
print(summary(model2))
checkresiduals(model2)
#Notable Result: Sigma(Residual Standard Deviation)  =  218.8133

# Model 3 - ARIMA Model
model3 <- auto.arima(Y,d=1,D=1,stepwise = FALSE,approximation = FALSE,trace = TRUE) 
print(summary(model3))
checkresiduals(model3)
# sigma^2 estimated as 39129 and hence Sigma (Residual Standard Deviation) = 197.81
# Hence we can conclude that among these three model, ARIMA model is the better candidate for forecasting

#Forecast - using ARIMA
fcst <- forecast(model3,h=24)
autoplot(fcst,include=120)
print(summary(fcst))



