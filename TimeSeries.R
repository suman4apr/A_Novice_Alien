library(caret)
library(forecast)
library(fpp2)
library(ggstatsplot)
library(readxl)
library(dplyr)
library(Rcpp)
library(ggplot2)
library(ggfortify)
library(zoo)
library(fUnitRoots)

df= read.csv("C:\\Users\\Username\\company\\OneDrive_Data\\OneDrive\\BI\\Learn\\R\\TimeSeries\\UNRATE.csv")
str(df)
head(df)
summary(df)

mape <- function(actual,pred){
  mape <- mean(abs((actual - pred)/actual))*100
  return (mape)
}

#split data
index = createDataPartition(df$UNRATE, p = 0.80, list = FALSE)
train = df[index, ]
test = df[-index, ]
nrow(train)
nrow(test)

tail(train)
head(test)

df_ts <- ts(train$UNRATE,start = c(1948,1), end = c(2007, 1), frequency = 12)
class(df_ts)

autoplot(df_ts) +
  ggtitle("Unemployment Rate") +
  xlab("Year") +
  ylab("Unemployment Rate")

#check for seasonality pattern
ggseasonplot(df_ts, Date=TRUE, Date.labels.left=TRUE) +
  ylab("UNRATE%") +
  ggtitle("Seasonal plot: Un-Employment Rate")

##check for autocorrelation using correlogram
acf(df_ts)
pacf(df_ts)

autoplot(decompose(df_ts))+theme(plot.title = element_text(size = 8))

##Stationarity Test
adfTest(df_ts)

# #decompose & get the estimated values of the seasonal component
# df_series<- decompose(df_ts)
# df_series$seasonal 
# #The estimated seasonal factors are given for the months January-December, 
# #and are the same for each year. The largest seasonal factor is for Oct 
# #(about 0.13), and the lowest is for Aug (about -0.06), indicating 
# #that there seems to be a peak in unemployment rate in July and 
# #a trough in births in April each year.
#plot(df_series)

# #Seasonally Adjusting
# df_series_seasonal_adjust<- df_ts-df_series$seasonal
# plot(df_series_seasonal_adjust)

# #Check seasonality removed or not
# autoplot(decompose(df_series_seasonal_adjust))+theme(plot.title = element_text(size = 8))
#plot.ts(df_series_seasonal_adjust)

# #Making stationary after transformation
# acf(df_series_seasonal_adjust)
# pacf(df_series_seasonal_adjust)
# dat_tsdiff1 <- diff(df_series_seasonal_adjust, differences=1)
# plot.ts(dat_tsdiff1)

df_tsdiff1 <- diff(df_ts, differences=1)
plot.ts(df_tsdiff1)

#check for seasonality pattern
ggseasonplot(df_tsdiff1, Date=TRUE, Date.labels.left=TRUE) +
  ylab("UNRATE%") +
  ggtitle("Seasonal plot: Un-Employment Rate")

#Validate data is stationary
adfTest(df_tsdiff1)
acf(df_tsdiff1)
pacf(df_tsdiff1)
# acf(diff(df_series_seasonal_adjust, differences = 1)) #q=1
# pacf(diff(df_series_seasonal_adjust, differences = 1)) #p=0

model= auto.arima(df_ts, seasonal=FALSE,stepwise=FALSE, approximation=FALSE)
summary(model)

fore_arima = forecast::forecast(model, h=175)
print(fore_arima)
df_arima = as.data.frame(fore_arima)
head(df_arima)
test$arima <- df_arima$`Point Forecast`
mape(test$UNRATE, test$arima)



