# Data Import
data("AirPassengers")
# This data in Time series format mentioned as Time-series
View(AirPassengers)

# This is start of dataset as from Jan 1949
start(AirPassengers)

# This is end of dataset 
end(AirPassengers)

# This is frequency of dataset
frequency(AirPassengers)

# The cycle of this time series is 12 month
summary(AirPassengers)

# detail Matrics
# The number of passengers are distributed this will plot the time series
plot(AirPassengers, col="red")

# This will fit a Regression LIne for AR=Auto Regression
abline(reg = lm(AirPassengers ~ time(AirPassengers)))

# MOre operation on data set

cycle(AirPassengers)

# this will aggregate the cycle and display a year
plot(aggregate(AirPassengers,FUN = mean), col="green")

# This box plot across the month give us sense of seasonal pattern 
boxplot(AirPassengers ~ cycle(AirPassengers), col= 'Yellow')

## boxplot Inferences
# in mnth of June - August seasonality efect can easily idenify
# n become larger in AR model



plot(aggregate(log(AirPassengers) ), col='red')
install.packages('tseries')
library(tseries)

# Augmented Dickey-Fuller Test
adf.test(diff((AirPassengers)), alternative = 'stationary', k=0)
adf.test(diff(log(AirPassengers)), alternative = 'stationary', k=0)

# we know that we need to adress two issues before we test stationary series
# we need to remove unequal variance, we do using log of the series
# we need t adress the trend components
plot(diff(log(AirPassengers)))

library(tseries)
adf.test(diff(log(AirPassengers)), alternative = c("stationary","explosive"),k=0)

# Next step is to find right parameter to be used in ARIMA Model
# We already know the 'd' component is 1 as we need 1 difference 
# we do this using coorelation plot

# AR I MA
acf(AirPassengers)

acf(diff(log(AirPassengers)))   # Determine the value of q
pacf(diff(log(AirPassengers)))   # Determine the value of p
plot(diff(log(AirPassengers)))




# Let us fit an ARIMA Model and predcit future for 10 Years
#c(p,q,d)

fit <- arima(log(AirPassengers),c(0,1,1),seasonal = list(order=c(0,1,1),period=12))
pred <- predict(fit, n.head = 10*12)
ts.plot(AirPassengers, 2.718^pred$pred , log='y', lty = c(1,3))


#Testing our Model

datawide <- ts(AirPassengers, frequency = 12 , start = c(1949,1), end = c(1959,12))
