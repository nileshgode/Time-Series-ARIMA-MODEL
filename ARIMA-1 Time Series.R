# get the online data
data = read.csv('http://ucanalytics.com/blogs/wp-content/uploads/2015/06/Tractor-Sales.csv')

#  chart has an upward trend for tractors sales and there is also a seasonal 
# component that we have already analyzed an earlier article on time series decomposition.
data = ts(data[,2],start = c(2003,1),frequency = 12)
plot(data, xlab='Years', ylab = 'Tractor Sales')

# This to remove the upward trend through 1st order differencing the series
#  the above series is not stationary on variance

plot(diff(data),ylab='Differenced Tractor Sales')

# this series is not stationary on mean since we are using the original data without differencing. 
plot(log10(data),ylab='Log (Tractor Sales)')


# now this series looks stationary on both mean and variance. 
plot(diff(log10(data)),ylab='Differenced Log (Tractor Sales)')     

# create autocorrelation factor (ACF) and partial autocorrelation factor (PACF) plots to identify patterns
par(mfrow = c(1,2))
acf(ts(diff(log10(data))),main='ACF Tractor Sales')
pacf(ts(diff(log10(data))),main='PACF Tractor Sales')

# Identification of best fit ARIMA model
install.packages('forecast')
library(forecast)
require(forecast)
ARIMAfit = auto.arima(log10(data), approximation=FALSE,trace=FALSE)
summary(ARIMAfit)

# The next step is to predict tractor sales for next 3 years i.e. for 2015, 2016, and 2017 through the above model.
par(mfrow = c(1,1))
pred = predict(ARIMAfit, n.ahead = 36)
pred
plot(data,type='l',xlim=c(2004,2018),ylim=c(1,1600),xlab = 'Year',ylab = 'Tractor Sales')
lines(10^(pred$pred),col='blue')
lines(10^(pred$pred+2*pred$se),col='orange')
lines(10^(pred$pred-2*pred$se),col='orange')

# create an ACF and PACF plot
par(mfrow=c(1,2))
acf(ts(ARIMAfit$residuals),main='ACF Residual')
pacf(ts(ARIMAfit$residuals),main='PACF Residual')

