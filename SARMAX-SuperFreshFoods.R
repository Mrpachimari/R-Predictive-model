# SARMAX - SuperFreshFoods
rm(list = ls())
setwd("/Users/lianghao/Desktop/求职文件/resume投递版/投递用的简历和cover letter/Petco Analytics Exercise")
library(tseries)
library(forecast)
# import data
data = read.csv("SuperFreshFood.csv")
Email = data[-c(1:31),6]
Holidays = data[-c(1:31),7]
Telecast = data[-c(1:31),8]
data = na.omit(data)
data = data[,-1]
str(data)
data[,8] = data$Repeat.Orders / data$Orders
colnames(data)[8] = "Repeat.ratio"
data = ts(data,frequency = 7)
str(data)
# draw time series
plot(data[,"Orders"])
plot(data[,"Shipments"])
plot(data[,"New.Customers"])
plot(data[,"Repeat.Orders"])

# Orders Forecast
# Consider Email as xreg parameter
orders_fit <- auto.arima(data[, "Orders"], xreg = data[,"Emial"], seasonal = TRUE)
orders_forecast <- forecast(orders_fit, h = 31, xreg = Email) 
plot(data[,"Orders"])
plot(orders_forecast)

# Shipments Forecast
# Consider Orders and Holidays as xreg parameter
plot(decompose(data[, "Shipments"], type = "additive"))
XM = data.matrix(data[,c(1,6)])
XM2 = data.matrix(data.frame(orders_forecast$mean, ts(Holidays,frequency = 7)))
colnames(XM2) = colnames(XM)
shipments_fit <- auto.arima(data[,"Shipments"], xreg = XM, seasonal = TRUE)
shipments_forecast <- forecast(shipments_fit, h = 31, xreg = XM2)
Holidays <- ts(Holidays, start = c(5, 4), end = c(9, 6), frequency = 7)
shipments_forecast$mean <- ts(ifelse(Holidays == 1, 0, shipments_forecast$mean),
                              start = c(5, 4), end = c(9, 6), frequency = 7 )
for (i in 1:2) {
  shipments_forecast$lower[,i] <- ts(ifelse(Holidays == 1, 0, shipments_forecast$lower[,i]),
                                     start = c(5, 4), end = c(9, 6), frequency = 7) 
  shipments_forecast$upper[,i] <- ts(ifelse(Holidays == 1, 0, shipments_forecast$upper[,i]),
                                     start = c(5, 4), end = c(9, 6), frequency = 7)
}
plot(shipments_forecast) 

# New.Customers Forecast
# Assuming that new customer acquisition is not affected by other variables
# so we do not use the xreg parameter
plot(decompose(data[, "New.Customers"], type = "additive"))
# new_customers_fit <- auto.arima(data[, "New.Customers"], xreg = data[,"Telecast"],seasonal = TRUE)
# new_customers_forecast <- forecast(new_customers_fit, h = 31, xreg = Telecast)
new_customers_fit <- auto.arima(data[, "New.Customers"],seasonal = TRUE)
new_customers_forecast <- forecast(new_customers_fit, h = 31)
plot(new_customers_forecast)

# Repeat.Orders Forecast
# Consider Orders as xreg parameter
repeat_orders_fit <- auto.arima(data[, "Repeat.Orders"], xreg = data[, "Orders"], seasonal = TRUE)
repeat_orders_forecast <- forecast(repeat_orders_fit, h = 31, xreg = orders_forecast$mean) 
plot(repeat_orders_forecast)

# Print the result of forecast 
print(orders_forecast)
print(shipments_forecast)
print(new_customers_forecast)
print(repeat_orders_forecast)

# write.csv(print(orders_forecast),"Orders_forecast.csv")
# write.csv(print(shipments_forecast),"Shipments_forecast.csv")
# write.csv(print(new_customers_forecast),"New_customers_forecast.csv")
# write.csv(print(repeat_orders_forecast),"Repeat_orders_forecast.csv")
