library(quantmod)
library(tseries)
library(timeSeries)
library(forecast)


getSymbols("NVDA", from = "2021-01-01", to = "2023-10-01")
nrow(NVDA)

chartSeries(NVDA, subset = 'last 6 months', type = 'auto')
addBBands()

# Bollinger Bands are often used to identify overbought and oversold conditions. 
# When prices touch or move outside the upper band, it may indicate an overbought condition, 
# and when prices touch or move outside the lower band, it may suggest an oversold condition.

# Assign columns
Open_Prices = NVDA[,1]
High_Prices = NVDA[,2]
Low_Prices = NVDA[,3]
Close_Prices = NVDA[,4]
Volume_Prices = NVDA[,5]
Adjusted_Prices = NVDA[,6]

par(mfrow = c(2, 3))

plot(Open_Prices)
plot(High_Prices)
plot(Low_Prices)
plot(Close_Prices)
plot(Volume_Prices)
plot(Adjusted_Prices)

Predict_Price = Close_Prices

# Find linear relationship
par(mfrow = c(1, 2))
Acf(Predict_Price, main = "Autocorrelation")
Pacf(Predict_Price, main = "Partial Autocorrelation")
Acf_Values <- Acf(Predict_Price, plot = FALSE)
Acf_Values
Pacf_Values <- Pacf(Predict_Price, plot = FALSE)
Pacf_Values

# Dickey-Fuller should be >0.5 and negative
print(adf.test(Predict_Price))

# Prediction of return using Arima
Return_NVDA <- 100*diff(log(Predict_Price))

NVDA_Return_Train <- Return_NVDA[1:(0.9*length(Return_NVDA))]

NVDA_Return_Test <- Return_NVDA[(0.9*length(Return_NVDA)+1):length(Return_NVDA)]

auto.arima(NVDA_Return_Train, seasonal = FALSE)

fit <- Arima(NVDA_Return_Train, order = c(1, 1, 1))
fit

predict <- predict(fit, n.ahead = (length(Return_NVDA) - (0.9*length(Return_NVDA))))$pred
predict

# Forecast for 15 days
test_forecast <- forecast(fit, h = 100)
test_forecast

par(mfrow = c(1,1))
plot(test_forecast, main = "Arima forecast for NVIDIA stock")

# Accuracy
accuracy(predict, NVDA_Return_Test)

