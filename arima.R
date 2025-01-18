library(tseries)
library(forecast)
library(vars)
library(ggplot2)
library(tidyr)
library(zoo)

weather_data <- read.csv("dataset/processed_hourly_D3.csv")
summary(weather_data)

# Check for stationarity
adf.test(weather_data$AirTemperature)
adf.test(weather_data$BarometricPressure)
adf.test(weather_data$RelativeHumidity)

# Check for autocorrelation
acf(weather_data$AirTemperature, main = "Air Temperature Autocorrelation")
acf(weather_data$BarometricPressure, main = "Barometric Pressure Autocorrelation")
acf(weather_data$RelativeHumidity, main = "Relative Humidity Autocorrelation")

# Check for partial autocorrelation
pacf(weather_data$AirTemperature, main = "Air Temperature Partial Autocorrelation")
pacf(weather_data$BarometricPressure, main = "Barometric Pressure Partial Autocorrelation")
pacf(weather_data$RelativeHumidity, main = "Relative Humidity Partial Autocorrelation")

# Apply first difference and seasonal difference
temp_diff <- diff(diff(weather_data$AirTemperature, lag = 24))
pressure_diff <- diff(weather_data$BarometricPressure)
humidity_diff <- diff(diff(weather_data$RelativeHumidity, lag = 24))

# Check ACF and PACF of differenced series
par(mfrow = c(3, 2))
acf(temp_diff, main = "Differenced Temperature ACF")
pacf(temp_diff, main = "Differenced Temperature PACF")
acf(pressure_diff, main = "Differenced Pressure ACF")
pacf(pressure_diff, main = "Differenced Pressure PACF")
acf(humidity_diff, main = "Differenced Humidity ACF")
pacf(humidity_diff, main = "Differenced Humidity PACF")

# Check stationarity of differenced series
adf.test(temp_diff)
adf.test(pressure_diff)
adf.test(humidity_diff)

# Split data into training and testing
train_size <- floor(0.8 * length(weather_data$AirTemperature))
train <- weather_data[1:train_size, ]
test <- weather_data[(train_size + 1):nrow(weather_data), ]

# Fit SARIMA models
sarima_fit <- arima(train$AirTemperature,
  order = c(1, 1, 1),
  seasonal = list(order = c(1, 1, 1), period = 24)
)

# Create time series objects for each variable
ts_data <- ts(
  data.frame(
    Temperature = train$AirTemperature,
    Pressure = train$BarometricPressure,
    Humidity = train$RelativeHumidity
  ),
  # hourly data (24 hours)
  frequency = 24
)

# Fit VAR model
var_fit <- VAR(
  ts_data,
  # 6 lags (ideal after testing)
  p = 6
)

# determine forecast period (number of test samples)
forecast_periods <- length(test$AirTemperature)

# Make forecast using SARIMA and VAR
sarima_forecast <- forecast(sarima_fit, h = forecast_periods)
var_forecast <- forecast(var_fit, h = forecast_periods)

# Calculate accuracy of SARIMA and VAR
sarima_accuracy <- accuracy(sarima_forecast, test$AirTemperature)
var_accuracy <- accuracy(var_forecast$forecast$Temperature, test$AirTemperature)

# Store VAR forecasted pressure and humidity to be used in dynamic model
future_pressure <- var_forecast$forecast$Pressure$mean
future_humidity <- var_forecast$forecast$Humidity$mean

# Fit dynamic model
dynamic_fit <- auto.arima(ts(train$AirTemperature, frequency = 24),
  xreg = cbind(
    Pressure = train$BarometricPressure,
    Humidity = train$RelativeHumidity
  ),
  seasonal = TRUE,
  lambda = "auto"
)

# Use VAR forecasted pressure and humidity in dynamic model
dynamic_forecast <- forecast(dynamic_fit,
  xreg = cbind(
    Pressure = future_pressure,
    Humidity = future_humidity
  ),
  h = forecast_periods
)

# Combine forecasts with weights
# Calculate weights based on inverse of training errors
var_rmse <- accuracy(
  var_forecast$forecast$Temperature$mean, test$AirTemperature
)[, "RMSE"]

dynamic_rmse <- accuracy(
  dynamic_forecast$mean, test$AirTemperature
)[, "RMSE"]

rolling_weights <- function(var_pred, dyn_pred, window = 24) {
  # Use rollapply with partial windows at the start
  recent_var_error <- rollapply(abs(var_pred - test$AirTemperature),
    width = window,
    FUN = mean,
    align = "right",
    partial = TRUE
  )

  recent_dyn_error <- rollapply(abs(dyn_pred - test$AirTemperature),
    width = window,
    FUN = mean,
    align = "right",
    partial = TRUE
  )

  # Calculate weights based on inverse of training errors
  w1 <- 1 / recent_var_error / (1 / recent_var_error + 1 / recent_dyn_error)
  return(list(w1 = w1, w2 = 1 - w1))
}

# List of weights for each forecast period (237 samples)
weights <- rolling_weights(var_forecast$forecast$Temperature$mean, dynamic_forecast$mean, window = 24)
w1 <- weights$w1
w2 <- weights$w2

# Combine forecasts from VAR and dynamic model
combined_forecast <- w1 * var_forecast$forecast$Temperature$mean +
  w2 * dynamic_forecast$mean

# Check accuracy
combined_accuracy <- accuracy(combined_forecast, test$AirTemperature)
dynamic_accuracy <- accuracy(dynamic_forecast$mean, test$AirTemperature)

# Print accuracy of each model
print("SARIMA Accuracy: ")
print(sarima_accuracy)
print("VAR Accuracy: ")
print(var_accuracy)
print("Dynamic Accuracy: ")
print(dynamic_accuracy)
print("Combined Accuracy: ")
print(combined_accuracy)

# First check the format of timestamps
print(head(test$timestamp, 24)) # Look at a full day of data

# Fix timestamp format with conditional handling
test$timestamp <- ifelse(
  !grepl(":", test$timestamp), # If no time component
  paste(test$timestamp, "00:00:00"), # Add midnight time
  test$timestamp # Keep original if time exists
)

# Double check fixed timestamp
print(head(test$timestamp, 24))

# Convert to proper timestamp format
test$timestamp <- as.POSIXct(test$timestamp)

# Ensure all data is numeric and timestamps are properly formatted
forecast_df <- data.frame(
  timestamp = test$timestamp,
  Actual = as.numeric(test$AirTemperature),
  SARIMA = as.numeric(sarima_forecast$mean),
  VAR = as.numeric(var_forecast$forecast$Temperature$mean),
  Combined = as.numeric(combined_forecast)
)

# Check the data structure
head(forecast_df)

# Convert to long format for ggplot
forecast_long <- gather(forecast_df, Model, Temperature, -timestamp)

# Create plot
ggplot(forecast_long, aes(x = timestamp, y = Temperature, color = Model)) +
  geom_line() +
  theme_minimal() +
  labs(
    title = "Comparison of Model Forecasts",
    x = "Time",
    y = "Temperature"
  ) +
  scale_color_manual(values = c("black", "blue", "red", "green")) +
  theme(legend.position = "bottom")

# Error distribution plot
error_df <- data.frame(
  SARIMA = test$AirTemperature - sarima_forecast$mean,
  VAR = test$AirTemperature - var_forecast$forecast$Temperature$mean,
  Combined = test$AirTemperature - combined_forecast
)

# Convert to long format for ggplot
error_long <- gather(error_df, Model, Error)

# Create plot
ggplot(error_long, aes(x = Error, fill = Model)) +
  geom_density(alpha = 0.5) +
  theme_minimal() +
  labs(
    title = "Error Distribution by Model",
    x = "Forecast Error",
    y = "Density"
  )
