library(dplyr)
library(DescTools)
library(lubridate)
library(xgboost)

weather_data <- read.csv("dataset/06_Weather.csv")

# Convert timestamps and sort data
weather_data$ObservationTimestamp <- ymd_hms(weather_data$ObservationTimestamp)
weather_data <- weather_data %>% arrange(ObservationTimestamp)

# Store scaling parameters when creating scaled variables
temp_scale <- scale(weather_data$AirTemperature)
weather_data$AirTemperatureScaled <- temp_scale
scaling_params <- list(
  center = attr(temp_scale, "scaled:center"),
  scale = attr(temp_scale, "scaled:scale")
)

# Process variables
weather_data$BarometricPressureScaled <- scale(weather_data$BarometricPressure)
weather_data$RelativeHumidityScaled <- log(
  Winsorize(weather_data$RelativeHumidity) + 1
)

# Create lagged variables based on time windows
weather_data <- weather_data %>%
  mutate(
    # Calculate time difference in minutes
    time_diff = as.numeric(
      difftime(ObservationTimestamp, lag(ObservationTimestamp), units = "mins")
    ),

    # Create weighted lags based on time difference
    lag_temp = lag(AirTemperatureScaled) * exp(-time_diff / 60),
    lag_pressure = lag(BarometricPressureScaled) * exp(-time_diff / 60),
    lag_humidity = lag(RelativeHumidityScaled) * exp(-time_diff / 60),

    # Polynomial terms
    lag_temp_squared = lag_temp^2,
    lag_pressure_squared = lag_pressure^2,
    lag_humidity_squared = lag_humidity^2,

    # Logarithmic transformations (adding small constant to avoid log(0))
    log_time_diff = log(time_diff + 1),

    # Exponential decay
    exp_temp_decay = exp(-lag_temp),

    # Periodic transformations (if there might be daily patterns)
    hour_of_day = hour(ObservationTimestamp),
    sin_hour = sin(2 * pi * hour_of_day / 24),
    cos_hour = cos(2 * pi * hour_of_day / 24)
  ) %>%
  na.omit()

# Prepare feature matrix for XGBoost
features <- as.matrix(weather_data[, c(
  # Original features
  "lag_temp", "lag_pressure", "lag_humidity", "time_diff",
  # Non-linear transformations
  "lag_temp_squared", "lag_pressure_squared", "lag_humidity_squared",
  "log_time_diff", "exp_temp_decay",
  "sin_hour", "cos_hour"
)])

# Linear Models
temp_model <- lm(
  AirTemperatureScaled ~
    lag_temp + lag_pressure + lag_humidity + lag_temp_squared + lag_pressure_squared + lag_humidity_squared + log_time_diff + exp_temp_decay + sin_hour + cos_hour + time_diff,
  data = weather_data
)

# Set up cross-validation parameters
xgb_params <- list(
  objective = "reg:squarederror",
  eta = 0.1, # Learning rate
  max_depth = 6,
  subsample = 0.8,
  colsample_bytree = 0.8
)

# Find optimal number of rounds for temperature model
cv_temp <- xgb.cv(
  params = xgb_params,
  data = features,
  label = weather_data$AirTemperatureScaled,
  nrounds = 1000,
  nfold = 5,
  early_stopping_rounds = 50,
  verbose = 0
)

# Get optimal number of rounds for each model
best_nrounds_temp <- cv_temp$best_iteration

# Print optimal rounds
cat("\nOptimal number of rounds:\n")
cat("Temperature:", best_nrounds_temp, "\n")

# Update XGBoost models with optimal rounds
xgb_temp <- xgboost(
  params = xgb_params,
  data = features,
  label = weather_data$AirTemperatureScaled,
  nrounds = best_nrounds_temp,
  verbose = 0
)

# Get predictions for both models
predictions <- data.frame(
  # Linear model predictions
  lm_temp_pred = predict(temp_model, newdata = weather_data) * scaling_params$scale + scaling_params$center,

  # XGBoost predictions
  xgb_temp_pred = predict(xgb_temp, features) * scaling_params$scale + scaling_params$center
)

# Add actual values
predictions$temp_actual <- weather_data$AirTemperature

head(predictions)
sum(is.na(predictions$temp_actual))

# Calculate RMSE for both models
rmse <- tryCatch(
  {
    data.frame(
      # Linear Models RMSE
      LM_Temperature = sqrt(mean((predictions$lm_temp_pred - predictions$temp_actual)^2, na.rm = TRUE)),

      # XGBoost Models RMSE
      XGB_Temperature = sqrt(mean((predictions$xgb_temp_pred - predictions$temp_actual)^2, na.rm = TRUE))
    )
  },
  error = function(e) {
    print("Error in RMSE calculation:")
    print(e)
    return(NULL)
  }
)

# Print results
cat("\nLinear Models Summaries:\n")
cat("\nTemperature Model:\n")
summary(temp_model)

cat("\nRoot Mean Square Errors:\n")
print(rmse)

# Feature importance for XGBoost models
cat("\nXGBoost Feature Importance:\n")
cat("\nTemperature Model:\n")
importance_matrix <- xgb.importance(feature_names = colnames(features), model = xgb_temp)
print(importance_matrix)