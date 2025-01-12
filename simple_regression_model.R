library(dplyr)
library(DescTools)
library(lubridate)
library(xgboost)
library(ggplot2)

# Read and preprocess data
weather_data <- read.csv("dataset/06_Weather.csv")
weather_data$ObservationTimestamp <- ymd_hms(weather_data$ObservationTimestamp)

# Add station ID and sort data
weather_data <- weather_data %>%
  mutate(StationID = sub("_.*", "", SID)) %>%
  group_by(StationID) %>%
  arrange(ObservationTimestamp) %>%
  ungroup()

# Create scaled variables and store scaling parameters
temp_scale <- scale(weather_data$AirTemperature)
pressure_scale <- scale(weather_data$BarometricPressure)
weather_data$AirTemperatureScaled <- temp_scale
weather_data$BarometricPressureScaled <- pressure_scale

scaling_params <- list(
  temp_center = attr(temp_scale, "scaled:center"),
  temp_scale = attr(temp_scale, "scaled:scale"),
  pressure_center = attr(pressure_scale, "scaled:center"),
  pressure_scale = attr(pressure_scale, "scaled:scale")
)

# Process variables by station
weather_data <- weather_data %>%
  group_by(StationID) %>%
  arrange(ObservationTimestamp) %>%
  mutate(
    # Basic lags
    lag_temp = lag(AirTemperatureScaled),
    lag_pressure = lag(BarometricPressureScaled),
    lag_humidity = log(Winsorize(RelativeHumidity) + 1),
    lag_humidity_prev = lag(lag_humidity),

    # Non-linear transformations
    lag_temp_squared = lag_temp^2,
    lag_pressure_squared = lag_pressure^2,
    lag_humidity_squared = lag_humidity_prev^2,

    # Interaction terms
    temp_pressure_interact = lag_temp * lag_pressure,
    temp_humidity_interact = lag_temp * lag_humidity_prev,
    pressure_humidity_interact = lag_pressure * lag_humidity_prev
  ) %>%
  ungroup() %>%
  na.omit()

# Prepare feature matrix for XGBoost
features <- as.matrix(weather_data[, c(
  # Basic lags
  "lag_temp", "lag_pressure", "lag_humidity_prev",
  # Non-linear terms
  "lag_temp_squared", "lag_pressure_squared", "lag_humidity_squared",
  # Interactions
  "temp_pressure_interact", "temp_humidity_interact", "pressure_humidity_interact"
)])

# Split data into train and test sets (80-20 split)
set.seed(351)
train_idx <- sample(seq_len(nrow(weather_data)), 0.8 * nrow(weather_data))
train_data <- features[train_idx, ]
test_data <- features[-train_idx, ]
train_labels <- weather_data$AirTemperatureScaled[train_idx]
test_labels <- weather_data$AirTemperatureScaled[-train_idx]

# Grid search for best parameters
param_grid <- expand.grid(
  max_depth = c(3, 6, 9),
  eta = c(0.01, 0.1, 0.3),
  subsample = c(0.6, 0.8, 1.0),
  colsample_bytree = c(0.6, 0.8, 1.0)
)

# Convert data to matrix format once
train_matrix <- as.matrix(train_data)

# Function to evaluate parameters
evaluate_params <- function(params, train_matrix, train_labels) {
  cv <- xgb.cv(
    params = params,
    data = train_matrix,
    label = train_labels,
    nrounds = 1000,
    nfold = 5,
    early_stopping_rounds = 50,
    verbose = 0
  )
  return(min(cv$evaluation_log$test_rmse_mean))
}

# Find best parameters
best_params <- NULL
best_rmse <- Inf

for (i in seq_len(nrow(param_grid))) {
  current_params <- list(
    objective = "reg:squarederror",
    max_depth = param_grid$max_depth[i],
    eta = param_grid$eta[i],
    subsample = param_grid$subsample[i],
    colsample_bytree = param_grid$colsample_bytree[i]
  )

  current_rmse <- evaluate_params(current_params, train_matrix, train_labels)

  if (current_rmse < best_rmse) {
    best_rmse <- current_rmse
    best_params <- current_params
  }
}

# Set up XGBoost parameters
xgb_params <- list(
  objective = "reg:squarederror",
  eta = best_params$eta,
  max_depth = best_params$max_depth,
  subsample = best_params$subsample,
  colsample_bytree = best_params$colsample_bytree
)

# Find optimal number of rounds using cross-validation
cv_temp <- xgb.cv(
  params = xgb_params,
  data = train_data,
  label = train_labels,
  nrounds = 1000,
  nfold = 5,
  early_stopping_rounds = 50,
  verbose = 0
)

best_nrounds <- cv_temp$best_iteration

# Train final model
xgb_model <- xgboost(
  params = xgb_params,
  data = train_data,
  label = train_labels,
  nrounds = best_nrounds,
  verbose = 0
)

# Make predictions
train_pred <- predict(xgb_model, train_data)
test_pred <- predict(xgb_model, test_data)

# Calculate RMSE
train_rmse <- sqrt(mean((train_pred - train_labels)^2))
test_rmse <- sqrt(mean((test_pred - test_labels)^2))

# Convert scaled predictions back to original temperature values
train_pred_orig <- train_pred * scaling_params$temp_scale + scaling_params$temp_center
test_pred_orig <- test_pred * scaling_params$temp_scale + scaling_params$temp_center
train_actual <- weather_data$AirTemperature[train_idx]
test_actual <- weather_data$AirTemperature[-train_idx]

train_rmse_orig <- sqrt(mean((train_pred_orig - train_actual)^2))
test_rmse_orig <- sqrt(mean((test_pred_orig - test_actual)^2))

# Print results
cat("\nModel Performance:\n")
cat("Training RMSE (scaled):", train_rmse, "\n")
cat("Test RMSE (scaled):", test_rmse, "\n")
cat("Training RMSE (°C):", train_rmse_orig, "\n")
cat("Test RMSE (°C):", test_rmse_orig, "\n")

# Feature importance
importance_matrix <- xgb.importance(feature_names = colnames(features), model = xgb_model)
cat("\nFeature Importance:\n")
print(importance_matrix)

# Optional: Plot actual vs predicted values
if (requireNamespace("ggplot2", quietly = TRUE)) {
  results_df <- data.frame(
    Actual = test_actual,
    Predicted = test_pred_orig
  )

  ggplot(results_df, aes(x = Actual, y = Predicted)) +
    geom_point(alpha = 0.5) +
    geom_abline(intercept = 0, slope = 1, color = "red") +
    labs(
      title = "Actual vs Predicted Temperature",
      x = "Actual Temperature (°C)",
      y = "Predicted Temperature (°C)"
    ) +
    theme_minimal()
}
