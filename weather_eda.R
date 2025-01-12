library(lubridate)
library(ggplot2)
library(dplyr)
library(DescTools)
library(corrplot)
library(DataExplorer)

weather_data <- read.csv("dataset/06_Weather.csv")

str(weather_data)
summary(weather_data)
head(weather_data)

weather_data$LocalTimestampConverted <- mdy_hm(weather_data$LocalTimestamp)
str(weather_data)
summary(weather_data)

# Extract time-based features from LocalTimestamp
weather_data <- weather_data %>%
  mutate(
    hour = hour(LocalTimestampConverted),
    day = day(LocalTimestampConverted),
    month = month(LocalTimestampConverted),
    weekday = wday(LocalTimestampConverted, label = TRUE)
  )

# Plot Air Temperature over time
ggplot(weather_data, aes(x = LocalTimestampConverted, y = AirTemperature)) +
  geom_line() +
  labs(title = "Air Temperature Over Time")

# Distribution of Air Temperature
ggplot(weather_data, aes(x = AirTemperature)) +
  geom_histogram(bins = 30, fill = "skyblue") +
  labs(title = "Distribution of Air Temperature")

# Plot Barometric Pressure over time
ggplot(weather_data, aes(x = LocalTimestampConverted, y = BarometricPressure)) +
  geom_line() +
  labs(title = "Barometric Pressure Over Time")

# Distribution of Barometric Pressure
ggplot(weather_data, aes(x = BarometricPressure)) +
  geom_histogram(bins = 30, fill = "skyblue") +
  labs(title = "Distribution of Barometric Pressure")

# Plot Relative Humidity over time
ggplot(weather_data, aes(x = LocalTimestampConverted, y = RelativeHumidity)) +
  geom_line() +
  labs(title = "Relative Humidity Over Time")

# Distribution of Relative Humidity
ggplot(weather_data, aes(x = RelativeHumidity)) +
  geom_histogram(bins = 30, fill = "skyblue") +
  labs(title = "Distribution of Relative Humidity")

# Correlation Plot
cor_matrix <-
  cor(weather_data[
    , c("AirTemperature", "BarometricPressure", "RelativeHumidity")
  ])
corrplot(cor_matrix, method = "circle")

# Check for missing values
plot_missing(weather_data)

weather_data$AirTemperatureScaled <- scale(weather_data$AirTemperature)
weather_data$BarometricPressureScaled <- scale(weather_data$BarometricPressure)
weather_data$RelativeHumidityScaled <- log(Winsorize(
  weather_data$RelativeHumidity
) + 1)

# Distribution of Air Temperature after processing
ggplot(weather_data, aes(x = AirTemperatureScaled)) +
  geom_histogram(bins = 30, fill = "skyblue") +
  labs(title = "Distribution of Air Temperature after processing")

# Distribution of Barometric Pressure after processing
ggplot(weather_data, aes(x = BarometricPressureScaled)) +
  geom_histogram(bins = 30, fill = "skyblue") +
  labs(title = "Distribution of Barometric Pressure after processing")

# Distribution of Relative Humidity after processing
ggplot(weather_data, aes(x = RelativeHumidityScaled)) +
  geom_histogram(bins = 30, fill = "skyblue") +
  labs(title = "Distribution of Relative Humidity after processing")