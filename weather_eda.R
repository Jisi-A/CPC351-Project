library(lubridate)
library(ggplot2)
library(dplyr)
library(DescTools)
library(corrplot)
library(DataExplorer)
library(car)

weather_data <- read.csv("dataset/06_Weather.csv")

str(weather_data)
summary(weather_data)
head(weather_data)

weather_data$LocalTimestampConverted <- mdy_hm(weather_data$LocalTimestamp)
str(weather_data)
summary(weather_data)

weather_data$DeviceID <- as.factor(weather_data$DeviceID)
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

# Plot Air Temperature over time by DeviceID
ggplot(weather_data, aes(x = LocalTimestampConverted, y = AirTemperature)) +
  geom_line(color = "darkred") +
  facet_wrap(~DeviceID) +
  labs(
    title = "Air Temperature Over Time by Device",
    x = "Timestamp",
    y = "Air Temperature"
  ) +
  theme_minimal()

# Distribution of Air Temperature by Device
ggplot(weather_data, aes(x = AirTemperature)) +
  geom_histogram(bins = 30, fill = "skyblue") +
  facet_wrap(~DeviceID) +
  labs(
    title = "Distribution of Air Temperature by Device",
    x = "Air Temperature",
    y = "Count"
  ) +
  theme_minimal()

# Plot Barometric Pressure over time by DeviceID
ggplot(weather_data, aes(x = LocalTimestampConverted, y = BarometricPressure)) +
  geom_line(color = "darkred") +
  facet_wrap(~DeviceID) +
  labs(
    title = "Barometric Pressure Over Time",
    x = "Timestamp",
    y = "Barometric Pressure"
  ) +
  theme_minimal()

# Distribution of Barometric Pressure
ggplot(weather_data, aes(x = BarometricPressure)) +
  geom_histogram(bins = 30, fill = "darkred", alpha = 0.7) +
  facet_wrap(~DeviceID) +
  labs(
    title = "Distribution of Barometric Pressure by Device",
    x = "Barometric Pressure",
    y = "Count"
  ) +
  theme_minimal()

# Plot Relative Humidity over time
ggplot(weather_data, aes(x = LocalTimestampConverted, y = RelativeHumidity)) +
  geom_line(color = "darkgreen") +
  facet_wrap(~DeviceID) +
  labs(
    title = "Relative Humidity Over Time",
    x = "Timestamp",
    y = "Relative Humidity"
  ) +
  theme_minimal()

# Distribution of Relative Humidity
ggplot(weather_data, aes(x = RelativeHumidity)) +
  geom_histogram(bins = 30, fill = "darkgreen", alpha = 0.7) +
  facet_wrap(~DeviceID) +
  labs(
    title = "Distribution of Relative Humidity by Device",
    x = "Relative Humidity",
    y = "Count"
  ) +
  theme_minimal()

weather_data_D1 <- weather_data %>% filter(DeviceID == "18zua9muwbb")
weather_data_D2 <- weather_data %>% filter(DeviceID == "2hq3byfebne")
weather_data_D3 <- weather_data %>% filter(DeviceID == "uu90853psl")

summary(weather_data_D1)
summary(weather_data_D2)
summary(weather_data_D3)

# Correlation Plot
cor_matrix <-
  cor(weather_data_D3[
    , c("AirTemperature", "BarometricPressure", "RelativeHumidity")
  ])
corrplot(cor_matrix, method = "circle")

# Check for missing values
plot_missing(weather_data_D3)

# Create hourly averaged data
hourly_weather_data <- weather_data %>%
  group_by(DeviceID,
    date = as.Date(LocalTimestampConverted),
    hour = hour(LocalTimestampConverted)
  ) %>%
  summarise(
    AirTemperature = mean(AirTemperature, na.rm = TRUE),
    BarometricPressure = mean(BarometricPressure, na.rm = TRUE),
    RelativeHumidity = mean(RelativeHumidity, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(timestamp = as.POSIXct(paste(date, hour), format = "%Y-%m-%d %H"))

# View the hourly averaged data
head(hourly_weather_data)
str(hourly_weather_data)
summary(hourly_weather_data)

hourly_D3 <- hourly_weather_data %>% filter(DeviceID == "uu90853psl")

# Plot hourly averaged Air Temperature
ggplot(hourly_D3, aes(x = timestamp, y = AirTemperature)) +
  geom_line(color = "darkred") +
  facet_wrap(~DeviceID) +
  labs(
    title = "Hourly Averaged Air Temperature Over Time by Device",
    x = "Timestamp",
    y = "Air Temperature"
  ) +
  theme_minimal()

# Plot hourly averaged Barometric Pressure
ggplot(hourly_D3, aes(x = timestamp, y = BarometricPressure)) +
  geom_line(color = "darkblue") +
  facet_wrap(~DeviceID) +
  labs(
    title = "Hourly Averaged Barometric Pressure Over Time by Device",
    x = "Timestamp",
    y = "Barometric Pressure"
  ) +
  theme_minimal()

# Plot hourly averaged Relative Humidity
this_plt <- ggplot(hourly_D3, aes(x = timestamp, y = RelativeHumidity)) +
  geom_line(color = "darkgreen") +
  facet_wrap(~DeviceID) +
  labs(
    title = "Hourly Averaged Relative Humidity Over Time by Device",
    x = "Timestamp",
    y = "Relative Humidity"
  ) +
  theme_minimal()

this_plt

# Distribution of hourly averaged measurements
ggplot(hourly_D3, aes(x = AirTemperature)) +
  geom_histogram(bins = 30, fill = "darkred", alpha = 0.7) +
  facet_wrap(~DeviceID) +
  labs(
    title = "Distribution of Hourly Averaged Air Temperature by Device",
    x = "Air Temperature",
    y = "Count"
  ) +
  theme_minimal()

ggplot(hourly_D3, aes(x = BarometricPressure)) +
  geom_histogram(bins = 30, fill = "darkblue", alpha = 0.7) +
  facet_wrap(~DeviceID) +
  labs(
    title = "Distribution of Hourly Averaged Barometric Pressure by Device",
    x = "Barometric Pressure",
    y = "Count"
  ) +
  theme_minimal()

ggplot(hourly_D3, aes(x = RelativeHumidity)) +
  geom_histogram(bins = 30, fill = "darkgreen", alpha = 0.7) +
  facet_wrap(~DeviceID) +
  labs(
    title = "Distribution of Hourly Averaged Relative Humidity by Device",
    x = "Relative Humidity",
    y = "Count"
  ) +
  theme_minimal()

# Box plots for Device 3 hourly data
# Create box plot for Air Temperature
ggplot(hourly_D3, aes(y = AirTemperature)) +
  geom_boxplot(fill = "darkred", alpha = 0.7) +
  labs(
    title = "Box Plot of Hourly Air Temperature - Device 3",
    y = "Air Temperature"
  ) +
  theme_minimal()

# Box plot for Barometric Pressure
ggplot(hourly_D3, aes(y = BarometricPressure)) +
  geom_boxplot(fill = "darkblue", alpha = 0.7) +
  labs(
    title = "Box Plot of Hourly Barometric Pressure - Device 3",
    y = "Barometric Pressure"
  ) +
  theme_minimal()

# Box plot for Relative Humidity
ggplot(hourly_D3, aes(y = RelativeHumidity)) +
  geom_boxplot(fill = "darkgreen", alpha = 0.7) +
  labs(
    title = "Box Plot of Hourly Relative Humidity - Device 3",
    y = "Relative Humidity"
  ) +
  theme_minimal()

# Print summary statistics to see actual values
summary(hourly_D3[c("AirTemperature", "BarometricPressure", "RelativeHumidity")])

# Calculate outlier boundaries for each variable
air_temp_stats <- boxplot.stats(hourly_D3$AirTemperature)
pressure_stats <- boxplot.stats(hourly_D3$BarometricPressure)
humidity_stats <- boxplot.stats(hourly_D3$RelativeHumidity)

# Print number of outliers for each variable
cat("Number of outliers:\n",
    "Air Temperature:", length(air_temp_stats$out), "\n",
    "Barometric Pressure:", length(pressure_stats$out), "\n",
    "Relative Humidity:", length(humidity_stats$out), "\n")

# List outliers
boxplot.stats(hourly_D3$AirTemperature)$out

# Transform Relative Humidity
hourly_D3$RelativeHumidityTransformed <- bcPower(hourly_D3$RelativeHumidity, lambda = 0.3)

ggplot(hourly_D3, aes(x = RelativeHumidityTransformed)) +
  geom_histogram(bins = 30, fill = "darkgreen", alpha = 0.7) +
  facet_wrap(~DeviceID) +
  labs(
    title = "Distribution of Hourly Averaged Relative Humidity After Transformation",
    x = "Relative Humidity",
    y = "Count"
  ) +
  theme_minimal()

# Perform Z-score Scaling to Data
hourly_D3$AirTemperatureScaled <- scale(hourly_D3$AirTemperature)
hourly_D3$BarometricPressureScaled <- scale(hourly_D3$BarometricPressure)

# Perform Min-Max Scaling to Data
hourly_D3$RelativeHumidityScaled <- (hourly_D3$RelativeHumidityTransformed - min(hourly_D3$RelativeHumidityTransformed)) / 
  (max(hourly_D3$RelativeHumidityTransformed) - min(hourly_D3$RelativeHumidityTransformed))

# Plot Scaled Data
ggplot(hourly_D3, aes(x = timestamp, y = AirTemperatureScaled)) +
  geom_line(color = "darkred") +
  labs(
    title = "Hourly Averaged Air Temperature Over Time by Device",
    x = "Timestamp",
    y = "Air Temperature"
  ) +
  theme_minimal()

ggplot(hourly_D3, aes(x = timestamp, y = BarometricPressureScaled)) +
  geom_line(color = "darkblue") +
  labs(
    title = "Hourly Averaged Barometric Pressure Over Time by Device",
    x = "Timestamp",
    y = "Barometric Pressure"
  ) +
  theme_minimal()

ggplot(hourly_D3, aes(x = timestamp, y = RelativeHumidityScaled)) +
  geom_line(color = "darkgreen") +
  labs(
    title = "Hourly Averaged Relative Humidity Over Time by Device",
    x = "Timestamp",
    y = "Relative Humidity"
  ) +
  theme_minimal()

# Create scatter plot for Temperature vs Humidity
ggplot(hourly_D3, aes(x = RelativeHumidity, y = AirTemperature)) +
  geom_point(alpha = 0.5, color = "darkgreen") +
  geom_smooth(method = "lm", color = "red", se = FALSE) +
  labs(
    title = "Relative Humidity vs Air Temperature - Device 3",
    x = "Relative Humidity",
    y = "Air Temperature"
  ) +
  theme_minimal()

# Create scatter plot for Temperature vs Pressure
ggplot(hourly_D3, aes(x = BarometricPressure, y = AirTemperature)) +
  geom_point(alpha = 0.5, color = "darkblue") +
  geom_smooth(method = "lm", color = "red", se = FALSE) +
  labs(
    title = "Barometric Pressure vs Air Temperature - Device 3",
    x = "Barometric Pressure",
    y = "Air Temperature"
  ) +
  theme_minimal()