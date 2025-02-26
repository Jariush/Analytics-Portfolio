# Load necessary libraries
#BEST REGRESSION MODEL
library(ggplot2)
library(dplyr)
library(lubridate)
library(tidyr)

# Load data
ridership <- read.csv("C:/Users/hamidja/OneDrive - Lee County BoCC/Intern Project/ridership_data.csv")

# Convert Date column to Date type
ridership$Date <- as.Date(ridership$Date, format = "%m/%d/%Y")

# Convert Ridership column to numeric
ridership$Ridership <- as.numeric(ridership$Ridership)

# Create a feature indicating whether the date falls within the service change period (Nov - Apr)
ridership$ServiceChange <- ifelse(month(ridership$Date) %in% c(11, 12, 1, 2, 3, 4), 1, 0)

# Create a numeric variable for Date to use in the linear model
ridership$DateNumeric <- as.numeric(ridership$Date)

# Aggregate data by week
weekly_data <- ridership %>%
  mutate(Week = floor_date(Date, "week")) %>%
  group_by(Week) %>%
  summarize(
    Ridership = sum(Ridership, na.rm = TRUE),
    ServiceChange = max(ServiceChange, na.rm = TRUE)
  ) %>%
  ungroup()

# Define the week to exclude
exclude_week <- as.Date("2024-09-08") %>% floor_date("week")

# Exclude the specified week from historical data
weekly_data <- weekly_data %>%
  filter(Week != exclude_week)

# Create a numeric variable for Week
weekly_data$WeekNumeric <- as.numeric(weekly_data$Week)

# Fit the linear regression model to weekly data
lm_model_weekly <- lm(Ridership ~ WeekNumeric + ServiceChange, data = weekly_data)

# Create a new dataframe for future weekly predictions
future_weeks <- seq(max(weekly_data$Week) + weeks(1), by = "week", length.out = 52)
future_data_weekly <- data.frame(
  Week = future_weeks,
  WeekNumeric = as.numeric(future_weeks),
  ServiceChange = ifelse(month(future_weeks) %in% c(11, 12, 1, 2, 3, 4), 1, 0)
)

# Exclude the specified week from future data
future_data_weekly <- future_data_weekly %>%
  filter(Week != exclude_week)

# Predict future values and get confidence intervals
forecast_weekly <- predict(lm_model_weekly, newdata = future_data_weekly, interval = "confidence")

# Add forecast to the future data
future_data_weekly <- cbind(future_data_weekly, forecast_weekly)

# Plot the actual data and the forecast with confidence intervals
ggplot() +
  geom_line(data = weekly_data, aes(x = Week, y = Ridership), color = "black", size = 1) +
  geom_ribbon(data = future_data_weekly, aes(x = Week, ymin = lwr, ymax = upr), alpha = 0.2, fill = "blue") +
  geom_smooth(data = future_data_weekly, aes(x = Week, y = fit), color = "blue") +
  ggtitle("Weekly Linear Regression Forecast with Confidence Intervals") +
  ylab("Ridership (in 000s)") +
  xlab("Date") +
  theme_minimal() 
