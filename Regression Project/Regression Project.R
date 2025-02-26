# Load All of the Libraries
library(ggplot2)
library(dplyr)
library(lubridate)
library(tidyverse)
library(ggplot2)
library(dplyr)
library(lubridate)
library(tidyverse)
library(e1071)
library(xtable)
library(psych)
library(caret)
library(fixest)

# Load Regression Data
reg_data <- read.csv("C:/Users/hamidja/OneDrive - Lee County BoCC/Intern Project/r_reg.csv")

# Rename the variable
names(reg_data)[names(reg_data) == "route.."] <- "route"

# Creating Categorical Variables
reg_data$route <- as.factor(reg_data$route)
reg_data$day <- as.factor(reg_data$day)

# Compute summary statistics
table <- describe(reg_data)

# Convert the summary statistics to a data frame
table_df <- as.data.frame(table)

# Remove unnecessary columns and rename them
table_df <- table_df[, c("mean", "sd", "median", "range", "skew", "kurtosis")]
names(table_df) <- c("Mean", "SD", "Median", "Range", "Skew", "Kurtosis")

# Add a column for variable names
table_df$Variable <- rownames(table_df)

# Reorder columns
table_df <- table_df[, c("Variable", "Mean", "SD", "Median", "Range", "Skew", "Kurtosis")]

# Generate LaTeX code
latex_table <- xtable(table_df, caption = "Summary Statistics", label = "tab:summary_statistics")

# Print LaTeX code
print(latex_table, type = "latex", include.rownames = FALSE, booktabs = TRUE)


# Set a seed for reproducibility
set.seed(123)

# Create a partition for training and testing (80% training, 20% testing)
train_index <- createDataPartition(reg_data$on_time, p = 0.8, list = FALSE)
train_data <- reg_data[train_index, ]
test_data <- reg_data[-train_index, ]

# Further partition the training data into training and validation sets (80% training, 20% validation)
val_index <- createDataPartition(train_data$on_time, p = 0.8, list = FALSE)
training_set <- train_data[val_index, ]
validation_set <- train_data[-val_index, ]

# Fit a regression model
# model <- lm(on_time ~ route + day + avg_rain_inch + accident_count, data = training_set)

# Fit the fixed effects model with route-specific effects
model_fe <- feols(on_time ~ day + upt + avg_rain_inch + accident_count | route, data = training_set)

# Summary of the model
summary(model_fe)


# Validate using validation_set
predictions_val <- predict(model, newdata = validation_set)
# Evaluate the performance (e.g., RMSE, MAE)



# New data for prediction
new_data <- data.frame(
  route = factor(c("140"), levels = levels(train_data$route)), 
  day = factor(c("Monday"), levels = levels(train_data$day)), 
  avg_rain_inch = c(100),
  accident_count = c(50)
)

# Predict OTP for new data
future_predictions <- predict(model, newdata = new_data)

# Clamp predictions to the range 0-100
future_predictions <- pmin(pmax(future_predictions, 0), 100)

# Print future predictions
print(future_predictions)




#Day Analysis Chart
ggplot(data = reg_data, aes(x = day, y = on_time)) +
  geom_boxplot() + # or use geom_point() for individual points
  labs(
    title = "On-Time Performance by Day",
    x = "Day",
    y = "On-Time Performance"
  ) +
  theme_minimal()

# Compute summary statistics
table <- describe(reg_data)

# Convert the summary statistics to a data frame
table_df <- as.data.frame(table)

# Remove unnecessary columns and rename them
table_df <- table_df[, c("mean", "sd", "median", "range", "skew", "kurtosis")]
names(table_df) <- c("Mean", "SD", "Median", "Range", "Skew", "Kurtosis")

# Add a column for variable names
table_df$Variable <- rownames(table_df)

# Reorder columns
table_df <- table_df[, c("Variable", "Mean", "SD", "Median", "Range", "Skew", "Kurtosis")]

# Generate LaTeX code
latex_table <- xtable(table_df, caption = "Summary Statistics", label = "tab:summary_statistics")

# Print LaTeX code
print(latex_table, type = "latex", include.rownames = FALSE, booktabs = TRUE)