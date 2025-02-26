# Load necessary libraries
#Ridership graphs
library(forecast)
library(ggplot2)
library(dplyr)
library(lubridate)
library(gridExtra)
library(tidyr)
library(zoo)

# Load data
ridership <- read.csv("C:/Users/hamidja/OneDrive - Lee County BoCC/Intern Project/ridership_data.csv")

ridership.ts <- ts(ridership$Ridership,
                   start = c(2022, 11), end = c(2024, 9), freq = 52)

# plot the series using the autoplot function to make use of ggplot
autoplot(ridership.ts, xlab="Time", ylab="Ridership (in 000s)") +
  scale_y_continuous(limits=c(1000, 6000))



# to zoom in to a certain period, use window() to create a new, shorter time series
# we create a new, 3-year time series of ridership.ts from Jan 1997 to Dec 1999
ridership.ts.3yrs <- window(ridership.ts, start =  c(2022, 11), end = c(2024, 9))

g1 <- autoplot(ridership.ts.3yrs, xlab="Time", ylab="Ridership (in 000s)") +
  scale_y_continuous(limits=c(1000, 6000))

# fit a trend line to the time series
g2 <- autoplot(ridership.ts, xlab="Time", ylab="Ridership (in 000s)") +
  scale_y_continuous(limits=c(1000, 6000)) +
  geom_smooth(method="lm", formula=y~poly(x, 2))

grid.arrange(g1, g2, nrow=2)


# we can also use tslm to create the quadratic fit
ridership.lm <- tslm(ridership.ts ~ trend + I(trend^2))
autoplot(ridership.ts, xlab="Time", ylab="Ridership (in 000s)") +
  scale_y_continuous(limits=c(1000, 6000)) +
  autolayer(ridership.lm$fitted.values)