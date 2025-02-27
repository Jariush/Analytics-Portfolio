Regression Project
================
Jarius Hamid
2024-08-26

``` r
knitr::opts_chunk$set(echo = TRUE)
```

## Loading all of The Libraries and Data

``` r
library(ggplot2)
library(dplyr)
```

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

``` r
library(lubridate)
```

    ## 
    ## Attaching package: 'lubridate'

    ## The following objects are masked from 'package:base':
    ## 
    ##     date, intersect, setdiff, union

``` r
library(tidyverse)
```

    ## ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
    ## ✔ forcats 1.0.0     ✔ stringr 1.5.1
    ## ✔ purrr   1.0.4     ✔ tibble  3.2.1
    ## ✔ readr   2.1.5     ✔ tidyr   1.3.1

    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()
    ## ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors

``` r
library(e1071)
library(xtable)
library(psych)
```

    ## 
    ## Attaching package: 'psych'
    ## 
    ## The following objects are masked from 'package:ggplot2':
    ## 
    ##     %+%, alpha

``` r
library(caret)
```

    ## Loading required package: lattice
    ## 
    ## Attaching package: 'caret'
    ## 
    ## The following object is masked from 'package:purrr':
    ## 
    ##     lift

``` r
library(fixest)
library(stringr)
library(forcats)

transit_data <- read.csv("r_reg.csv")
rain_and_temp <- read.csv("rain_and_temp_data.csv")
miles_and_stops <- read.csv("route_milesandstops.csv")
```

# Data Manipulation

``` r
# Rename the variable
names(transit_data)[names(transit_data) == "route.."] <- "route"

# Creating Categorical Variables

transit_data <- transit_data %>%
  mutate(avg_rain_inch = round(avg_rain_inch, digits = 2))

#renaming variables to be all lowercase and selecting appropriate variables for join
rain_and_temp <- rain_and_temp %>%
  rename_with(tolower) %>%
  select(date, tmax)

#joining rain_andtemp data into regression data
transit_data <- transit_data %>%
  left_join(rain_and_temp,by = "date")

###finish join with miles 
transit_data <- transit_data %>%
  left_join(miles_and_stops, by = "route") %>%
  # Normalize date formats and extract month
  mutate(
    # Parse the date using lubridate's mdy function, which handles multiple formats
    date = mdy(date), 
    # Extract month from the normalized date
    month = month(date),
    # Create the `season` variable
    season = ifelse(month >= 11 | month <= 4, 1, 0))

#create categorical variables


######################################################
#transit_data$route <- as.factor(transit_data$route)
#transit_data$day <- as.factor(transit_data$day)

#Omit missing values
transit_data_clean<- na.omit(transit_data)
```

# Principle Components Analysis

``` r
#select Variables Miles,s tops, and stops apart for PCA
pca_selected_vars <- transit_data_clean[, c("miles_roundtrip", "stops")]
 
# Standardize the selected variables
#pca_scaled_data <- scale(pca_selected_vars)

# Perform PCA
#pca_result <- prcomp(pca_scaled_data, center = TRUE, scale. = TRUE)



##################
# Standardize the data
pca_data_scaled <- scale(pca_selected_vars)

# Compute PCA on selected variables
pcs <- prcomp(pca_data_scaled, center = TRUE, scale. = TRUE)

# Summary of PCA
summary(pcs)
```

    ## Importance of components:
    ##                           PC1     PC2
    ## Standard deviation     1.3435 0.44149
    ## Proportion of Variance 0.9025 0.09746
    ## Cumulative Proportion  0.9025 1.00000

``` r
# Variance explained by each component
var_explained <- summary(pcs)$importance[2,]

# Plot variance explained
ggplot(data.frame(PC = 1:length(var_explained), Variance = var_explained), aes(x = PC, y = Variance)) +
  geom_bar(stat = "identity") +
  labs(title = "Variance Explained by Principal Components", x = "Principal Component", y = "Variance Explained")
```

![](OTP-Analysis_files/figure-gfm/pca-1.png)<!-- -->

``` r
# PCA Loadings
pcs$rotation
```

    ##                       PC1        PC2
    ## miles_roundtrip 0.7071068  0.7071068
    ## stops           0.7071068 -0.7071068

``` r
# Cumulative proportion of variance to see which PC to keep
cumulative_var <- cumsum(var_explained)
ggplot(data.frame(PC = 1:length(cumulative_var), CumulativeVariance = cumulative_var), aes(x = PC, y = CumulativeVariance)) +
  geom_line() +
  labs(title = "Cumulative Variance Explained by Principal Components", x = "Principal Component", y = "Cumulative Variance")
```

![](OTP-Analysis_files/figure-gfm/pca-2.png)<!-- -->

``` r
# Looking at PCA Scores
scores <- pcs$x
head(scores, 5)
```

    ##          PC1        PC2
    ## 1 -0.2774899 -0.6134051
    ## 2  1.0267975 -0.3236498
    ## 3  2.0133898  1.0011199
    ## 4 -0.3557549  0.5764949
    ## 5  2.0278193 -0.3794322

``` r
scores_df <- data.frame(scores)

ggplot(scores_df, aes(x = PC1, y = PC2)) +
  geom_point() +
  labs(title = "Scatter Plot of PC1 vs. PC2", x = "Principal Component 1", y = "Principal Component 2") +
  theme_minimal()
```

![](OTP-Analysis_files/figure-gfm/pca-3.png)<!-- -->

``` r
# Final touches
transit_data_clean <- transit_data_clean %>%
  # Add the first principal component
  mutate(PC1 = pcs$x[, 1]) 
```

# Summary Statistics with Latex Ouput

``` r
# Compute summary statistics
table <- describe(transit_data_clean)
```

    ## Warning in FUN(newX[, i], ...): no non-missing arguments to min; returning Inf

    ## Warning in FUN(newX[, i], ...): no non-missing arguments to max; returning -Inf

``` r
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
```

    ## % latex table generated in R 4.4.2 by xtable 1.8-4 package
    ## % Thu Feb 27 14:29:39 2025
    ## \begin{table}[ht]
    ## \centering
    ## \begin{tabular}{lrrrrrr}
    ##   \toprule
    ## Variable & Mean & SD & Median & Range & Skew & Kurtosis \\ 
    ##   \midrule
    ## date &  &  &  & -Inf &  &  \\ 
    ##   day* & 4.02 & 2.07 & 4.00 & 6.00 & -0.02 & -1.36 \\ 
    ##   route & 218.80 & 219.87 & 110.00 & 595.00 & 0.76 & -1.16 \\ 
    ##   sum\_processed & 135.05 & 69.72 & 119.00 & 377.00 & 1.45 & 2.60 \\ 
    ##   on\_time & 63.32 & 15.27 & 64.59 & 100.00 & -0.56 & 0.37 \\ 
    ##   late & 29.37 & 16.95 & 27.37 & 100.00 & 0.74 & 0.53 \\ 
    ##   early & 7.31 & 6.82 & 6.10 & 57.14 & 1.53 & 4.30 \\ 
    ##   avg\_rain\_inch & 0.13 & 0.36 & 0.00 & 3.66 & 5.57 & 40.71 \\ 
    ##   upt & 227.04 & 341.16 & 136.00 & 13175.00 & 8.46 & 220.33 \\ 
    ##   avg\_sched\_dev & 3.74 & 2.98 & 3.20 & 48.17 & 3.12 & 19.00 \\ 
    ##   index & 1.00 & 0.64 & 1.00 & 12.65 & 3.43 & 34.04 \\ 
    ##   accident\_count & 83.72 & 21.18 & 84.00 & 121.00 & -0.23 & -0.51 \\ 
    ##   tmax & 84.56 & 7.36 & 85.00 & 49.00 & -0.91 & 1.16 \\ 
    ##   miles\_roundtrip & 22.85 & 10.11 & 23.03 & 39.70 & 0.23 & -0.41 \\ 
    ##   stops & 78.05 & 33.45 & 80.00 & 134.00 & 0.35 & -0.30 \\ 
    ##   stops\_ftapart & 1633.79 & 483.32 & 1452.00 & 1962.00 & 0.55 & -0.46 \\ 
    ##   month & 6.15 & 3.69 & 5.00 & 11.00 & 0.23 & -1.32 \\ 
    ##   season & 0.61 & 0.49 & 1.00 & 1.00 & -0.45 & -1.80 \\ 
    ##   PC1 & -0.00 & 1.34 & -0.21 & 5.38 & 0.21 & -0.43 \\ 
    ##    \bottomrule
    ## \end{tabular}
    ## \caption{Summary Statistics} 
    ## \label{tab:summary_statistics}
    ## \end{table}

OTP BAR CHARTS

``` r
# Assuming your data is stored in a dataframe called 'transit_data_clean'

# Calculate OTP (On-Time Percentage) by day
transit_data_clean <- transit_data_clean %>%
  mutate(otp = (on_time / sum_processed) * 100)  # OTP as percentage

# Aggregate OTP by day
otp_by_day <- transit_data_clean %>%
  group_by(day) %>%
  summarize(mean_otp = mean(otp, na.rm = TRUE))

# Plot OTP by Day
ggplot(otp_by_day, aes(x = day, y = mean_otp)) +
  geom_bar(stat = "identity", fill = "skyblue", color = "black") +
  labs(title = "On-Time Performance by Day",
       x = "Day of the Week",
       y = "Average OTP (%)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
```

![](OTP-Analysis_files/figure-gfm/OTP%20BY%20DAY%20AND%20ROUTE-1.png)<!-- -->

``` r
# Aggregate OTP by route
otp_by_route <- transit_data_clean %>%
  mutate(otp = (on_time / sum_processed) * 100) %>%
  group_by(route) %>%
  summarize(mean_otp = mean(otp, na.rm = TRUE)) %>%
  ungroup() %>%
  # Convert 'route' to a factor (if not already) and reorder by mean OTP in descending order
  mutate(route = factor(route)) %>%
  mutate(route = fct_reorder(route, mean_otp, .desc = TRUE))

# Plot OTP by Route (ordered by descending OTP)
ggplot(otp_by_route, aes(x = route, y = mean_otp)) +
  geom_bar(stat = "identity", fill = "lightgreen", color = "black") +
  labs(title = "On-Time Performance by Route",
       x = "Route Number",
       y = "Average OTP (%)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
```

![](OTP-Analysis_files/figure-gfm/OTP%20BY%20DAY%20AND%20ROUTE-2.png)<!-- -->

# Scatter Plot Materix TESTING

``` r
# Box plot for season vs on_time
library(ggplot2)
ggplot(transit_data_clean, aes(x = as.factor(season), y = on_time)) +
  geom_boxplot() +
  labs(title = "Boxplot of On-Time Performance by Season",
       x = "Season",
       y = "On-Time Performance") +
  theme_minimal()
```

![](OTP-Analysis_files/figure-gfm/scatterplot-1.png)<!-- -->

``` r
# Load necessary libraries
library(corrplot)
```

    ## corrplot 0.95 loaded

``` r
library(dplyr)

# Subset the data to include the continuous variables and on_time
continuous_vars_with_otp <- transit_data_clean %>%
  select(avg_rain_inch, tmax, upt, accident_count, PC1, on_time)

# Compute the correlation matrix
cor_matrix_with_otp <- cor(continuous_vars_with_otp, use = "complete.obs")

# Plot the correlation matrix
corrplot(cor_matrix_with_otp, method = "circle", type = "upper", 
         tl.col = "black", tl.srt = 45)
```

![](OTP-Analysis_files/figure-gfm/scatterplot-2.png)<!-- -->

# Explanatory Regression

``` r
model_fe <- feols(on_time ~ avg_rain_inch^2 + tmax + upt + accident_count + season | route, data = transit_data_clean)
summary(model_fe)
```

    ## OLS estimation, Dep. Var.: on_time
    ## Observations: 9,848
    ## Fixed-effects: route: 25
    ## Standard-errors: Clustered (route) 
    ##                     Estimate Std. Error   t value   Pr(>|t|)    
    ## I(avg_rain_inch^2)  0.308413   0.118862  2.594720 1.5893e-02 *  
    ## tmax                0.080768   0.053838  1.500219 1.4660e-01    
    ## upt                 0.000928   0.001916  0.484361 6.3252e-01    
    ## accident_count     -0.112139   0.022027 -5.090872 3.3024e-05 ***
    ## season             -2.141691   1.197522 -1.788435 8.6338e-02 .  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## RMSE: 12.6     Adj. R2: 0.313204
    ##              Within R2: 0.054192

``` r
model_lm <- lm(on_time ~ avg_rain_inch^2 + tmax + upt + accident_count + route + season + PC1, data = transit_data_clean)
summary(model_lm)
```

    ## 
    ## Call:
    ## lm(formula = on_time ~ avg_rain_inch^2 + tmax + upt + accident_count + 
    ##     route + season + PC1, data = transit_data_clean)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -70.775  -8.264   1.188  10.010  37.793 
    ## 
    ## Coefficients:
    ##                  Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)    70.0299707  2.3968015  29.218  < 2e-16 ***
    ## avg_rain_inch   1.5970051  0.4181081   3.820 0.000134 ***
    ## tmax            0.0903018  0.0259163   3.484 0.000495 ***
    ## upt             0.0028468  0.0004315   6.598 4.40e-11 ***
    ## accident_count -0.1114160  0.0071092 -15.672  < 2e-16 ***
    ## route          -0.0194474  0.0007561 -25.720  < 2e-16 ***
    ## season         -2.6393521  0.4067065  -6.490 9.02e-11 ***
    ## PC1            -1.8226588  0.1232226 -14.792  < 2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 14.39 on 9840 degrees of freedom
    ## Multiple R-squared:  0.1129, Adjusted R-squared:  0.1123 
    ## F-statistic: 178.9 on 7 and 9840 DF,  p-value: < 2.2e-16
