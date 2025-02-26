# Load necessary libraries
library(readxl)
library(dplyr)
library(lubridate)  # For date manipulation
library(stringr)    # For string operations

# Define the path to the folder
folder_path <- "C:/Users/hamidja/Downloads/Route70_Monthly_Reports"
output_file <- "C:/Users/hamidja/OneDrive - Lee County BoCC/Intern Project/MonthlyVolumeData.csv"

# List all .xlsx files in the folder
file_list <- list.files(path = folder_path, pattern = "\\.xlsx$", full.names = TRUE)

# Define the route (adjust as needed)
route <- "70"

# Function to extract the year and month from the file name
extract_year_month_from_filename <- function(filename) {
  # Extract the filename without the path and extension
  file_name <- str_remove(basename(filename), "\\.xlsx$")
  
  # Extract parts of the filename
  parts <- str_split(file_name, "_")[[1]]
  
  # Debugging: Print parts to understand the filename structure
  print(parts)
  
  # Ensure we have at least three parts and the format is correct
  if (length(parts) >= 3) {
    # Extract the month and year from the parts
    month_num <- as.numeric(parts[3])  # The second part should be the month
    year <- as.numeric(parts[length(parts)])  # Year is the last part
    
    # Debugging: Print extracted month and year
    print(paste("Extracted month:", month_num))
    print(paste("Extracted year:", year))
    
    # Convert month number to month name
    if (!is.na(month_num) && month_num >= 1 && month_num <= 12) {
      month_name <- month.name[month_num]
    } else {
      stop("Invalid month number in filename.")
    }
    
    return(list(year = year, month = month_name))
  } else {
    stop("Filename format is incorrect. Expected format: 'prefix_month_month_year'.")
  }
}

# Function to read an Excel file and extract data starting from specific cells
read_and_extract <- function(file_path) {
  # Extract the year and month from the file name
  date_info <- extract_year_month_from_filename(file_path)
  year <- date_info$year
  month <- date_info$month
  
  # Read the entire Excel file without using the first row as column names
  data <- read_excel(file_path, col_names = FALSE)
  
  # Define the starting row for extracting data
  start_row <- 10  # Starting from row 11 (A11 for Day, Z11 for Total)
  
  # Extract the 'Day' column (first column starting from A11)
  day_column <- data[start_row:nrow(data), 1, drop = FALSE]
  # Extract the 'Total' column (column Z starting from Z11)
  total_column <- data[start_row:nrow(data), 26, drop = FALSE]  # Z is the 26th column
  
  # Rename the columns
  colnames(day_column) <- "Day"
  colnames(total_column) <- "Total"
  
  # Combine the two columns into a single data frame
  combined_data <- bind_cols(day_column, total_column)
  
  # Add 'Date' column by combining day, month, and year
  combined_data <- combined_data %>%
    mutate(Date = as.Date(paste(year, month, Day, sep = "-"), format = "%Y-%B-%d"))
  
  # Add 'Route' column
  combined_data <- combined_data %>%
    mutate(Route = route)
  
  return(combined_data)
}

# Apply the function to all files
data_list <- lapply(file_list, read_and_extract)

# Optionally, combine all data frames into one
new_data <- bind_rows(data_list)

# Check if the CSV file already exists
if (file.exists(output_file)) {
  # Read the existing data
  existing_data <- read.csv(output_file)
  
  # Combine the existing data with the new data
  combined_data <- bind_rows(existing_data, new_data)
} else {
  # If the file doesn't exist, use the new data as is
  combined_data <- new_data
}

# Write the combined data to the CSV file
write.csv(combined_data, file = output_file, row.names = FALSE)

# Print a message to indicate completion
print("Data has been appended to the CSV file.")