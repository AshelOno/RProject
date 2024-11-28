# Define the Data_Preparation function
Data_Preparation <- function(file_path) {
  # Load necessary libraries
  library(readr)
  library(dplyr)
  
  # Load the dataset using the provided file_path
  data <- read_csv(file_path)
  
  # Rename columns for clarity
  colnames(data) <- c("Index", "Airline", "Flight", "Source_City", "Departure_Time",
                      "Stops", "Arrival_Time", "Destination_City", "Class",
                      "Duration", "Days_Left", "Price")
  
  # Check for missing values
  cat("Missing Values:\n")
  print(colSums(is.na(data)))
  
  # Handle missing values (optional): removing rows with NA in critical columns (Price, Duration, etc.)
  data <- data %>%
    filter(!is.na(Price), !is.na(Duration), !is.na(Departure_Time), !is.na(Arrival_Time))
  
  # Add engineered features
  # Convert Duration to hours
  data$Duration_Hours <- data$Duration / 60  # Convert minutes to hours
  # Price per hour (price divided by duration in hours)
  data$price_per_hour <- data$Price / data$Duration_Hours
  
  # Handle outliers in the 'Price' column using IQR
  Q1 <- quantile(data$Price, 0.25, na.rm = TRUE)
  Q3 <- quantile(data$Price, 0.75, na.rm = TRUE)
  IQR <- Q3 - Q1
  lower_bound <- Q1 - 1.5 * IQR
  upper_bound <- Q3 + 1.5 * IQR
  
  # Remove outliers from 'Price'
  data <- data %>%
    filter(Price >= lower_bound & Price <= upper_bound)
  
  # Visualize Price distribution using a boxplot
  boxplot(data$Price, main = "Boxplot of Prices", horizontal = TRUE)
  
  # Return the cleaned data
  return(data)
}
