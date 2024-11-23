Data_Preparation <- function(file_path) {  # Added file_path as a parameter
  library(readr)
  library(dplyr)
  
  # Load the dataset using the file_path argument
  data <- read_csv("data/Clean_Dataset.csv")  # Use the provided file_path
  
  # Rename columns for clarity
  colnames(data) <- c("Index", "Airline", "Flight", "Source_City", "Departure_Time",
                      "Stops", "Arrival_Time", "Destination_City", "Class",
                      "Duration", "Days_Left", "Price")
  
  # Check for missing values
  cat("Missing Values:\n")
  print(colSums(is.na(data)))
  
  # Add engineered features
  data$Duration_Hours <- data$Duration / 60  # Convert minutes to hours
  data$price_per_hour <- data$Price / data$Duration_Hours  # Price per hour
  
  # Remove rows with missing values
  data <- na.omit(data)
  
  # Outlier Detection and Removal
  Q1 <- quantile(data$Price, 0.25, na.rm = TRUE)
  Q3 <- quantile(data$Price, 0.75, na.rm = TRUE)
  IQR <- Q3 - Q1
  lower_bound <- Q1 - 1.5 * IQR
  upper_bound <- Q3 + 1.5 * IQR
  
  # Remove outliers
  data <- data %>%
    filter(Price >= lower_bound & Price <= upper_bound)
  
  # Visualize Price distribution using a boxplot
  boxplot(data$Price, main = "Boxplot of Prices", horizontal = TRUE)
  
  # Return the cleaned data
  return(data)
}
