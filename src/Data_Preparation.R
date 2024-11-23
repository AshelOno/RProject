Data_Preparation <- function() {
  library(readr)
  library(dplyr)
  
  # Load the dataset
  data <- read_csv("data/Clean_Dataset.csv")
  
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
  
  # Step 4: Outlier Detection and Removal
  # Calculate Q1, Q3, and IQR for the 'Price' column
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
