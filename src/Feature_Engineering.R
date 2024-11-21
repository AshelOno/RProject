# src/Feature_Engineering.R
Feature_Engineering <- function(data) {
  library(dplyr)
  library(lubridate)
  
# Ensure data types are correct  
  data <- data %>%
    mutate(
      Airline = as.factor(Airline),
      Source_City = as.factor(Source_City),
      Destination_City = as.factor(Destination_City),
      Stops = as.factor(Stops),
      Class = as.factor(Class),
      Price = as.numeric(Price),
      Duration = as.numeric(Duration),
      Days_Left = as.numeric(Days_Left),
      Departure_Time = as.POSIXct(Departure_Time, format = "%Y-%m-%d %H:%M:%S", tz = "UTC"),
      Arrival_Time = as.POSIXct(Arrival_Time, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
    )

  
  # Extract time-based features
  data <- data %>%
    mutate(
      Departure_Day = wday(Departure_Time, label = TRUE, abbr = TRUE),
      Departure_Month = month(Departure_Time, label = TRUE, abbr = TRUE),
      Departure_Hour = hour(Departure_Time),
      Arrival_Day = wday(Arrival_Time, label = TRUE, abbr = TRUE),
      Arrival_Hour = hour(Arrival_Time)
    )
  
  # Normalize price by duration
  data <- data %>%
    mutate(price_per_hour = Price / Duration)
  
  # Add distance features
  if (all(c("source_lat", "source_long", "dest_lat", "dest_long") %in% colnames(data))) {
    haversine_distance <- function(lat1, lon1, lat2, lon2) {
      R <- 6371
      dlat <- (lat2 - lat1) * pi / 180
      dlon <- (lon2 - lon1) * pi / 180
      a <- sin(dlat / 2)^2 + cos(lat1 * pi / 180) * cos(lat2 * pi / 180) * sin(dlon / 2)^2
      c <- 2 * atan2(sqrt(a), sqrt(1 - a))
      R * c
    }
    data <- data %>%
      mutate(distance_km = mapply(haversine_distance, source_lat, source_long, dest_lat, dest_long))
  } else {
    cat("Latitude and longitude data not available. Skipping distance features.\n")
  }
  
  # Convert categorical variables to binary features
  time_levels <- c("Early_Morning", "Morning", "Afternoon", "Evening", "Night")
  data <- data %>%
    mutate(Departure_Time = factor(Departure_Time, levels = time_levels, ordered = TRUE))
  
  for (time_level in time_levels) {
    column_name <- paste0("Departure_", time_level)
    data[[column_name]] <- ifelse(data$Departure_Time == time_level, 1, 0)
  }
  
  # Save the engineered dataset
  # Ensure results directory exists
  if (!dir.exists("results")) {
    dir.create("results")
  }
  

  write.csv(data, "results/Engineered_Dataset_with_Features.csv", row.names = FALSE)
  cat("Feature Engineering Complete! Dataset saved.\n")
  
  return(data)
}
