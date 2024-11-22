Data_Visualization <- function(data) {
  library(ggplot2)
  library(dplyr)
  library(scales)  # For better axis scaling
  
  # 1. Distribution of Price
  ggplot(data, aes(x = Price)) +
    geom_histogram(bins = 30, fill = "steelblue", color = "black", alpha = 0.7) +
    labs(title = "Price Distribution", x = "Price", y = "Frequency") +
    theme_minimal()
  
  # 2. Average Price by Airline
  ggplot(data %>% group_by(Airline) %>% summarize(Average_Price = mean(Price, na.rm = TRUE)), 
         aes(x = reorder(Airline, -Average_Price), y = Average_Price, fill = Airline)) +
    geom_bar(stat = "identity", color = "black", alpha = 0.7) +
    coord_flip() +
    labs(title = "Average Price by Airline", x = "Airline", y = "Average Price") +
    theme_minimal() +
    theme(legend.position = "none")
  
  # 3. Price vs. Duration
  ggplot(data, aes(x = Duration, y = Price, color = Class)) +
    geom_point(alpha = 0.6) +
    geom_smooth(method = "lm", color = "red", linetype = "dashed") +
    labs(title = "Price vs Duration by Class", x = "Duration (Hours)", y = "Price") +
    theme_minimal()
  
  # 4. Departure Day Analysis
  ggplot(data, aes(x = Departure_Day)) +
    geom_bar(aes(fill = Departure_Day), color = "black", alpha = 0.7) +
    labs(title = "Flight Distribution by Departure Day", x = "Day of the Week", y = "Count") +
    theme_minimal() +
    theme(legend.position = "none")
  
  # 5. Price per Hour Analysis
  ggplot(data, aes(x = price_per_hour)) +
    geom_histogram(bins = 30, fill = "seagreen", color = "black", alpha = 0.7) +
    labs(title = "Price Per Hour Distribution", x = "Price Per Hour", y = "Frequency") +
    theme_minimal()
  
  # 6. Flight Class Distribution
  ggplot(data, aes(x = Class, fill = Class)) +
    geom_bar(color = "black", alpha = 0.7) +
    labs(title = "Distribution of Flight Classes", x = "Class", y = "Count") +
    theme_minimal() +
    theme(legend.position = "none")
  
  # 7. Distance vs Price
  if ("distance_km" %in% colnames(data)) {
    ggplot(data, aes(x = distance_km, y = Price, color = Airline)) +
      geom_point(alpha = 0.6) +
      labs(title = "Price vs Distance by Airline", x = "Distance (KM)", y = "Price") +
      theme_minimal()
  } else {
    cat("Distance feature not available. Skipping Price vs Distance visualization.\n")
  }
  
  # 8. Departure Hour Analysis
  ggplot(data, aes(x = Departure_Hour, fill = ..count..)) +
    geom_histogram(bins = 24, color = "black", alpha = 0.7) +
    scale_x_continuous(breaks = 0:23) +
    labs(title = "Flight Departure Times", x = "Hour of the Day", y = "Frequency") +
    theme_minimal()
  
  # 9. Arrival Hour Analysis
  ggplot(data, aes(x = Arrival_Hour, fill = ..count..)) +
    geom_histogram(bins = 24, color = "black", alpha = 0.7) +
    scale_x_continuous(breaks = 0:23) +
    labs(title = "Flight Arrival Times", x = "Hour of the Day", y = "Frequency") +
    theme_minimal()
  
  # 10. Class Distribution by Departure Day
  ggplot(data, aes(x = Departure_Day, fill = Class)) +
    geom_bar(position = "dodge", color = "black", alpha = 0.7) +
    labs(title = "Class Distribution by Departure Day", x = "Departure Day", y = "Count") +
    theme_minimal()
  
  cat("Visualizations created successfully.\n")
}