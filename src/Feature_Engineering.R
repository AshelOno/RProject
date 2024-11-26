# Feature_Engineering.R
library(dplyr)
library(lubridate)

# Feature Engineering function
Feature_Engineering <- function(data) {
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
      price_per_hour = as.numeric(price_per_hour),
      Departure_Time = as.POSIXct(Departure_Time, format = "%Y-%m-%d %H:%M:%S", tz = "UTC"),
      Arrival_Time = as.POSIXct(Arrival_Time, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
    )
  
  data <- data %>%
    mutate(
      Departure_Day = wday(Departure_Time, label = TRUE, abbr = TRUE),
      Departure_Hour = hour(Departure_Time),
      Arrival_Hour = hour(Arrival_Time),
      price_per_hour = ifelse(Duration > 0, Price / Duration, NA),
      Departure_Time_Category = case_when(
        Departure_Hour >= 5 & Departure_Hour < 8 ~ "Early_Morning",
        Departure_Hour >= 8 & Departure_Hour < 12 ~ "Morning",
        Departure_Hour >= 12 & Departure_Hour < 17 ~ "Afternoon",
        Departure_Hour >= 17 & Departure_Hour < 21 ~ "Evening",
        TRUE ~ "Night"
      ),
      IsWeekend = ifelse(weekdays(Departure_Time) %in% c("Saturday", "Sunday"), 1, 0)
    )
  
  data$Departure_Time_Category <- factor(
    data$Departure_Time_Category,
    levels = c("Early_Morning", "Morning", "Afternoon", "Evening", "Night"),
    ordered = TRUE
  )
  
  data$price_per_hour <- data$Price / data$Duration
  
  return(data)
}