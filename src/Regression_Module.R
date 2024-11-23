# src/Regression_Module.R
Regression_Analysis <- function(data) {
  library(caret)
  
  # Select predictors and target variable
  regression_data <- engineered_data %>%
    select(Price, Duration, price_per_hour, Days_Left)  # Target and numeric predictors
  
  # Ensure all columns are numeric
  regression_data <- regression_data %>%
    mutate(across(everything(), as.numeric))
  
  # Output data is ready for regression
  print(head(regression_data))
  
  
}