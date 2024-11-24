# src/Regression_Module.R
Regression_Analysis <- function(data) {
  library(caret)
  
  # Run Data Preparation
  prepared_data <- Data_Preparation()
  
  # Perform feature engineering
  engineered_data <- Feature_Engineering(prepared_data)
  
  # Select predictors and target variable
  regression_data <- engineered_data %>%
    select(Price, Duration, price_per_hour, Days_Left)  # Target and numeric predictors
  
  # Ensure all columns are numeric
  regression_data <- regression_data %>%
    mutate(across(everything(), as.numeric))
  
  # Output data is ready for regression
  print(head(regression_data))
  
  # Split data into training and testing sets
  set.seed(123)
  train_index <- createDataPartition(regression_data$Price, p = 0.8, list = FALSE)
  train_data <- regression_data[train_index, ]
  test_data <- regression_data[-train_index, ]
  
  # Fit a linear regression model
  regression_model <- lm(Price ~ Duration + price_per_hour + Days_Left, data = train_data)
  
  # Print summary of the model
  print(summary(regression_model))
  # Predict on the test set
  predicted_prices <- predict(regression_model, test_data)
  
  # Evaluate model performance using RMSE
  # Ensure that test_data$Price and predicted_prices are numeric and aligned
  if (!is.numeric(test_data$Price) || !is.numeric(predicted_prices)) {
    stop("Error: test_data$Price or predicted_prices is not numeric.")
  }
  
  # Check for NA values and remove them if necessary
  if (any(is.na(test_data$Price)) || any(is.na(predicted_prices))) {
    warning("Missing values detected. They will be excluded from RMSE calculation.")
    valid_indices <- !is.na(test_data$Price) & !is.na(predicted_prices)
    actual_prices <- test_data$Price[valid_indices]
    predicted_prices <- predicted_prices[valid_indices]
  } else {
    actual_prices <- test_data$Price
  }
  
  # Calculate RMSE
  rmse <- sqrt(mean((actual_prices - predicted_prices)^2))
  
  # Print RMSE
  cat("Root Mean Square Error (RMSE):", rmse, "\n")
  
  # Create a data frame for saving results
  results_df <- test_data %>%
    mutate(
      Predicted_Price = predicted_prices,  # Add predictions
      Residuals = Price - predicted_prices # Calculate residuals
    )
  
  # Optionally print the results data frame (for verification)
  print(head(results_df))
  # Return the regression model and predictions
  return(list(model = regression_model, results = results_df))
  
  
  # Example usage
  regression_result <- Regression_Analysis(your_dataset)
  
}