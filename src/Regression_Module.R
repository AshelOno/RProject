# src/Regression_Module.R
Regression_Analysis <- function(data) {
  library(caret)
  library(dplyr)
  
  # Select predictors and target variable
  regression_data <- data %>%
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
  
  # Calculate RMSE (Root Mean Squared Error)
  rmse <- sqrt(mean((predicted_prices - test_data$Price)^2))
  print(paste("RMSE: ", rmse))
  
  # Combine predictions with actual prices and calculate residuals
  results <- data.frame(Actual = test_data$Price, Predicted = predicted_prices)
  results$residuals <- results$Actual - results$Predicted
  
  # Save results to a CSV file
  write.csv(results, "results/regression_results.csv")
  
  # Create a data frame for saving results
  results_df <- test_data %>%
    mutate(
      Predicted_Price = predicted_prices,  # Add predictions
      Residuals = Price - predicted_prices  # Calculate residuals
    )
  
  # Optionally print the results data frame (for verification)
  print(head(results_df))
  
  # Return the regression model and predictions
  return(list(model = regression_model, results = results_df))
}