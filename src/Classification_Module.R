# src/Classification_Module.R
Classification_Analysis <- function(data) {
  library(caret)
  
  # Run Data Preparation
  prepared_data <- Data_Preparation()
  
  # Perform feature engineering
  engineered_data <- Feature_Engineering(prepared_data)
  
  # Filter rows with a non-missing 'Class' column and ensure it's a factor
  classification_data <- engineered_data %>%
    filter(!is.na(Class)) %>%
    mutate(Class = as.factor(Class))  # Convert target variable to factor
  
  # Select relevant features
  classification_data <- classification_data %>%
    select(Class, Price, Duration, price_per_hour, Days_Left)  # Include predictors and target
  
  # Output data is ready for classification
  print(head(classification_data))
  
}
