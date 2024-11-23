# src/Classification_Module.R
  Classification_Analysis <- function(data) {
    # Load required libraries
    library(dplyr)
    library(caret)
    library(rpart)
    library(rpart.plot)
    
    # Step 1: Data Preparation
    prepared_data <- Data_Preparation()
    
    # Step 2: Feature Engineering
    engineered_data <- Feature_Engineering(prepared_data)
    
    # Step 3: Create a binary classification target ('High' vs 'Low') for Price
    # Define 'High' price if it is above the median, else 'Low'
    engineered_data$Price_Class <- ifelse(engineered_data$Price > median(engineered_data$Price, na.rm = TRUE), 'High', 'Low')
    
    # Step 4: Select features and prepare the data for classification
    classification_data <- engineered_data %>%
      select(Price, Duration, price_per_hour, Days_Left, Price_Class) %>%
      filter(!is.na(Price_Class))  # Remove rows with missing Price_Class
    
    # Step 5: Split the data into training and test sets
    set.seed(123)  # For reproducibility
    train_index <- createDataPartition(classification_data$Price_Class, p = 0.8, list = FALSE)
    train_data <- classification_data[train_index, ]
    test_data <- classification_data[-train_index, ]
    
    # Step 6: Train a Decision Tree model
    tree_model <- rpart(Price_Class ~ Price + Duration + price_per_hour + Days_Left, 
                        data = train_data, 
                        method = "class")  # Classification method
    
    # Step 7: Visualize the Decision Tree
    rpart.plot(tree_model)
    
    # Step 8: Predict on the test set
    predicted_classes <- predict(tree_model, test_data, type = "class")
    
    # Ensure both predicted and actual labels are factors with the same levels
    predicted_classes <- factor(predicted_classes, levels = c("Low", "High"))
    test_data$Price_Class <- factor(test_data$Price_Class, levels = c("Low", "High"))
    
    # Step 9: Evaluate the model using a confusion matrix
    confusion_matrix <- confusionMatrix(predicted_classes, test_data$Price_Class)
    
    # Step 10: Return results
    return(list(
      model = tree_model,                # The trained Decision Tree model
      confusion_matrix = confusion_matrix,  # Confusion matrix for evaluation
      predicted_classes = predicted_classes  # Predicted classes for the test set
    ))
  }
  

  