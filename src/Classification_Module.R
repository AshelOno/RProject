# Classification Module
Classification_Analysis <- function(data) {
  library(caret)
  library(rpart)
  library(rpart.plot)
  library(dplyr)
  
  # Step 1: Transform the dataset for classification
  cat("Preparing dataset for classification...\n")
  # Add a binary classification target: "High" (Above median) and "Low" (Below median) price
  data <- data %>%
    mutate(
      Price_Class = ifelse(Price > median(Price, na.rm = TRUE), "High", "Low"),
      Price_Class = factor(Price_Class, levels = c("Low", "High"))  # Ensure correct order
    )
  
  # Step 2: Split the data into training and testing sets
  set.seed(123)  # For reproducibility
  train_index <- createDataPartition(data$Price_Class, p = 0.8, list = FALSE)
  train_data <- data[train_index, ]
  test_data <- data[-train_index, ]
  
  cat("Training data size: ", nrow(train_data), "\n")
  cat("Testing data size: ", nrow(test_data), "\n\n")
  
  # Step 3: Train a decision tree classifier
  cat("Training Decision Tree Classifier...\n")
  tree_model <- rpart(
    Price_Class ~ Airline + Source_City + Destination_City + Stops + Class +
      Duration + Days_Left + Departure_Time_Category + IsWeekend + price_per_hour,
    data = train_data,
    method = "class",
    control = rpart.control(cp = 0.01)  # Set complexity parameter
  )
  
  # Visualize the tree
  rpart.plot(tree_model, main = "Decision Tree for Price Classification")
  
  # Step 4: Make predictions on the test dataset
  cat("Making predictions on the test dataset...\n")
  predicted_classes <- predict(tree_model, test_data, type = "class")
  
  # Step 5: Evaluate the model
  cat("Evaluating the model...\n")
  confusion_matrix <- confusionMatrix(predicted_classes, test_data$Price_Class)
  
  # Extract evaluation metrics
  accuracy <- confusion_matrix$overall["Accuracy"]
  precision <- confusion_matrix$byClass["Pos Pred Value"]  # Precision
  recall <- confusion_matrix$byClass["Sensitivity"]        # Recall
  f1_score <- 2 * ((precision * recall) / (precision + recall))
  
  cat("\nModel Evaluation Metrics:\n")
  cat("Accuracy: ", accuracy, "\n")
  cat("Precision: ", precision, "\n")
  cat("Recall: ", recall, "\n")
  cat("F1 Score: ", f1_score, "\n\n")
  
  # Step 6: Return results
  return(list(
    model = tree_model,
    confusion_matrix = confusion_matrix,
    predictions = data.frame(Actual = test_data$Price_Class, Predicted = predicted_classes),
    metrics = data.frame(Accuracy = accuracy, Precision = precision, Recall = recall, F1_Score = f1_score)
  ))
}
