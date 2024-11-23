<<<<<<< HEAD
# Required Libraries
library(dplyr)
library(caret)
library(rpart)
library(rpart.plot)

# Classification Analysis Function
Classification_Analysis <- function(data) {
  # Step 1: Data Preparation
  prepared_data <- Data_Preparation(data)
=======
# Classification Analysis Function
Classification_Analysis <- function(file_path = "data/Clean_Dataset.csv") {
  # Load required libraries
  library(dplyr)
  library(caret)
  library(rpart)
  library(rpart.plot)
  
  # Step 1: Data Preparation
  prepared_data <- Data_Preparation(file_path)
>>>>>>> 87cfa3085cf9844c5258e2d86c702a672cf56cac
  
  # Step 2: Feature Engineering
  engineered_data <- Feature_Engineering(prepared_data)
  
  # Step 3: Create a binary classification target ('High' vs 'Low') for Price
<<<<<<< HEAD
  engineered_data$Price_Class <- ifelse(engineered_data$Price > median(engineered_data$Price, na.rm = TRUE), 'High', 'Low')
  
  # Step 4: Select features and prepare the data for classification
=======
  engineered_data <- engineered_data %>%
    mutate(Price_Class = ifelse(Price > median(Price, na.rm = TRUE), 'High', 'Low'))
  
  # Ensure Price_Class is a factor
  engineered_data$Price_Class <- factor(engineered_data$Price_Class, levels = c("Low", "High"))
  
  # Step 4: Select features and filter the data for classification
>>>>>>> 87cfa3085cf9844c5258e2d86c702a672cf56cac
  classification_data <- engineered_data %>%
    select(Price, Duration, price_per_hour, Days_Left, Price_Class) %>%
    filter(!is.na(Price_Class))  # Remove rows with missing Price_Class
  
<<<<<<< HEAD
=======
  # Check for sufficient data
  if (nrow(classification_data) < 2) {
    stop("Insufficient data for classification. Please check the input dataset.")
  }
  
>>>>>>> 87cfa3085cf9844c5258e2d86c702a672cf56cac
  # Step 5: Split the data into training and test sets
  set.seed(123)  # For reproducibility
  train_index <- createDataPartition(classification_data$Price_Class, p = 0.8, list = FALSE)
  train_data <- classification_data[train_index, ]
  test_data <- classification_data[-train_index, ]
  
  # Step 6: Train a Decision Tree model
<<<<<<< HEAD
  tree_model <- rpart(Price_Class ~ Price + Duration + price_per_hour + Days_Left, 
                      data = train_data, 
                      method = "class")
  
  # Step 7: Visualize the Decision Tree
  rpart.plot(tree_model)
  
  # Step 8: Predict on the test set
  predicted_classes <- predict(tree_model, test_data, type = "class")
  
  # Ensure both predicted and actual labels are factors with the same levels
  predicted_classes <- factor(predicted_classes, levels = c("Low", "High"))
  test_data$Price_Class <- factor(test_data$Price_Class, levels = c("Low", "High"))
=======
  tree_model <- rpart(
    Price_Class ~ Price + Duration + price_per_hour + Days_Left,
    data = train_data,
    method = "class"  # Classification method
  )
  
  # Step 7: Visualize the Decision Tree
  rpart.plot(tree_model, main = "Decision Tree for Price Classification")
  
  # Step 8: Predict on the test set
  predicted_classes <- predict(tree_model, test_data, type = "class")
  predicted_classes <- factor(predicted_classes, levels = levels(test_data$Price_Class))
>>>>>>> 87cfa3085cf9844c5258e2d86c702a672cf56cac
  
  # Step 9: Evaluate the model using a confusion matrix
  confusion_matrix <- confusionMatrix(predicted_classes, test_data$Price_Class)
  
<<<<<<< HEAD
  # Step 10: Return results as a list
  return(list(
    model = tree_model,                # The trained Decision Tree model
    confusion_matrix = confusion_matrix,  # Confusion matrix for evaluation
    predictions = data.frame(          # Predicted vs Actual data
      Actual = test_data$Price_Class,
      Predicted = predicted_classes
    )
  ))
}

=======
  # Save Results
  # Predicted vs Actual
  predicted_vs_actual <- data.frame(
    Predicted = predicted_classes,
    Actual = test_data$Price_Class
  )
  write.csv(predicted_vs_actual, "results/Classification_Results.csv", row.names = FALSE)
  
  # Save Confusion Matrix
  confusion_matrix_table <- as.data.frame(confusion_matrix$table)
  write.csv(confusion_matrix_table, "results/Confusion_Matrix.csv", row.names = FALSE)
  
  # Save Model
  saveRDS(tree_model, "results/Tree_Model.rds")
  
  # Return the results
  return(list(
    model = tree_model,
    confusion_matrix = confusion_matrix,
    predicted_classes = predicted_classes,
    test_data = test_data
  ))
}

# Ensure the "results" folder exists
if (!dir.exists("results")) dir.create("results")

# Example: Run the analysis
results <- Classification_Analysis("data/Clean_Dataset.csv")
>>>>>>> 87cfa3085cf9844c5258e2d86c702a672cf56cac
