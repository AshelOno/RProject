# Main Script


# Source the modularized scripts
source("src/Data_Preparation.R")
source("src/Feature_Engineering.R")
source("src/Data_Visualization.R")
source("src/Regression_Module.R")
source("src/Classification_Module.R")
source("src/Clustering_Module.R")

# Step 1: Data Preparation
cat("Starting Data Preparation...\n")
cleaned_data <- Data_Preparation()
cat("Data Preparation Completed.\n\n")

# Step 2: Feature Engineering
cat("Starting Feature Engineering...\n")
engineered_data <- Feature_Engineering(cleaned_data)
cat("Feature Engineering Completed.\n\n")

# Step 3: Data Visualization
cat("Starting Data Visualization...\n")
Data_Visualization(engineered_data)
cat("Data Visualization Completed.\n\n")

# Step 4: Regression Analysis
cat("Starting Regression Analysis...\n")
Regression_Results <- Regression_Analysis(engineered_data)
write.csv(Regression_Results, "results/Regression_Results.csv", row.names = FALSE)
cat("Regression Analysis Completed. Results saved in 'results/Regression_Results.csv'.\n\n")

# Step 5: Classification Analysis
cat("Starting Classification Analysis...\n")
tryCatch({
  Classification_Results <- Classification_Analysis(engineered_data)
  cm_df <- as.data.frame(Classification_Results$confusion_matrix$table)
  write.csv(cm_df, "results/Classification_Results.csv", row.names = FALSE)
  
  predictions_df <- Classification_Results$predictions
  write.csv(predictions_df, "results/Classification_Predictions.csv", row.names = FALSE)
  cat("Classification Analysis Completed. Results saved in 'results/Classification_Results.csv'.\n\n")
}, error = function(e) {
  cat("Error during Classification Analysis: ", e$message, "\n")
})

Classification_Results <- Classification_Analysis(engineered_data)

# Save predictions to a CSV file
write.csv(Classification_Results$predictions, "results/Classification_Predictions.csv", row.names = FALSE)

# Save confusion matrix table to a CSV file
conf_matrix_df <- as.data.frame(Classification_Results$confusion_matrix$table)
write.csv(conf_matrix_df, "results/Confusion_Matrix.csv", row.names = FALSE)

# Save the decision tree model as an RDS file for later use
saveRDS(Classification_Results$model, "results/Tree_Model.rds")

cat("Classification Analysis Completed. Predictions and Confusion Matrix saved in 'results/' folder.\n\n")

# Step 6: Clustering Analysis
cat("Starting Clustering Analysis...\n")
Clustering_Results <- Clustering_Analysis(engineered_data)
write.csv(Clustering_Results, "results/Clustering_Results.csv", row.names = FALSE)
cat("Clustering Analysis Completed. Results saved in 'results/Clustering_Results.csv'.\n\n")
