# Main Script
#install.packages(c("caret", "cluster", "factoextra", "rpart", "ggplot2", "ggfortify", "scales"))

# Source the modularized scripts
  source("src/Data_Preparation.R")
  source("src/Feature_Engineering.R")
  source("src/Data_Visualization.R")
  source("src/Regression_Module.R")
  source("src/Classification_Module.R")
  source("src/Clustering_Module.R")

# Step 1: Data Preparation
  cat("Starting Data Preparation...\n")
  file_path <- "data/Clean_Dataset.csv" 
  cleaned_data <- Data_Preparation(file_path)
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
  # Save the results to a CSV
  write.csv(Regression_Results$results, "results/Regression_Results.csv", row.names = FALSE)
  cat("Regression Analysis Completed. Results saved in 'results/Regression_Results.csv'.\n\n")

# Step 5: Classification Analysis
  cat("Starting Classification Analysis...\n")
  Classification_Results <- Classification_Analysis(engineered_data)
  # Save predictions to a CSV file
  write.csv(Classification_Results$predictions, "results/Classification_Predictions.csv", row.names = FALSE)
  # Save confusion matrix as a CSV file
  conf_matrix_df <- as.data.frame(Classification_Results$confusion_matrix$table)
  write.csv(conf_matrix_df, "results/Confusion_Matrix.csv", row.names = FALSE)
  # Save the decision tree model as an RDS file
  saveRDS(Classification_Results$model, "results/Tree_Model.rds")
  # Save evaluation metrics
  write.csv(Classification_Results$metrics, "results/Classification_Metrics.csv", row.names = FALSE)
  cat("Classification Analysis Completed.\n")
  cat("Predictions, Confusion Matrix, and Metrics saved in 'results/' folder.\n")

  # Step 6: Clustering Analysis
  cat("Starting Clustering Analysis...\n")
  Clustering_Results <- Clustering_Analysis(engineered_data)  # Performs clustering analysis
  write.csv(Clustering_Results$clustered_data, "results/Clustering_Results.csv", row.names = FALSE)  # Saves data with cluster labels
  write.csv(Clustering_Results$cluster_summary, "results/Cluster_Summary.csv", row.names = FALSE)  # Saves summary statistics for clusters
  write.csv(as.data.frame(Clustering_Results$cluster_centers), "results/Cluster_Centers.csv", row.names = TRUE)  # Saves cluster centers
  cluster_sizes <- data.frame(Cluster = 1:length(Clustering_Results$cluster_sizes), Size = Clustering_Results$cluster_sizes)  # Formats cluster sizes
  write.csv(cluster_sizes, "results/Cluster_Sizes.csv", row.names = FALSE)  # Saves cluster sizes
  cat("Clustering Analysis Completed. Results saved in 'results/' folder.\n\n")

  cat("Project successfully completed. All results have been saved and analyzed professionally.\n")
