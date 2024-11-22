


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
Classification_Results <- Classification_Analysis(engineered_data)
write.csv(Classification_Results, "results/Classification_Results.csv", row.names = FALSE)
cat("Classification Analysis Completed. Results saved in 'results/Classification_Results.csv'.\n\n")

# Step 6: Clustering Analysis
cat("Starting Clustering Analysis...\n")
Clustering_Results <- Clustering_Analysis(engineered_data)
write.csv(Clustering_Results, "results/Clustering_Results.csv", row.names = FALSE)
cat("Clustering Analysis Completed. Results saved in 'results/Clustering_Results.csv'.\n\n")

# Final Summary
cat("All analysis completed successfully!\n")
cat("Check the 'results' folder for outputs and the visualizations displayed during execution.\n")

