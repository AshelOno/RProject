# Main.R
source("src/Data_Preparation.R")
source("src/Feature_Engineering.R")
source("src/Regression_Module.R")
source("src/Classification_Module.R")
source("src/Clustering_Module.R")

# Step 1: Data Preparation
cleaned_data <- Data_Preparation()

# Step 2: Feature Engineering
engineered_data <- Feature_Engineering(cleaned_data)

# Step 3: Regression Analysis
Regression_Results <- Regression_Analysis(engineered_data)

# Step 4: Classification Analysis
Classification_Results <- Classification_Analysis(engineered_data)

# Step 5: Clustering Analysis
Clustering_Results <- Clustering_Analysis(engineered_data)

# Save and summarize results
write.csv(Regression_Results, "results/Regression_Results.csv", row.names = FALSE)
write.csv(Classification_Results, "results/Classification_Results.csv", row.names = FALSE)
write.csv(Clustering_Results, "results/Clustering_Results.csv", row.names = FALSE)

cat("All analysis completed and results saved in the 'results' folder.\n")

