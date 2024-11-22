# src/Clustering_Module.R
Clustering_Analysis <- function(data) {
  library(dplyr)
  library(cluster)
  library(factoextra)
  
  # Run Data Preparation
  prepared_data <- Data_Preparation()
  
  # Perform feature engineering
  engineered_data <- Feature_Engineering(prepared_data)
  
  
}
  
  
  
  
  
  
  
  
  
  
  
  
