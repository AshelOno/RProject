# Load required libraries
library(cluster)
library(factoextra)
library(dplyr)

# Clustering Analysis Function
Clustering_Analysis <- function(data) {
  # Ensure data contains relevant columns
  if (!all(c("Price", "Duration", "Days_Left") %in% names(data))) {
    stop("The data must contain 'Price', 'Duration', and 'Days_Left' columns.")
  }
  
  # Select relevant columns for clustering
  clustering_data <- data %>% select(Price, Duration, Days_Left)
  
  # Scale the data for normalization
  scaled_data <- scale(clustering_data)
  
  # Perform K-Means clustering (3 clusters as an example)
  set.seed(123)  # For reproducibility
  kmeans_result <- kmeans(scaled_data, centers = 3, nstart = 25)
  
  # Add cluster labels to the original data
  data$Cluster <- as.factor(kmeans_result$cluster)
  
  # Visualize clusters
  fviz_cluster(kmeans_result, data = scaled_data, geom = "point", main = "Clustering of Flights")
  
  # Ensure results directory exists
  if (!dir.exists("results")) {
    dir.create("results")
  }
  
  # Return clustered data
  return(data)
}

# Main Script
cat("Starting Clustering Analysis...\n")

# Assuming engineered_data is already prepared before calling this function
tryCatch({
  # Perform clustering analysis
  Clustering_Results <- Clustering_Analysis(engineered_data)
  
  # Write results to CSV
  output_path <- "results/Clustering_Results.csv"
  write.csv(Clustering_Results, output_path, row.names = FALSE)
  cat("Clustering results saved to:", output_path, "\n")
}, error = function(e) {
  cat("Error during clustering analysis or file writing:\n", e$message, "\n")
})

