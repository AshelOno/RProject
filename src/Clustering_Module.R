# src/Clustering_Module.R
Clustering_Analysis <- function(data) {
  # Load required libraries
  library(dplyr)
  library(cluster)
  library(factoextra)  # For cluster visualization
  
  # Step 1: Data Preparation
  prepared_data <- Data_Preparation()
  
  # Step 2: Feature Engineering
  engineered_data <- Feature_Engineering(prepared_data)
  
  # Step 3: Select Numeric Features and Scale Them
  clustering_data <- engineered_data %>%
    select(Price, Duration, price_per_hour) %>%  # Select relevant numeric columns
    scale()  # Normalize features
  
  # Step 4: Perform K-Means Clustering
  set.seed(123)  # For reproducibility
  kmeans_model <- kmeans(clustering_data, centers = 3, nstart = 10)  # Create 3 clusters
  
  # Step 5: Add Cluster Labels to the Dataset
  engineered_data$Cluster <- kmeans_model$cluster  # Add cluster assignments to the dataset
  
  # Step 6: Save Results to CSV Files
  if (!dir.exists("results")) dir.create("results")
  
  ## (a) Save the dataset with cluster assignments
  write.csv(engineered_data, "results/Clustering_Results.csv", row.names = FALSE)
  
  ## (b) Save the cluster centers
  cluster_centers <- as.data.frame(kmeans_model$centers)  # Convert cluster centers to a data frame
  write.csv(cluster_centers, "results/Cluster_Centers.csv", row.names = TRUE)
  
  ## (c) Save summarized statistics for each cluster
  cluster_summary <- engineered_data %>%
    group_by(Cluster) %>%
    summarize(
      Avg_Price = mean(Price, na.rm = TRUE),
      Avg_Duration = mean(Duration, na.rm = TRUE),
      Avg_Price_Per_Hour = mean(price_per_hour, na.rm = TRUE),
      Count = n()
    )
  write.csv(cluster_summary, "results/Cluster_Summary.csv", row.names = FALSE)
  
  # Step 7: Visualize Clusters using fviz_cluster (explicitly from factoextra)
  factoextra::fviz_cluster(kmeans_model, data = clustering_data, geom = "point") +
    ggtitle("K-Means Clustering of Flights")
  
  # Step 8: Return Results
  return(list(
    model = kmeans_model,               # The kmeans model object
    clustered_data = engineered_data,   # Data with cluster labels
    cluster_centers = cluster_centers,  # Cluster centers
    cluster_summary = cluster_summary   # Cluster summary
  ))
}














