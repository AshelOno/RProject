Clustering_Analysis <- function(data) {
  # Load required libraries
  library(dplyr)          # For data manipulation
  library(cluster)        # For clustering algorithms
  library(factoextra)     # For visualizing clusters
  
  # Step 1: Select Numeric Features for Clustering
  clustering_data <- data %>%
    select(Price, Duration, price_per_hour) %>%  # Select relevant numeric columns for clustering
    na.omit()  # Remove rows with missing values to avoid errors in clustering
  
  # Step 2: Normalize the Data
  scaled_data <- scale(clustering_data)  # Scale the data to normalize features
  print("Scaled data structure:")
  print(str(scaled_data))  # Debugging: Check structure of the scaled data
  
  # Step 3: Apply K-Means Clustering
  set.seed(123)  # For reproducibility of clustering results
  kmeans_result <- kmeans(scaled_data, centers = 3, nstart = 25)  # Run k-means with 3 clusters
  print("Kmeans result structure:")
  print(str(kmeans_result))  # Debugging: Check structure of the clustering result
  
  # Step 4: Add Cluster Labels to the Dataset
  data$Cluster <- kmeans_result$cluster  # Assign cluster labels to the original dataset
  
  # Step 5: Analyze Clusters
  cluster_summary <- data %>%
    group_by(Cluster) %>%
    summarize(
      Avg_Price = mean(Price, na.rm = TRUE),             # Average price per cluster
      Avg_Duration = mean(Duration, na.rm = TRUE),       # Average duration per cluster
      Avg_Price_Per_Hour = mean(price_per_hour, na.rm = TRUE),  # Average price per hour per cluster
      Count = n()                                        # Count of points in each cluster
    )
  print("Cluster Summary:")
  print(cluster_summary)  # Debugging: Verify cluster summary statistics
  
  # Step 6: Save Results
  if (!dir.exists("results")) dir.create("results")  # Create 'results' folder if it doesn't exist
  
  # Step 7: Visualize Clusters
  try({
    cluster_plot <- fviz_cluster(kmeans_result, data = scaled_data, geom = "point", main = "Clustering of Flights")
    print(cluster_plot)  # Display the cluster plot
    ggsave("results/Cluster_Plot.png", cluster_plot, width = 8, height = 6)  # Save the plot
  }, silent = TRUE)
  
  # Step 8: Return Results
  return(list(
    model = kmeans_result,               # The k-means model object
    clustered_data = data,               # Dataset with cluster labels
    cluster_summary = cluster_summary,   # Summary statistics for each cluster
    cluster_centers = kmeans_result$centers,  # Coordinates of cluster centers
    cluster_sizes = kmeans_result$size   # Number of points in each cluster
  ))
}