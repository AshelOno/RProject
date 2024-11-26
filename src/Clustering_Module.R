Clustering_Analysis <- function(data) {
  # Load required libraries
  library(dplyr)
  library(cluster)
  library(factoextra)  # For visualizing clusters
  
  # Step 1: Select Numeric Features for Clustering
  clustering_data <- data %>%
    select(Price, Duration, price_per_hour) %>%  # Select relevant numeric columns
    na.omit()  # Remove rows with missing values
  
  # Step 2: Normalize the Data
  scaled_data <- scale(clustering_data)  # Scale the data to normalize features
  print("Scaled data structure:")
  print(str(scaled_data))  # Debugging: Check structure of scaled_data
  
  # Step 3: Apply K-Means Clustering
  set.seed(123)  # For reproducibility
  kmeans_result <- kmeans(scaled_data, centers = 3, nstart = 25)  # Create 3 clusters
  print("Kmeans result structure:")
  print(str(kmeans_result))  # Debugging: Check structure of kmeans_result
  
  # Step 4: Add Cluster Labels to the Dataset
  data$Cluster <- kmeans_result$cluster  # Add cluster labels to 'data'
  
  # Step 5: Analyze Clusters
  cluster_summary <- data %>%
    group_by(Cluster) %>%
    summarize(
      Avg_Price = mean(Price, na.rm = TRUE),
      Avg_Duration = mean(Duration, na.rm = TRUE),
      Avg_Price_Per_Hour = mean(price_per_hour, na.rm = TRUE),
      Count = n()
    )
  print("Cluster Summary:")
  print(cluster_summary)  # Debugging: Check cluster_summary
  
  # Step 6: Save Results
  if (!dir.exists("results")) dir.create("results")  # Create 'results' folder if it doesn't exist
  
  # Save the dataset with Cluster Labels
  write.csv(data, "results/Clustering_Results.csv", row.names = FALSE)  # Save the data with 'Cluster' column
  
  # Save Cluster Summary
    write.csv(cluster_summary, "results/Cluster_Summary.csv", row.names = FALSE)
  
  # Save Cluster Centers (from kmeans_result)
  cluster_centers <- as.data.frame(kmeans_result$centers)  # Convert to data frame
  print(str(cluster_centers))  # Debugging: Check structure before saving
  write.csv(cluster_centers, "results/Cluster_Centers.csv", row.names = TRUE)  # Save cluster centers
  
  # Save Cluster Sizes (from kmeans_result)
  cluster_sizes <- data.frame(Cluster = 1:length(kmeans_result$size), Size = kmeans_result$size)  # Convert to data frame
  print(str(cluster_sizes))  # Debugging: Check structure before saving
  write.csv(cluster_sizes, "results/Cluster_Sizes.csv", row.names = FALSE)  # Save cluster sizes
  
  # Check the structure of the data (final dataset with cluster labels)
  str(data)  # Ensure it's a valid data frame
  
  # Step 7: Visualize Clusters
  try({
    cluster_plot <- fviz_cluster(kmeans_result, data = scaled_data, geom = "point", main = "Clustering of Flights")
    print(cluster_plot)
    ggsave("results/Cluster_Plot.png", cluster_plot, width = 8, height = 6)
  }, silent = TRUE)
  
  # Step 8: Return Results
  return(list(
    model = kmeans_result,               # The kmeans model object
    clustered_data = data,               # Data with cluster labels
    cluster_summary = cluster_summary,   # Summary of clusters
    cluster_centers = kmeans_result$centers,  # Cluster centers
    cluster_sizes = kmeans_result$size  # Cluster sizes
  ))
}