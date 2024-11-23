# Clustering_Module.R

library(cluster)
library(factoextra)

# Clustering Analysis function
Clustering_Analysis <- function(data) {
  # Select relevant columns for clustering
  clustering_data <- data %>% select(Price, Duration, Days_Left)
  
  # Scale the data
  scaled_data <- scale(clustering_data)
  
  # K-means clustering (using 3 clusters as an example)
  set.seed(123)
  kmeans_result <- kmeans(scaled_data, centers = 3, nstart = 25)
  
  # Add cluster labels to data
  data$Cluster <- as.factor(kmeans_result$cluster)
  
  # Visualize clusters
  fviz_cluster(kmeans_result, data = scaled_data, geom = "point", main = "Clustering of Flights")
  
  return(data)
}
