# src/Clustering_Module.R
Clustering_Analysis <- function(data) {
  library(dplyr)
  library(cluster)
  library(factoextra)
  
  # Run Data Preparation
  prepared_data <- Data_Preparation()
  
  # Perform feature engineering
  engineered_data <- Feature_Engineering(prepared_data)
  
  # Select numeric features and scale them
  clustering_data <- engineered_data %>%
    select(Price, Duration, price_per_hour) %>%
    scale()  # Normalize data for clustering
  
  # Perform K-Means clustering
  set.seed(123)  # For reproducibility
  kmeans_model <- kmeans(clustering_data, centers = 3, nstart = 10)  # 3 clusters
  
  # Add cluster labels to the dataset
  engineered_data$Cluster <- kmeans_model$cluster
  
  # Visualize the clusters using fviz_cluster (pass original scaled data)
  fviz_cluster(kmeans_model, data = clustering_data, geom = "point") +
    ggtitle("K-Means Clustering of Flights")
  
  # Return the model and clustered data
  return(list(model = kmeans_model, clustered_data = engineered_data))
}
  
  
  
  
  
  
  
  
  
  
  
  
