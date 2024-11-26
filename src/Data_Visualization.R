Data_Visualization <- function(data) {
  library(ggplot2)
  library(dplyr)
  library(scales)
  library(corrplot)
  
  # Define a custom color palette
  custom_palette <- c("#1f77b4", "#ff7f0e", "#2ca02c", "#d62728", "#9467bd", "#e377c2")
  
  # Clean the data: Remove rows with missing values in key columns
  cleaned_data <- data %>%
    filter(!is.na(Price), !is.na(Duration), !is.na(Airline), !is.na(Class), !is.na(Stops))
  
  # 1. Histogram of Price Distribution
  price_dist_plot <- ggplot(cleaned_data, aes(x = Price)) +
    geom_histogram(bins = 30, fill = "steelblue", color = "black", alpha = 0.7) +
    labs(title = "Price Distribution", x = "Price", y = "Frequency") +
    scale_x_continuous(labels = dollar_format(prefix = "$")) +
    theme_minimal()
  print(price_dist_plot)
  Sys.sleep(2)
  
  # 2. Box Plot: Price by Stops
  price_stops_plot <- ggplot(cleaned_data, aes(x = factor(Stops), y = Price, fill = factor(Stops))) +
    geom_boxplot(outlier.shape = 8, outlier.size = 3) +
    labs(title = "Price by Stops", x = "Number of Stops", y = "Price") +
    scale_fill_manual(values = custom_palette) +
    theme_minimal()
  print(price_stops_plot)
  Sys.sleep(2)
  
  # 3. Heatmap: Correlation between Numeric Columns
  heatmap_data <- cleaned_data %>%
    select_if(is.numeric)  # Select only numeric columns
  
  if (ncol(heatmap_data) > 1) {
    heatmap_data <- heatmap_data %>%
      select_if(~ sum(!is.na(.)) > 1)  # Remove columns with only NA values
    
    if (ncol(heatmap_data) > 1) {
      correlation_matrix <- cor(heatmap_data, use = "complete.obs")
      corrplot(correlation_matrix, method = "color", 
               tl.col = "black", tl.cex = 0.8, 
               addCoef.col = "black", number.cex = 0.7, 
               col = colorRampPalette(c("blue", "white", "red"))(200))
    } else {
      message("Insufficient numeric columns for meaningful correlation heatmap.")
    }
  } else {
    message("No numeric columns available for correlation heatmap.")
  }
  Sys.sleep(2)
  
  # 4. Box Plot: Price by Class
  price_class_plot <- ggplot(cleaned_data, aes(x = factor(Class), y = Price, fill = factor(Class))) +
    geom_boxplot(outlier.shape = 8, outlier.size = 3) +
    labs(title = "Price by Class", x = "Class", y = "Price") +
    scale_fill_manual(values = custom_palette) +
    theme_minimal()
  print(price_class_plot)
  Sys.sleep(2)
  
  # 5. Scatter Plot: Price vs Duration
  price_duration_plot <- ggplot(cleaned_data, aes(x = Duration, y = Price, color = Class)) +
    geom_point(alpha = 0.6) +
    geom_smooth(method = "lm", color = "red", linetype = "dashed") +
    labs(title = "Price vs Duration", x = "Duration (Hours)", y = "Price") +
    theme_minimal()
  print(price_duration_plot)
  Sys.sleep(2)
  
  # 6. Pie Chart: Flights by Class
  class_distribution_plot <- ggplot(cleaned_data, aes(x = "", fill = Class)) +
    geom_bar(stat = "count", width = 1) +
    coord_polar(theta = "y") +
    labs(title = "Distribution of Flights by Class") +
    scale_fill_manual(values = custom_palette) +
    theme_void()
  print(class_distribution_plot)
}
