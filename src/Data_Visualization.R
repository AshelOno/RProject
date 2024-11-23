Data_Visualization <- function(data) {
  library(ggplot2)
  library(dplyr)
  library(scales)
  library(corrplot)
  library(GGally)
  
  # Define a custom color palette (avoiding #8c564b and black)
  custom_palette <- c("#1f77b4", "#ff7f0e", "#2ca02c", "#d62728", "#9467bd", "#e377c2")
  
  # Clean data: Remove rows with NA in key columns (Price, Duration, Airline, Class, Stops)
  cleaned_data <- data %>% 
    filter(!is.na(Price), !is.na(Duration), !is.na(Airline), !is.na(Class), !is.na(Stops))
  
  # Histogram of Prices
  price_dist_plot <- ggplot(cleaned_data, aes(x = Price)) +
    geom_histogram(bins = 30, fill = "steelblue", color = "black", alpha = 0.7) +
    labs(title = "Price Distribution", x = "Price", y = "Frequency") +
    theme(panel.grid.major = element_line(color = "gray", linetype = "dashed")) +
    scale_x_continuous(labels = dollar_format(prefix = "$")) +
    scale_y_continuous(labels = scales::comma)
  print(price_dist_plot)
  Sys.sleep(2)
  
  # Average Price by Airline
  avg_price_plot <- ggplot(cleaned_data %>% 
                             group_by(Airline) %>% 
                             summarize(Average_Price = mean(Price, na.rm = TRUE)), 
                           aes(x = reorder(Airline, -Average_Price), y = Average_Price, fill = Airline)) +
    geom_bar(stat = "identity", color = "black", alpha = 0.7) +
    geom_text(aes(label = scales::dollar(Average_Price)), vjust = -0.3, color = "black", size = 3) +
    coord_flip() +
    labs(title = "Average Price by Airline", x = "Airline", y = "Average Price") +
    theme_minimal() +
    theme(legend.position = "none")
  print(avg_price_plot)
  Sys.sleep(2)
  
  # Bar Plot: Flights by Airline
  airline_bar_plot <- ggplot(cleaned_data, aes(x = Airline, fill = Airline)) +
    geom_bar(stat = "count", show.legend = FALSE) +
    geom_text(stat = 'count', aes(label = ..count..), vjust = -0.3, color = "black", size = 3) +
    labs(title = "Flights by Airline", x = "Airline", y = "Count") +
    scale_fill_manual(values = custom_palette) +
    scale_y_continuous(labels = scales::comma) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  print(airline_bar_plot)
  Sys.sleep(2)
  
  # Box Plot: Price by Stops
  price_stops_plot <- ggplot(cleaned_data, aes(x = factor(Stops), y = Price, fill = factor(Stops))) +
    geom_boxplot(outlier.shape = 8, outlier.size = 3) +
    labs(title = "Price by Stops", x = "Stops", y = "Price ($)") +
    scale_fill_manual(values = custom_palette) +
    scale_y_continuous(labels = dollar_format(prefix = "$")) +
    theme_minimal()
  print(price_stops_plot)
  Sys.sleep(2)
  
  # Pie Chart: Flights by Class
  pie_chart_plot <- ggplot(cleaned_data, aes(x = "", fill = Class)) +
    geom_bar(stat = "count", width = 1) +
    coord_polar(theta = "y") +
    labs(title = "Distribution of Flights by Class") +
    scale_fill_manual(values = custom_palette) +
    theme_void() +
    theme(legend.position = "top") +
    geom_text(aes(label = scales::percent(..count../sum(..count..))), stat = "count", position = position_stack(vjust = 0.5), color = "white")
  print(pie_chart_plot)
  Sys.sleep(2)
  
  # Box Plot of Price by Class
  engineered_box_plot <- ggplot(cleaned_data, aes(x = factor(Class), y = Price, fill = factor(Class))) +
    geom_boxplot(outlier.shape = 8, outlier.size = 3) +
    labs(title = "Price by Class", x = "Class", y = "Price ($)") +
    scale_fill_manual(values = custom_palette) +
    scale_y_continuous(labels = dollar_format(prefix = "$")) +
    theme_minimal()
  print(engineered_box_plot)
  Sys.sleep(2)
  
  #Price per Hour Analysis
  price_per_hour_plot <- ggplot(data, aes(x = price_per_hour)) +
    geom_histogram(bins = 30, fill = "seagreen", color = "black", alpha = 0.7) +
    labs(title = "Price Per Hour Distribution", x = "Price Per Hour", y = "Frequency") +
    theme_minimal()
  print(price_per_hour_plot)
  Sys.sleep(2)
  
  # Price vs. Duration
  price_vs_duration_plot <- ggplot(cleaned_data, aes(x = Duration, y = Price, color = Class)) +
    geom_point(alpha = 0.6) +
    geom_smooth(method = "lm", color = "red", linetype = "dashed") +
    labs(title = "Price vs Duration by Class", x = "Duration (Hours)", y = "Price") +
    theme_minimal()
  print(price_vs_duration_plot)
}

