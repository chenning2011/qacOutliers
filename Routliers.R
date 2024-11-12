# Load the Routliers package
library(Routliers)

#How do I put the coordinates of the outliers above the points?

find_and_plot_outliers_mad <- function(data, column) {
  # Check if the data is a data frame and column exists in the data
  if (!is.data.frame(data)) {
    stop("Input must be a data frame.")
  }
  if (!column %in% colnames(data)) {
    stop("Specified column not found in the data frame.")
  }

  # Check if the specified column is numeric
  if (!is.numeric(data[[column]])) {
    stop("The specified column must be numeric.")
  }

  # Use the outliers_mad function to find outliers
  res1 <- outliers_mad(data[[column]])

  # Display the outliers
  if (length(res1) == 0) {
    cat("No outliers detected in the data.\n")
  } else {
    cat("Outliers detected:\n")
    print(res1)
  }

  # Plot the outliers using plot_outliers_mad
  plot_outliers_mad(res1, data[[column]], pos_display = FALSE)
}


#Example usage
data(mtcars)
find_and_plot_outliers_mad(mtcars, "mpg")


# Example usage
find_and_plot_outliers_mad(Attacks, "age")



library(ggplot2)
library(dplyr)

find_and_plot_outliers_mad3 <- function(data, column) {
  # Check if the data is a data frame and column exists in the data
  if (!is.data.frame(data)) {
    stop("Input must be a data frame.")
  }
  if (!column %in% colnames(data)) {
    stop("Specified column not found in the data frame.")
  }

  # Check if the specified column is numeric
  if (!is.numeric(data[[column]])) {
    stop("The specified column must be numeric.")
  }

  # Find outliers using the MAD (Median Absolute Deviation) method
  res1 <- outliers_mad(data[[column]])

  # Display the outliers
  if (length(res1) == 0) {
    cat("No outliers detected in the data.\n")
  } else {
    cat("Outliers detected:\n")
    print(res1)
  }

  # Plot the data with outliers highlighted
  data <- data %>%
    mutate(is_outlier = data[[column]] %in% res1)

  ggplot(data, aes_string(x = column)) +
    geom_histogram(binwidth = 1, fill = "lightblue", color = "black", alpha = 0.7) +
    geom_point(data = subset(data, is_outlier), aes_string(y = 0), size = 3) +
    labs(title = paste("Outliers in", column), x = column, y = "Frequency") +
    theme_minimal()
}

find_and_plot_outliers_mad3(Attacks, "age")

library(ggplot2)
library(dplyr)

library(ggplot2)
library(dplyr)
library(ggrepel)
library(plotly)

find_and_plot_outliers_mad4 <- function(data, column) {
  # Check if the data is a data frame and column exists in the data
  if (!is.data.frame(data)) {
    stop("Input must be a data frame.")
  }
  if (!column %in% colnames(data)) {
    stop("Specified column not found in the data frame.")
  }

  # Check if the specified column is numeric
  if (!is.numeric(data[[column]])) {
    stop("The specified column must be numeric.")
  }

  # Use the outliers_mad function to find outliers
  res1 <- outliers_mad(data[[column]])

  # Display the outliers
  if (length(res1) == 0) {
    cat("No outliers detected in the data.\n")
  } else {
    cat("Outliers detected:\n")
    print(res1)
  }

  # Add an ID column to identify each point in the dataset
  data <- data %>%
    mutate(ID = 1:n(), is_outlier = data[[column]] %in% res1)

  # Filter the data to only include the outliers for labeling
  outliers <- data %>% filter(is_outlier)

  # Plot with ggplot, highlighting outliers and using ggrepel for labels
  p <- ggplot(data, aes(x = ID, y = .data[[column]])) +
    geom_point(alpha = 0.5) +
    geom_text_repel(data = outliers, aes(label = ID), size = 3, color = "red") +
    labs(title = paste("Outliers in", column), x = "ID", y = column) +
    theme_minimal()

  # Convert to an interactive plot with plotly
  ggplotly(p, tooltip = c("x", "y", "label"))
}

# Example usage
find_and_plot_outliers_mad4(Attacks, "age")


