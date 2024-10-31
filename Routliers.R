# Load the Routliers package
library(Routliers)

# Define a function to find and plot outliers using outliers_mad
find_and_plot_outliers_mad <- function(data) {
  # Check if the input data is numeric
  if (!is.numeric(data)) {
    stop("Input data must be numeric.")
  }

  # Use the outliers_mad function to find outliers
  res1 <- outliers_mad(data)

  # Display the outliers
  if (length(res1) == 0) {
    cat("No outliers detected in the data.\n")
  } else {
    cat("Outliers detected:\n")
    print(res1)
  }

  # Plot the outliers using plot_outliers_mad
  plot_outliers_mad(res1, data, pos_display = FALSE)
}

# Example usage
data <- c(10, 12, 10, 11, 13, 100, 10, 9, 11) # Example data
find_and_plot_outliers_mad(data)

# Example usage
data2 <- Attacks$age
find_and_plot_outliers_mad(data2)

