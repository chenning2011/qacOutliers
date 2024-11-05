# Load the Routliers package
library(Routliers)

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



