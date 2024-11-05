# Load the Routliers package
library(Routliers)
#argument inputs are dataframe and column name in the data frame where you want to
#conduct univariate analysis on
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

# test of functions in Routliers package
data(Attacks)
res2 <- outliers_mad(Attacks$soc12)
plot_outliers_mad(res2, Attacks$soc12, pos_display = FALSE)


find_and_plot_outliers_mad2 <- function(data, column) {
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
  outlier_values <- outliers_mad(data[[column]])

  # Check if there are any outliers
  if (length(outlier_values) == 0) {
    cat("No outliers detected in the data.\n")
  } else {
    # Find row indices for each outlier and print the row and column
    for (outlier in outlier_values) {
      # Find row indices where the column value matches the outlier
      outlier_rows <- which(data[[column]] == outlier)
      for (row_index in outlier_rows) {
        cat("Outlier detected: Value:", outlier,
            "at Row:", row_index,
            "Column:", column, "\n")
      }
    }
  }

  # Plot the outliers using plot_outliers_mad
  plot_outliers_mad(outlier_values, data[[column]], pos_display = FALSE)
}

#Example usage
data(mtcars)
find_and_plot_outliers_mad2(mtcars, "mpg")


# Example usage
find_and_plot_outliers_mad2(Attacks, "age")


