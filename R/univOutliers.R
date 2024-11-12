#'@title Univariate Outlier Detection
#'@description Provides three methods for detecting univariate outliers in a dataset.
#'@param data A data frame containing the variable to be analyzed.
#'@param x A character string naming the numeric variable to assess for outliers.
#'@param method A character supplying the method used for outlier detection. Methods are boxplot, mad, and grubbs.
#'@returns A list containing the ggplot object and boxplot statistics.
#'@import ggplot2
#'@import Routliers
#'@import stats
#'@export
#'@examples
#'data(mtcars)
#'univOutliers(mtcars, "mpg", method="boxplot")
#'univOutliers(mtcars, "hp", method="boxplot")
#'data <- c(10, 12, 10, 11, 13, 100, 10, 9, 11) # Example data
#'univOutliers(data, method="mad")
#'data2 <- Attacks$age
#'univOutliers(data2, method="mad")

univOutliers <- function(data, x = NULL, method = "boxplot") {

  # Identify numeric columns in the dataset
  numeric_columns <- sapply(data, is.numeric)

  # If 'x' is not specified, use all numeric columns in the dataset
  if (is.null(x)) {
    x <- names(data)[numeric_columns]
  } else {
    if (!x %in% names(data)) stop(paste("The specified column", x, "does not exist in the data frame."))
    x <- list(x)
  }

  # Loop through each numeric variable specified in 'x'
  for (column in x) {

    # Boxplot Method
    if (method == "boxplot") {
      # Calculate boxplot stats using base R
      stats <- boxplot.stats(data[[column]])

      # Check if outliers exist
      if (length(stats$out) == 0) {
        cat("No univariate outliers detected for", column, "\n")
      } else {
        cat("Outliers detected for", column, ":\n")
        # Print outliers with their corresponding row numbers
        outlier_rows <- which(data[[column]] %in% stats$out)
        for (i in outlier_rows) {
          cat("Row", i, ":", data[[column]][i], "\n")
        }
      }

      # Create the ggplot boxplot (optional, only for visualization)
      library(ggplot2)
      p <- ggplot(data, aes_string(y = column)) +
        geom_boxplot(outlier.colour = "red", coef = 1.58) +
        ggtitle(paste("Univariate Boxplot of", column)) +
        theme_minimal()

      print(p)
    }


  if(method=="mad"){
    library(Routliers)
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
  }

  }
}

