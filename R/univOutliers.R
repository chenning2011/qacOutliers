#'@title Univariate Outlier Detection with Boxplot
#'@description Generate a boxplot specifically for univariate outlier detection and return associated statistics from base R.
#'@param data A data frame containing the variable to be analyzed.
#'@param x A character string naming the numeric variable to assess for outliers.
#'@returns A list containing the ggplot object and boxplot statistics.
#'@import ggplot2
#'@import Routliers
#'@import stats
#'@export
#'@examples
#'univOutliers(mtcars, "mpg", method="boxplot")
#'univOutliers(mtcars, "hp", method="boxplot")
#'data <- c(10, 12, 10, 11, 13, 100, 10, 9, 11) # Example data
#'univOutliers(data, method="mad")
#'data2 <- Attacks$age
#'univOutliers(data2, method="mad")

univOutliers <- function(data, x, method="boxplot") {
  if (method=="boxplot"){
    library(ggplot2)
    if (!x %in% names(data)) stop(paste("The specified column", x, "does not exist in the data frame."))

    # Calculate boxplot stats using base R
    stats <- boxplot.stats(data[[x]])
    cat("Univarate Boxplot Statistics for", x, ":\n")
    print(stats)  # Print boxplot statistics

    # Create the ggplot boxplot
    p <- ggplot(data, aes(y = .data[[x]])) +
      geom_boxplot(outlier.colour = "red") +
      ggtitle(paste("Univariate Boxplot of", x)) +
      theme_minimal()

    return(list(plot = p, stats = stats))
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
