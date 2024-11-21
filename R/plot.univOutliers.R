#' @title Plotting Univariate Outlier Detection
#' @description Creates visualizations for univariate outlier detection using the method-specific plot style.
#' @param data The original data frame used for outlier detection.
#' @param x A character string naming the numeric variable to visualize.
#' @param method The method used for outlier detection ("boxplot", "mad", or "grubbs").
#' @param results A list containing the results of the outlier detection.
#' @returns A plot object specific to the method.
#' @export
#' @examples
#' # # Example usage:
# object <- univOutliers(data = mtcars, method = "mad")
# plot.univOutliers(object, mtcars)  # Plotting the object with 'mtcars' dataset
# print(object)  # Printing the outliers and methods
#
# object <- univOutliers(data = mtcars, method = "boxplot")
# plot.univOutliers(object, mtcars)  # Plotting the object with 'mtcars' dataset
# print(object)  # Printing the outliers and methods
#
# object <- univOutliers(data = mtcars, method = "grubbs")
# plot.univOutliers(object, mtcars)  # Plotting the object with 'mtcars' dataset
# print(object)  # Printing the outliers and methods

# Plot function for 'univOutliers'
plot.univOutliers <- function(outlier_results, data) {
  # Check if the input is of class "univOutliers"
  if (!inherits(outlier_results, "univOutliers")) {
    stop("Input must be of class 'univOutliers'.")
  }

  # Loop through each outlier result in the list
  for (column in names(outlier_results)) {
    result <- outlier_results[[column]]
    method <- result$method
    outliers <- result$outliers
    outlier_rows <- result$outlier_rows
    column_data <- na.omit(data[[column]])  # Ensure 'data' is passed to the function

    # Plotting based on the outlier detection method
    if (method == "boxplot") {
      # Plot the boxplot with red dots for outliers
      library(ggplot2)
      p <- ggplot(data, aes(y = .data[[column]])) +
        geom_boxplot(outlier.colour = "red", coef = 1.58) +  # Add outlier detection based on boxplot stats
        ggtitle(paste("Univariate Boxplot of", column)) +
        theme_minimal()
      print(p)

    } else if (method == "mad") {
      # MAD Method - Plot outliers using the 'plot_outliers_mad' function
      library(Routliers)
      # Access precomputed outlier information
      res1 <- outliers_mad(data[[column]])  # We assume the result already contains this data

      # Plot outliers using plot_outliers_mad
      plot_outliers_mad(res1, data[[column]], pos_display = FALSE)
      title(main = paste("MAD Outlier Detection for", column), line = 4)  # Adjust 'line' to control title spacing

    } else if (method == "grubbs") {
      # Grubbs' Test Method - Plot the data and outliers
      plot(column_data, main = paste("Grubbs' Test for", column), col = "black", pch = 16)
      points(outlier_rows, outliers, col = "red", pch = 19)  # Red dots for outliers
      legend("topright", legend = "Outliers", col = "red", pch = 19)
    }
  }
}
