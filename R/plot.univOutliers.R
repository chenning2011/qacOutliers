#' @title Plotting Univariate Outlier Detection
#' @description Creates visualizations for univariate outlier detection using method-specific plot styles.
#' @export
#' @param outlier_results A list containing the results from a univariate outlier detection function (e.g., `univOutliers`).
#' @returns A plot object specific to the outlier detection method used (boxplot, MAD, or Grubbs' test).
#' @examples
#' object <- univOutliers(data = mtcars, x = "carb", method = "mad")
#' #plot(object)
#'
#' object2 <- univOutliers(data = mtcars, x = "wt", method = "boxplot")
#' #plot(object2)
#'
#' object3 <- univOutliers(data = mtcars, x = "carb", method = "grubbs")
#' #plot(object3)


# Plot function for 'univOutliers'
plot.univOutliers <- function(outlier_results) {
  # Check if the input is of class "univOutliers"
  if (!inherits(outlier_results, "univOutliers")) {
    stop("Input must be of class 'univOutliers'.")
  }

  # Retrieve the stored dataset
  data <- outlier_results$dataset

  # Loop through each outlier result in the list
  for (column in names(outlier_results)) {
    # Skip the stored dataset key
    if (column == "dataset") next

    result <- outlier_results[[column]]
    method <- result$method
    outliers <- result$outliers
    outlier_rows <- result$outlier_rows

    # Plotting based on the outlier detection method
    if (method == "boxplot") {
      library(ggplot2)
      p <- ggplot(data, aes(y = .data[[column]])) +
        geom_boxplot(outlier.colour = "red", coef = 1.58) +
        ggtitle(paste("Univariate Boxplot of", column)) +
        theme_minimal()
      print(p)

    } else if (method == "mad") {
      library(Routliers)
      res1 <- outliers_mad(data[[column]])
      plot_outliers_mad(res1, data[[column]], pos_display = FALSE)
      title(main = paste("MAD Outlier Detection for", column), line = 4)

    } else if (method == "grubbs") {
      column_data <- na.omit(data[[column]])

      # Generate Q-Q plot for Grubbs' Test
      qq <- qqnorm(column_data, plot.it = FALSE)  # Get Q-Q plot data
      qqline(column_data, col = "blue", lwd = 2)  # Add Q-Q line

      # Create the Q-Q plot with fixed range for theoretical quantiles
      plot(qq$x, qq$y, main = paste("Q-Q Plot for", column, "(Grubbs' Test)"),
           xlab = "Theoretical Quantiles", ylab = "Sample Quantiles",
           col = "black", pch = 16,
           ylim = range(c(qq$y, outliers)))  # Adjust y-limits dynamically
      qqline(column_data, col = "blue", lwd = 2)  # Add Q-Q line again for clarity

      # Highlight the outliers in red
      if (length(outliers) > 0) {
        points(qq$x[outlier_rows], qq$y[outlier_rows], col = "red", pch = 19)
      }
    }
  }
}

