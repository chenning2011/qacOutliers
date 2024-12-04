#' @title Plotting Univariate Outlier Detection
#' @description Creates visualizations for univariate outlier detection using method-specific plot styles.
#' @export
#' @param x A list containing the results from a univariate outlier detection function (e.g., `univOutliers`).
#' @param ... not used
#' @returns A plot object specific to the outlier detection method used (boxplot, MAD, or Grubbs' test).
#' @import ggplot2
#' @import Routliers
#' @import stats
#' @import grDevices
#' @examples
#' object <- uniOutliers(data = mtcarsOutliers, x = "wt", method = "mad")
#' plot(object)
#'
#' object2 <- uniOutliers(data = mtcarsOutliers, x = "wt", method = "boxplot")
#' plot(object2)
#'
#' object3 <- uniOutliers(data = mtcarsOutliers, x = "wt", method = "grubbs")
#' plot(object3)

plot.uniOutliers <- function(x, ...) {
  if (!inherits(x, "uniOutliers")) {
    stop("Input must be of class 'uniOutliers'.")
  }

  #turning x$dataset into data
  data <- x$dataset

  for (column in names(x)) {
    #skipping dataset object
    if (column == "dataset") next

    #extracting information from x for every variable used in computing outliers
    result <- x[[column]]
    method <- result$method
    outliers <- result$outliers
    outlier_rows <- result$outlier_rows

    #boxplot method graph
    if (method == "boxplot") {
      p <- ggplot(data, aes(y = !!sym(column))) +
        geom_boxplot(outlier.colour = "red", coef = 1.58) +
        ggtitle(paste("Univariate Boxplot of", column)) +
        theme_minimal()
      print(p)

      #mad graph
    } else if (method == "mad") {
        res1 <- Routliers::outliers_mad(data[[column]])
        par(mfrow = c(1, 1))  # Ensure a single plot layout
        Routliers::plot_outliers_mad(res1, data[[column]], pos_display = FALSE)
        title(main = paste("MAD Outlier Detection for", column), line = 4)

      #grubbs graph
    } else if (method == "grubbs") {
      column_data <- na.omit(data[[column]])

      # Explicitly create the plot first
      qq <- qqnorm(column_data, main = paste("Q-Q Plot for", column, "(Grubbs' Test)"),
                   xlab = "Theoretical Quantiles", ylab = "Sample Quantiles",
                   col = "black", pch = 16, plot.it = TRUE)
      qqline(column_data, col = "blue", lwd = 2)

      # Highlight the outliers in red
      if (length(outliers) > 0 && !is.null(outlier_rows)) {
        points(qq$x[outlier_rows], qq$y[outlier_rows], col = "red", pch = 19)
      }
    }
  }
}
