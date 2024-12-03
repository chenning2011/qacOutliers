#' @title Plotting Univariate Outlier Detection
#' @description Creates visualizations for univariate outlier detection using method-specific plot styles.
#' @export
#' @param x A list containing the results from a univariate outlier detection function (e.g., `univOutliers`).
#' @param ... any additional arguments.
#' @returns A plot object specific to the outlier detection method used (boxplot, MAD, or Grubbs' test).
#' @import ggplot2
#' @import Routliers
#' @import stats
#' @import grDevices
#' @examples
#' object <- univOutliers(data = mtcarsOutliers, x = "carb", method = "mad")
#' plot(object)
#'
#' object2 <- univOutliers(data = mtcarsOutliers, x = "wt", method = "boxplot")
#' plot(object2)
#'
#' object3 <- univOutliers(data = mtcarsOutliers, x = "carb", method = "grubbs")
#' plot(object3)

plot.univOutliers <- function(x, ...) {
  if (!inherits(x, "univOutliers")) {
    stop("Input must be of class 'univOutliers'.")
  }

  data <- x$dataset

  for (column in names(x)) {
    if (column == "dataset") next

    result <- x[[column]]
    method <- result$method
    outliers <- result$outliers
    outlier_rows <- result$outlier_rows

    if (method == "boxplot") {
      p <- ggplot(data, aes(y = !!sym(column))) +
        geom_boxplot(outlier.colour = "red", coef = 1.58) +
        ggtitle(paste("Univariate Boxplot of", column)) +
        theme_minimal()
      print(p)

    } else if (method == "mad") {
        res1 <- Routliers::outliers_mad(data[[column]])
        par(mfrow = c(1, 1))  # Ensure a single plot layout
        Routliers::plot_outliers_mad(res1, data[[column]], pos_display = FALSE)
        title(main = paste("MAD Outlier Detection for", column), line = 4)
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
