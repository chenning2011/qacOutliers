#' @title Printing Univariate Outlier Detection
#' @description Prints identified univariate outliers using boxplot, Grubbs test, or MAD method
#' @param x The results from a univariate outlier detection function (e.g., `univOutliers`)
#' @returns A formatted print of the results of the univariate outlier detection
#' @import ggplot2
#' @import dplyr
#' @export
#' @examples
#' set.seed(42)
#' data1 <- data.frame(
#'   var1 = rnorm(100, mean = 50, sd = 10),
#'   var2 = c(rnorm(97, mean = 50, sd = 10), 150, 160, 170),
#'   var3 = sample(1:100, 100, replace = TRUE, prob = (1:100)^2))
#'
#' results <- univOutliers(data1, method = "boxplot")
#' results <- univOutliers(data1, "var1", method = "boxplot")
#' results <- univOutliers(data1, method = "grubbs")
#' results <- univOutliers(data1, "var1", method = "grubbs")
#' results <- univOutliers(data1, method = "mad")
#' results <- univOutliers(data1, "var1", method = "mad")


print.univOutliers <- function(results, method) {
  # Print the method being used
  cat("Outlier detection using", method, "method\n\n")

  # Loop through the results for each variable
  for (column in names(results)) {
    result <- results[[column]]

    # Check if outliers were detected
    if (length(result$outliers) > 0) {
      cat("Outliers detected for", column, ":\n")
      for (i in seq_along(result$outliers)) {
        cat("Row", result$outlier_rows[i], ":", result$outliers[i], "\n")
      }
    } else {
      cat("No outliers detected for", column, "\n")
    }
    cat("\n")  # Add a newline between columns for readability
  }
}
