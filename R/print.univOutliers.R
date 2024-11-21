#'@title Printing Univariate Outlier Detection
#'@description Prints identified univariate outliers using boxplot, Grubbs test, or MAD method
#'@param x the results from a univariate outlier detection function (e.g., `boxplot`, `grubbsTest`, or MAD)
#'@returns A formatted print of the results of the univariate outlier detection
#'@import ggplot2
#'@import dplyr
#'@import cli
#'@export
#'@examples
#' # Example usage:
#' object <- univOutliers(data = mtcars, method = "mad")
#' plot.univOutliers(object, mtcars)  # Plotting the object with 'mtcars' dataset
#' print(object)  # Printing the outliers and methods
#'
#' object <- univOutliers(data = mtcars, method = "boxplot")
#' plot.univOutliers(object, mtcars)  # Plotting the object with 'mtcars' dataset
#' print(object)  # Printing the outliers and methods
#'
#' object <- univOutliers(data = mtcars, method = "grubbs")
#' plot.univOutliers(object, mtcars)  # Plotting the object with 'mtcars' dataset
#' print(object)  # Printing the outliers and methods


# Print function for univOutliers
print.univOutliers <- function(x, ...) {
  # Load required libraries
  require(cli)
  require(knitr)

  # Loop through each column's results
  for (column in names(x)) {
    result <- x[[column]]
    method <- result$method
    outliers <- result$outliers
    outlier_rows <- result$outlier_rows

    # Print the method used
    cli_h1(paste("Method Chosen:", method))
    cli_text(paste("Variable:", column))

    # Check if any outliers are found
    if (length(outliers) > 0) {
      # Create a data frame for the outliers
      outlier_table <- data.frame(
        "Row Number" = outlier_rows,
        "Outlier Value" = outliers
      )

      # Print the table of outliers
      cat("\nOutliers Detected for:\n")
      print(knitr::kable(outlier_table, format = "simple", align = c("c", "c"), col.names = c("Row Number", "Outlier Value")))

    } else {
      cli_alert_success("No outliers detected.")
    }
  }

  return(invisible(x))
}
