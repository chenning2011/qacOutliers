#' @title Printing Univariate Outlier Detection Results
#' @description Prints identified univariate outliers using the specified method (boxplot, MAD, or Grubbs' test).
#' @export
#' @param x The results from a univariate outlier detection function (e.g., output from `univOutliers`).
#' @param ... not used
#' @returns A formatted print of the outlier results, including the detected outliers, their row numbers, and the method used.
#' @import ggplot2
#' @import dplyr
#' @import cli
#' @import knitr
#' @examples
#' object <- univOutliers(data = mtcars, x = "carb", method = "mad")
#' print(object)
#'
#' object2 <- univOutliers(data = mtcars, x = "wt", method = "boxplot")
#' print(object2)
#'
#' object3 <- univOutliers(data = mtcars, x = "carb", method = "grubbs")
#' print(object3)

print.univOutliers <- function(x, ...) {
  # Loop through each column's results
  for (column in names(x)) {
    # Skip if the entry is not a numeric column from the dataset
    if (!"method" %in% names(x[[column]]) || is.null(x[[column]]$method)) next

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
