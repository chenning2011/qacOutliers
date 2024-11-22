#' @title Univariate Outlier Detection
#' @description Detects univariate outliers using boxplot, MAD, or Grubbs' test.
#' @param data a data frame containing the variable(s) to be analyzed
#' @param x character, specifies the column(s) to assess for outliers. If NULL, all numeric columns are used
#' @param method character, the method used for outlier detection: "boxplot", "mad", or "grubbs" (default is "boxplot")
#' @returns A list with detected outliers, their indices, the method used, and the original dataset
#' @import ggplot2
#' @import Routliers
#' @import stats
#' @import grDevices
#' @export
#' @examples
#' Example usage:
#' object <- univOutliers(data = mtcars, x="carb", method = "mad")
#' plot.univOutliers(object)  # Plotting the object with 'mtcars' dataset
#' print(object)  # Printing the outliers and methods
#'
#' object2 <- univOutliers(data = mtcars, x= "wt", method = "boxplot")
#' plot.univOutliers(object2)  # Plotting the object with 'mtcars' dataset
#' print(object2)  # Printing the outliers and methods
#'
#' object3 <- univOutliers(data = mtcars, x = "carb", method = "grubbs")
#' plot.univOutliers(object3)  # Plotting the object with 'mtcars' dataset
#' print(object3)  # Printing the outliers and methods

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

  # Initialize a list to store outlier results for each column
  outliers_list <- list()

  # Loop through each numeric variable specified in 'x'
  for (column in x) {
    column_data <- na.omit(data[[column]])  # Remove NA values from the column

    # Detect outliers based on the specified method
    if (method == "boxplot") {
      stats <- boxplot.stats(column_data)
      outliers <- stats$out
      outlier_rows <- which(column_data %in% outliers)
      outliers_list[[column]] <- list(method = "boxplot", outliers = outliers, outlier_rows = outlier_rows)
    } else if (method == "mad") {
      library(Routliers)
      res1 <- outliers_mad(column_data)
      outlier_rows <- which(column_data < res1$LL_CI_MAD | column_data > res1$UL_CI_MAD)
      outliers_list[[column]] <- list(method = "mad", outliers = column_data[outlier_rows], outlier_rows = outlier_rows)
    } else if (method == "grubbs") {
      grubbs_test <- function(data, alpha = 0.05) {
        data <- na.omit(data)
        if (length(data) < 3) stop("Data must contain at least three points for Grubbs' test.")

        normality_test <- shapiro.test(data)
        if (normality_test$p.value < 0.05) {
          warning("Data is not normally distributed. Grubbs' test may not be appropriate.")
        }

        outliers <- c()
        current_data <- data
        repeat {
          mean_data <- mean(current_data)
          sd_data <- sd(current_data)
          G <- max(abs(current_data - mean_data)) / sd_data
          n <- length(current_data)
          critical_value <- (n - 1) / sqrt(n) * sqrt((qt(1 - alpha/(2*n), n - 2)^2) /
                                                       (n - 2 + qt(1 - alpha/(2*n), n - 2)^2))
          if (G > critical_value) {
            outlier <- current_data[which.max(abs(current_data - mean_data))]
            outliers <- c(outliers, outlier)
            current_data <- current_data[current_data != outlier]
          } else {
            break
          }

          if (length(current_data) < 3) break
        }

        outlier_rows <- which(data %in% outliers)
        return(list(outliers = unique(outliers), outlier_rows = outlier_rows))
      }

      result <- grubbs_test(column_data)
      outliers_list[[column]] <- list(method = "grubbs", outliers = result$outliers, outlier_rows = result$outlier_rows)
    } else {
      stop("Invalid method. Choose from 'boxplot', 'mad', or 'grubbs'.")
    }
  }

  # Store the original dataset in the object
  outliers_list$dataset <- data

  # Return the list of outlier results as an object
  class(outliers_list) <- "univOutliers"
  return(outliers_list)
}
