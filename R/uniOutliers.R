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
#'
#' #Example usage:
#' object <- uniOutliers(data = mtcarsOutliers, x="wt", method = "mad")
#' print(object)  # Printing the outliers and methods
#'
#' object2 <- uniOutliers(data = mtcarsOutliers, x= "wt", method = "boxplot")
#' print(object2)  # Printing the outliers and methods
#'
#' object3 <- uniOutliers(data = grubbsOutlier, method = "grubbs")
#' print(object3)  # Printing the outliers and methods

uniOutliers <- function(data, x = NULL, method = "boxplot") {
  # Identify numeric columns in the dataset
  numeric_columns <- sapply(data, is.numeric)

  # Standardize method argument
  method <- match.arg(method, c("boxplot", "mad", "grubbs"))

  # If 'x' is not specified, use all numeric columns in the dataset
  if (is.null(x)) {
    x <- names(data)[numeric_columns]
  } else {
    if (!x %in% names(data)) stop(paste("The specified column", x, "does not exist in the data frame."))
    x <- list(x)
  }

  #error messaging if any of the variables are non-numeric
  for (var in x){
    if(!is.numeric(data[[var]])){
      stop((paste(var, "is non-numeric. Please provide only numeric variables.")))
    }
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
      res1 <- Routliers::outliers_mad(column_data)
      outlier_rows <- which(column_data < res1$LL_CI_MAD | column_data > res1$UL_CI_MAD)
      outliers_list[[column]] <- list(method = "mad", outliers = column_data[outlier_rows], outlier_rows = outlier_rows)
    } else if (method == "grubbs") {
      grubbs_test <- function(data, alpha = 0.05) {
        data <- na.omit(data)
        if (length(data) < 3) stop("Data must contain at least three points for Grubbs' test.")

        normality_test <- shapiro.test(data)
        if (normality_test$p.value < 0.05) {
          warning(paste0("Data is not normally distributed for ", column,". Grubbs' test may not be appropriate."), call.=F)
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
    }
  }

  # Store the original dataset in the object
  outliers_list$dataset <- data

  # Return the list of outlier results as an object
  class(outliers_list) <- "uniOutliers"
  return(outliers_list)
}
