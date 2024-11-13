#' @title Univariate Outlier Detection
#' @description Provides three methods for detecting univariate outliers in a dataset.
#' @param data A data frame containing the variable to be analyzed.
#' @param x A character string naming the numeric variable to assess for outliers.
#' @param method A character supplying the method used for outlier detection. Methods are boxplot, mad, and grubbs.
#' @returns A list containing the ggplot object and boxplot statistics.
#' @import ggplot2
#' @import Routliers
#' @import stats
#' @export
#' @examples
#' # Generate example data
#' set.seed(42)
#' library(EnvStats)
#' data1 <- data.frame(
#'   var1 = rnorm(100, mean = 50, sd = 10),
#'   var2 = c(rnorm(97, mean = 50, sd = 10), 150, 160, 170),
#'   var3 = sample(1:100, 100, replace = TRUE, prob = (1:100)^2))
#'
#'
#' # Boxplot Method on test data
#' univOutliers(data1, "var1", method = "boxplot")
#' univOutliers(data1, "var2", method = "boxplot")
#' univOutliers(data1, method = "boxplot")
#'
#' # MAD Method on test data
#' univOutliers(data1, "var1", method = "mad")
#' univOutliers(data1, "var2", method = "mad")
#' univOutliers(data1, method = "mad")
#'
#' # Grubbs' Method on test data
#' univOutliers(data1, "var1", method = "grubbs")
#' univOutliers(data1, "var2", method = "grubbs")
#' univOutliers(data1, "var3", method = "grubbs")




univOutliers <- function(data, x = NULL, method = "boxplot") {
  # Identify numeric columns in the dataset
  numeric_columns <- sapply(data, is.numeric)

  # Suppressing warnings
  options(warn = -1)

  # If 'x' is not specified, use all numeric columns in the dataset
  if (is.null(x)) {
    x <- names(data)[numeric_columns]
  } else {
    if (!x %in% names(data)) stop(paste("The specified column", x, "does not exist in the data frame."))
    x <- list(x)
  }

  # Loop through each numeric variable specified in 'x'
  for (column in x) {
    column_data <- na.omit(data[[column]])  # Remove NA values from the column

    # Boxplot Method
    if (method == "boxplot") {
      stats <- boxplot.stats(column_data)
      if (length(stats$out) == 0) {
        cat("No outliers detected for", column, "\n")
      } else {
        cat("Outliers detected for", column, ":\n")
        outlier_rows <- which(data[[column]] %in% stats$out)
        for (i in outlier_rows) {
          cat("Row", i, ":", data[[column]][i], "\n")
        }
      }
      # Create the ggplot boxplot (optional, only for visualization)
      library(ggplot2)
      p <- ggplot(data, aes(y = .data[[column]])) +
        geom_boxplot(outlier.colour = "red", coef = 1.58) +
        ggtitle(paste("Univariate Boxplot of", column)) +
        theme_minimal()
      print(p)
    }

    # MAD Method
    else if (method == "mad") {
      library(Routliers)
      # Use the outliers_mad function to find outliers
      res1 <- outliers_mad(data[[column]])

      # Display the outliers information
      if (length(res1) == 0) {
        cat("No outliers detected for", column, "\n")
      } else {
        cat("Outliers detected for", column, ":\n")

        # Identify the row numbers and corresponding column values for outliers
        outlier_rows <- which(data[[column]] < res1$LL_CI_MAD | data[[column]] > res1$UL_CI_MAD)
        for (i in outlier_rows) {
          cat("Row", i, ":", data[[column]][i], "\n")
        }
      }

      # Plot the outliers using plot_outliers_mad
      plot_outliers_mad(res1, data[[column]], pos_display = FALSE)
    }

    # Grubbs' Test Method
    else if (method == "grubbs") {
      grubbs_test <- function(data, alpha = 0.05) {
        data <- na.omit(data)  # Remove NA values
        if (!is.numeric(data)) stop("Input data must be numeric.")
        if (length(data) < 3) stop("Data must contain at least three points for Grubbs' test.")

        # Normality check
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

        # Identify the rows of outliers
        outlier_rows <- which(data %in% outliers)

        return(list(outliers = unique(outliers), outlier_rows = outlier_rows))
      }

      result <- grubbs_test(column_data)
      cat("Outliers detected for", column, ":\n")
      if (length(result$outliers) == 0) {
        cat("No outliers detected for", column, "\n")
      } else {
        for (i in result$outlier_rows) {
          cat("Row", i, ":", data[[column]][i], "\n")
        }
      }
    } else {
      stop("Invalid method. Choose from 'boxplot', 'mad', or 'grubbs'.")
    }
  }
}
