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
#' # Example 1: Boxplot Method
#' data(mtcars)
#' univOutliers(mtcars, "mpg", method = "boxplot")
#' univOutliers(mtcars, "hp", method = "boxplot")
#'
#' # Example 2: MAD Method
#' data <- c(10, 12, 10, 11, 13, 100, 10, 9, 11)  # Example data
#' univOutliers(data.frame(values = data), "values", method = "mad")
#'
#' # Example 3: Grubbs' Method
#' data2 <- data.frame(values = c(10, 12, 10, 11, 13, 100, 10, 9, 11))
#' univOutliers(data2, "values", method = "grubbs")
#'
#' # Example 4: Custom Test Dataset
#' set.seed(42)
#' normal_data <- rnorm(100, mean = 50, sd = 5)  # 100 normal points
#' with_outlier <- c(rnorm(99, mean = 50, sd = 5), 70)  # 99 normal + 1 outlier
#' multiple_outliers <- c(rnorm(95, mean = 50, sd = 5), 80, 85, 90)  # 95 normal + 3 outliers
#' max_length <- max(length(normal_data), length(with_outlier), length(multiple_outliers))
#' normal_data <- c(normal_data, rep(NA, max_length - length(normal_data)))
#' with_outlier <- c(with_outlier, rep(NA, max_length - length(with_outlier)))
#' multiple_outliers <- c(multiple_outliers, rep(NA, max_length - length(multiple_outliers)))
#' data1 <- data.frame(
#'   normal_data = normal_data,
#'   with_outlier = with_outlier,
#'   multiple_outliers = multiple_outliers
#' )
#'
#' # Boxplot Method on test data
#' univOutliers(data1, "normal_data", method = "boxplot")
#' univOutliers(data1, "with_outlier", method = "boxplot")
#' univOutliers(data1, "multiple_outliers", method = "boxplot")
#'
#' # MAD Method on test data
#' univOutliers(data1, "normal_data", method = "mad")
#' univOutliers(data1, "with_outlier", method = "mad")
#' univOutliers(data1, "multiple_outliers", method = "mad")
#'
#' # Grubbs' Method on test data
#' univOutliers(data1, "normal_data", method = "grubbs")
#' univOutliers(data1, "with_outlier", method = "grubbs")
#' univOutliers(data1, "multiple_outliers", method = "grubbs")


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

  # Loop through each numeric variable specified in 'x'
  for (column in x) {
    column_data <- na.omit(data[[column]])  # Remove NA values from the column

    # Boxplot Method
    if (method == "boxplot") {
      stats <- boxplot.stats(column_data)
      if (length(stats$out) == 0) {
        cat("No univariate outliers detected for", column, "\n")
      } else {
        cat("Outliers detected for", column, ":\n")
        outlier_rows <- which(data[[column]] %in% stats$out)
        for (i in outlier_rows) {
          cat("Row", i, ":", column_data[i], "\n")
        }
      }
      # Create the ggplot boxplot (optional, only for visualization)
      library(ggplot2)
      p <- ggplot(data, aes_string(y = column)) +
        geom_boxplot(outlier.colour = "red", coef = 1.58) +
        ggtitle(paste("Univariate Boxplot of", column)) +
        theme_minimal()
      print(p)
    }

    # MAD Method
    else if (method == "mad") {
      library(Routliers)
      if (!is.numeric(column_data)) stop("Input data must be numeric.")
      res1 <- outliers_mad(column_data)
      if (length(res1) == 0) {
        cat("No outliers detected for", column, ".\n")
      } else {
        cat("Outliers detected for", column, ":\n")
        print(res1)
      }
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

        calculate_skewness <- function(x) {
          n <- length(x)
          mean_x <- mean(x)
          sd_x <- sd(x)
          return(sum((x - mean_x)^3) / (n * sd_x^3))
        }

        calculate_kurtosis <- function(x) {
          n <- length(x)
          mean_x <- mean(x)
          sd_x <- sd(x)
          return(sum((x - mean_x)^4) / (n * sd_x^4) - 3)
        }

        skewness <- calculate_skewness(data)
        kurtosis <- calculate_kurtosis(data)

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

        # Q-Q plot
        qqnorm(data, main = "Normal QQ Plot")
        qqline(data)

        return(list(outliers = unique(outliers), skewness = skewness, kurtosis = kurtosis))
      }

      result <- grubbs_test(column_data)
      cat("Results for", column, ":\n")
      cat("Skewness:", result$skewness, "\n")
      cat("Kurtosis:", result$kurtosis, "\n")
      if (length(result$outliers) > 0) {
        cat("Outliers detected:", paste(result$outliers, collapse = ", "), "\n")
      } else {
        cat("No outliers detected.\n")
      }
    } else {
      stop("Invalid method. Choose from 'boxplot', 'mad', or 'grubbs'.")
    }
  }
}

