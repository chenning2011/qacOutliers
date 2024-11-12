#' Grubbs' Test for Outlier Detection
#'
#' @param data A numeric vector of data points.
#' @param alpha Significance level for the test (default is 0.05).
#' @return A list containing detected outliers, skewness, kurtosis, and QQ plot.
#' @importFrom outliers grubbs.test
#' @export
grubbs_test <- function(data, alpha = 0.05) {
  # Check if data is numeric
  if (!is.numeric(data)) {
    stop("Input data must be a numeric vector.")
  }

  # Check if there are enough data points
  if (length(data) < 3) {
    stop("Data must contain at least three points for Grubbs' test.")
  }

  # Check for normality using the Shapiro-Wilk test
  normality_test <- shapiro.test(data)
  if (normality_test$p.value < 0.05) {
    warning("Data is not normally distributed. Grubbs' test may not be appropriate.")
  }

  # Function to calculate skewness
  calculate_skewness <- function(x) {
    n <- length(x)
    mean_x <- mean(x)
    sd_x <- sd(x)
    return(sum((x - mean_x)^3) / (n * sd_x^3))
  }

  # Function to calculate kurtosis
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
    # Calculate Grubbs' test statistic
    mean_data <- mean(current_data)
    sd_data <- sd(current_data)
    G <- max(abs(current_data - mean_data)) / sd_data
    n <- length(current_data)

    # Critical value from the t-distribution
    critical_value <- (n - 1) / sqrt(n) * sqrt((qt(1 - alpha/(2*n), n - 2)^2) / (n - 2 + qt(1 - alpha/(2*n), n - 2)^2))

    # Compare G to the critical value
    if (G > critical_value) {
      outlier <- current_data[which.max(abs(current_data - mean_data))]
      outliers <- c(outliers, outlier)
      current_data <- current_data[current_data != outlier]  # Remove detected outlier
    } else {
      break
    }

    # Exit if there are not enough data points left
    if (length(current_data) < 3) {
      break
    }
  }

  # Create Q-Q plot
  qqnorm(data, main = "Normal QQ Plot")
  qqline(data)

  if (length(outliers) == 0) {
    message("No outliers detected.")
  }

  return(list(outliers = unique(outliers), skewness = skewness, kurtosis = kurtosis))
}

set.seed(42)  # For reproducibility
data1 <- rnorm(100, mean = 50, sd = 5)  # 100 data points from a normal distribution
grubbs_test(data1)

set.seed(42)  # For reproducibility
data2 <- c(rnorm(99, mean = 50, sd = 5), 70)  # 99 normal points + 1 outlier
grubbs_test(data2)

set.seed(42)  # For reproducibility
data3 <- c(rnorm(95, mean = 50, sd = 5), 80, 85, 90)  # 95 normal points + 3 outliers
grubbs_test(data3)
