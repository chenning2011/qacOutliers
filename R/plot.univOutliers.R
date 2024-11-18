#' @title Plotting Univariate Outlier Detection
#' @description Creates visualizations for univariate outlier detection using the method-specific plot style.
#' @param data The original data frame used for outlier detection.
#' @param x A character string naming the numeric variable to visualize.
#' @param method The method used for outlier detection ("boxplot", "mad", or "grubbs").
#' @param results A list containing the results of the outlier detection.
#' @returns A plot object specific to the method.
#' @export

plotOutliers <- function(data, x, method, results) {
  if (!x %in% names(data)) {
    stop("The specified column does not exist in the data frame.")
  }

  column_data <- data[[x]]

  if (method == "boxplot") {
    # Boxplot visualization
    library(ggplot2)
    p <- ggplot(data, aes(y = .data[[x]])) +
      geom_boxplot(outlier.colour = "red", coef = 1.5) +
      ggtitle(paste("Univariate Boxplot of", x)) +
      theme_minimal()
    print(p)

  } # MAD Method
  else if (method == "mad") {
    library(Routliers)
    # Use the outliers_mad function to find outliers
    res1 <- outliers_mad(data[[x]])

    # Identify the rows of outliers
    outlier_rows <- which(data[[x]] < res1$LL_CI_MAD | data[[x]] > res1$UL_CI_MAD)

    if (length(outlier_rows) == 0) {
      # If no outliers are found
      cat("No outliers detected for", x, "\n")
    } else {
      # If outliers are detected
      cat("Outliers detected for", x, ":\n")
      for (i in outlier_rows) {
        cat("Row", i, ":", data[[x]][i], "\n")
      }
    }

    # Plot the outliers using plot_outliers_mad
    plot_outliers_mad(res1, data[[x]], pos_display = FALSE)

  } else if (method == "grubbs") {
    # Grubbs visualization with Q-Q plot
    outliers <- results$outliers  # Assuming results contain outliers detected by Grubbs

    # Base R Q-Q Plot
    qqnorm(column_data, main = paste("Normal Q-Q Plot for", x))
    qqline(column_data)

    # Highlight outliers in red if present
    if (length(outliers) > 0) {
      points_outliers <- column_data %in% outliers
      points(qqnorm(column_data[points_outliers], plot.it = FALSE), col = "red", pch = 19)
    }
  } else {
    stop("Invalid method. Choose from 'boxplot', 'mad', or 'grubbs'.")
  }
}


# Example Test Cases

# Load necessary libraries
library(ggplot2)
library(Routliers)

# Create synthetic data
set.seed(123)
data <- data.frame(
  value = c(rnorm(95, mean = 50, sd = 10), 150, 160, 170, 180, 190) # Outliers added
)

# Example 1: Boxplot method
# Assuming no additional `results` are needed for boxplot
plotOutliers(data = data, x = "value", method = "boxplot", results = NULL)

# Example 2: MAD method
library(Routliers)
plotOutliers(data = data, x = "value", method = "mad", results=NULL)

# Example 3: Grubbs method
# Assuming you have a Grubbs' test results list
library(outliers)
grubbs_result <- grubbs.test(data$value)
grubbs_outliers <- data$value[data$value %in% grubbs_result$data]
results <- list(outliers = grubbs_outliers)

plotOutliers(data = data, x = "value", method = "grubbs", results = results)

# Load necessary libraries
library(outliers)

# Example Dataset 1: Small numeric dataset with obvious outliers
data1 <- data.frame(
  value = c(10, 12, 15, 14, 13, 120) # 120 is an obvious outlier
)
grubbs_result1 <- grubbs.test(data1$value)
grubbs_outliers1 <- data1$value[data1$value %in% grubbs_result1$data]
results1 <- list(outliers = grubbs_outliers1)

plotOutliers(data = data1, x = "value", method = "grubbs", results = results1)

#MAD test on small numeric dataset with obvious outliers, did different data frame cause
#labels were smooshed when I did mad test for data1
data5 <- data.frame(
  value = c(10,15,30,45,70,300) # 300 is an obvious outlier
)
plotOutliers(data = data5, x = "value", method = "mad", results = NULL)


# Example Dataset 2: Larger dataset with extreme outliers
set.seed(42)
data2 <- data.frame(
  value = c(rnorm(98, mean = 100, sd = 10), 200, 300) # 200 and 300 are extreme outliers
)
grubbs_result2 <- grubbs.test(data2$value)
grubbs_outliers2 <- data2$value[data2$value %in% grubbs_result2$data]
results2 <- list(outliers = grubbs_outliers2)

plotOutliers(data = data2, x = "value", method = "grubbs", results = results2)

# using mad method on dataset with extreme outliers
plotOutliers(data = data2, x = "value", method = "mad", results = NULL)

# Example Dataset 3: Dataset with no outliers
data3 <- data.frame(
  value = rnorm(100, mean = 50, sd = 5) # No outliers
)
grubbs_result3 <- grubbs.test(data3$value)
grubbs_outliers3 <- data3$value[data3$value %in% grubbs_result3$data]
results3 <- list(outliers = grubbs_outliers3)

plotOutliers(data = data3, x = "value", method = "grubbs", results = results3)

#using mad method on dataset with "no outliers"
plotOutliers(data = data3, x = "value", method = "mad")

# Example Dataset 4: Highly skewed dataset
data4 <- data.frame(
  value = c(rexp(95, rate = 0.1), 150, 200) # Exponential distribution with outliers
)
grubbs_result4 <- grubbs.test(data4$value)
grubbs_outliers4 <- data4$value[data4$value %in% grubbs_result4$data]
results4 <- list(outliers = grubbs_outliers4)

plotOutliers(data = data4, x = "value", method = "grubbs", results = results4)

#using mad method on highly skewed dataset
plotOutliers(data = data4, x="value", method = "mad", results = NULL)
