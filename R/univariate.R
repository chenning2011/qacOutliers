#' Univariate Outlier Detection
#'
#' @param x A numeric vector of values.
#' @param method A character string indicating the method to be used. Currently, "boxplot" and "grubbs" are supported.
#' @param alpha A numeric value for significance level (default is 0.05).
#' @return A list containing outlier results and additional information.
#' @export
unioutlier <- function(x, method = c("boxplot", "grubbs"), alpha = 0.05) {
  method <- match.arg(method)

  if (!is.numeric(x) || length(x) < 3) {
    stop("Input must be a numeric vector with at least 3 values.")
  }

  if (method == "boxplot") {
    outliers <- boxplot.stats(x)$out
    return(list(outliers = outliers, method = "boxplot"))
  } else if (method == "grubbs") {
    library(outliers)
    grubbs_result <- grubbs.test(x)

    # Correctly extract outliers based on the Grubbs test
    outliers <- x[grubbs_result$outliers]

    return(list(outliers = outliers, method = "grubbs", p.value = grubbs_result$p.value))
  }
}

# Test the function directly
x_test <- c(1, 2, 3, 4, 100)
result <- unioutlier(x_test, method = "boxplot")
print(result)


