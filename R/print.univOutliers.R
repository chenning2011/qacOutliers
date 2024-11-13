#'@title Printing Univariate Outlier Detection
#'@description Prints identified univariate outliers using boxplot, Grubbs test, or MAD method
#'@param x the results from a univariate outlier detection function (e.g., `boxplot`, `grubbsTest`, or MAD)
#'@returns A formatted print of the results of the univariate outlier detection
#'@import ggplot2
#'@import dplyr
#'@import cli
#'@export
#'@examples
#'data(mtcars)
#'outliers_boxplot <- univOutliers(mtcars, "mpg", method="boxplot")
#'outliers_boxplot
#'outliers_mad <- univOutliers(mtcars$mpg, method="mad")
#'outliers_mad
#'outliers_grubbs <- univOutliers(mtcars, "mpg", method="grubbs")
#'outliers_grubbs
#'


print.univOutliers <- function(x, ...) {
  if(!inherits(x, "multiOutliers")){
    stop("This functon requires an object created by multiOutliers")
  }
  require(cli)
  cli_h1(cat("Method Chosen:", method))
  cli_h2("Outliers \n")
  print(x)
}

print.univOutliers <- function(x, ...) {
  # Check that x is from an appropriate outlier detection method
  if(!inherits(x, "boxplot") && !inherits(x, "grubbsTest") && !inherits(x, "mad")) {
    stop("This function requires an object created by a univariate outlier detection method (e.g., boxplot, grubbsTest, or MAD).")
  }

  require(cli)

  # Determine the method used and print accordingly
  if (inherits(x, "boxplot")) {
    method <- "Boxplot"
    outliers <- x$out
  } else if (inherits(x, "grubbsTest")) {
    method <- "Grubbs Test"
    outliers <- x$statistic
  } else if (inherits(x, "mad")) {
    method <- "MAD"
    outliers <- x$outliers
  }

  cli_h1(paste("Method Chosen:", method))
  cli_h2("Outliers \n")

  # Check if there are any outliers to display
  if(length(outliers) > 0) {
    print(outliers)
  } else {
    cli_alert_success("No outliers detected.")
  }
}
