#'@title Printing Multivariate outlier detection
#'@description Prints identified multivariate outliers
#'@export
#'@param x the results from "multiOutliers" function
#'@returns A formatted print of the results of the "multiOutliers" function
#'@import ggplot2
#'@import Routliers
#'@import dplyr
#'@import cli
#'@examples
#'multiOutliers(mtcars, method="mahalanobis")
#'


print.multiOutliers <- function(x, ...) {
  if(!inherits(x, "multiOutliers")){
    stop("This functon requires an object created by multiOutliers")
  }

  require(cli)

  cli_h1(cat("Method Chosen:", method))
  cli_h2("Outliers \n")

  cat("Method:", x$Method)
  cat("\nDataset:", x$Data)
  cat("\nVariables:", x$Variables)
  cat("\nRow:", x$Row)
  cat("\nOutlier Score:", x$Score)

  if (length(x) >= 6) {
    cat("\nOption 1:", names(x)[6], "=", x[[6]])
  }
  if (length(x) >= 7) {
    cat("\nOption 2:", names(x)[7], "=", x[[7]])
  }

  return(x)
}
