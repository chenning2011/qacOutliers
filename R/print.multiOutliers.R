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
  cat("Method:", x$Method)
  cat("\nVariables:", x$Variables)
  cat("\nRow:", x$Row)
  cat("\nScore:", x$Score)

  if (length(x) >= 5) {
    cat("\nOption 1:", names(x)[5], "-", x[[5]])
  }
  if (length(x) >= 6) {
    cat("\nOption 2:", names(x)[6], "-", x[[6]])
  }

  return(x)
}
