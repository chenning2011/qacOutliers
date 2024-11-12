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
  return(x)
}
