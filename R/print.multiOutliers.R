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
#'#iForest
#'results <- multiOutliers(irisOutliers, method = "iForest")
#'results
#'
#'#LoF
#'results <- multiOutliers(irisOutliers, method = "LoF")
#'results
#'
#'#kNN
#'results <- multiOutliers(irisOutliers, method = "kNN")
#'results
#'
#'#Mahalanobis
#'results <- multiOutliers(irisOutliers, method = "mahalanobis")
#'results
#'

print.multiOutliers <- function(x,...) {
  if(!inherits(x, "multiOutliers")){
    stop("This functon requires an object created by multiOutliers")
  }
  cat("\nMethod:", x$Method)
  cat("\nDataset:", x$Data)
  cat("\nVariables:", x$Variables)
  cat("\nRow:", x$Row)
  cat("\nOutlier Score:", x$Score)
  cat("\nMessage: ", x$Message)

  if (length(x)> 6){
    for (i in 7:length(x)) {
      cat("\nOption", i - 6, ":", names(x)[i], "=", x[[i]])
   }
  }

  return(invisible(x))
}
