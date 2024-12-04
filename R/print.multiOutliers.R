#'@title Printing Multivariate Outlier Detection
#'@description Prints identified multivariate outliers
#'@export
#'@param x the results from "multiOutliers" function
#'@param ... not used
#'@returns A formatted print of the results of the "multiOutliers" function
#'@import ggplot2
#'@import Routliers
#'@import dplyr
#'@import cli
#'@importFrom utils head
#'@examples
#'#iForest
#'results <- multiOutliers(irisOutliers, method = "iforest")
#'results
#'
#'#LoF
#'results <- multiOutliers(irisOutliers, method = "lof")
#'results
#'
#'#kNN
#'results <- multiOutliers(irisOutliers, method = "knn")
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
  cli::cli_h1("Summary Information")
  cat("\nMethod:", x$Method)
  cat("\nDataset:", x$Dataset)
  cat("\nVariables:", x$Variables)
  cat("\nRow:", x$Row)
  cat("\nOutlier Score:", x$Score)
  cat("\nMessage: ", x$Message)

  if (length(x)> 6){
    option_count <- 1
    for (i in 7:length(x)) {
      if(names(x)[i]!="Data"){
        cat("\nOption", option_count, ":", names(x)[i], "=", x[[i]])
        option_count <- option_count+1
      }
    }
  }

  cat("\n")
  cli::cli_h1("Dataset Information")
  cat("\nFive Largest Outliers Within the Provided Dataset:\n")
  print(head(x$Data[order(-x$Data$scores),],5))

  return(invisible(x))
}
