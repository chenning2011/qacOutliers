#'@title Multivariate outlier detection
#'@description Identifies multivariate outliers
#'@export
#'@param data a data frame
#'@param x a numeric variable
#'@param y a numeric variable
#'@param method character, supplies the method to be used for outlier detection
#'@returns indices of detected outliers, if any
#'@import ggplot2
#'@import Routliers
#'@import dplyr
#'@examples
#'multiOutliers(mtcars, hisp, cyl, method="mahalanobis")
#'

multiOutliers <- function(data, x, y, method="mahalanobis", ...){
  #add other methods as people finish them here

  if(method=="LoF"){
    # Check if data is a matrix or data frame and convert if necessary
    if (!is.matrix(data) && !is.data.frame(data)) {
      stop("Data should be a matrix or data frame.")
    }

    # Rmove any non numeric data
    data <- data[sapply(data, is.numeric)]

    # Check if there are enough points for the LOF calculation
    if (nrow(data) <= minPts) {
      stop("Number of data points must be greater than minPts.")
    }

    # Calculate the LoF scores
    lof_scores <- dbscan::lof(data, minPts = minPts)

    # Append the LOF scores as a new column in the data frame
    data_with_lof <- data.frame(ID = 1:nrow(data), data, LOF_Score = lof_scores)


    # Return the data frame with IDs, original data, and LOF scores
    return(data_with_lof)
  }

  if(method=="mahalanobis"){

    #create error messaging here for non-numeric variables

    #select just the rows given by the user
    subset <- select(data, {{x}}, {{y}})

    #make this into a matrix
    mat <- as.matrix(subset)

    #run matrix on function and store results
    results <- outliers_mahalanobis(x=mat)
    print(results)
  }
  else{
    stop("Method supplied must be kNN, mahalanobis, iForest, or LoF.")
  }
}
