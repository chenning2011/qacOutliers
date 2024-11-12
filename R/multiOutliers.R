#'@title Multivariate Outlier Detection
#'@description Identifies multivariate outliers using four different methods.
#'@export
#'@param data a data frame
#'@param varlist a list of numeric variables
#'@param method character, supplies the method to be used for outlier detection. Methods are LoF, kNN, mahalanobis, and iForest
#'@param minPts (optional) numeric, minimum points used for LoF outlier detection. Default value is 5
#'@param k (optional) a k value used for the kNN method of outlier detection. Default value is 5
#'@param threshold (optional) the threshold used for kNN outlier detection. Default value is 0.95
#'@param alpha (optional) the alpha used for mahalanobis distance outlier detection. Default value is 0.1
#'@returns indices of detected outliers, if any
#'@import ggplot2
#'@import Routliers
#'@import dplyr
#'@import outForest
#'@import dbscan
#'@examples
#'data(mtcars)
#'multiOutliers(mtcars, method="mahalanobis")
#'multiOutliers(mtcars, method="LoF")
#'multiOutliers(mtcars, method="kNN")
#'multiOutliers(mtcars, method="iForest")


multiOutliers <- function(data, varlist=names(data), method, minPts=5, k=5, threshold =0.95, alpha=0.1,na.rm=TRUE, ...){
  #removing missing data
  if(na.rm) data <- na.omit(data[,varlist])

  #add other methods as people finish them here
  method <- match.arg(method, c("kNN", "LoF", "mahalanobis", "iForest"))

  if(method=="LoF"){
    # Check if data is a matrix or data frame and convert if necessary
    if (!is.matrix(data) && !is.data.frame(data)) {
      stop("Data should be a matrix or data frame.")
    }

    # Remove any non numeric data
    data <- data[sapply(data, is.numeric)]

    # Check if there are enough points for the LOF calculation
    if (nrow(data) <= minPts) {
      stop("Number of data points must be greater than minPts.")
    }

    # Calculate the LoF scores
    lof_scores <- dbscan::lof(data, minPts = minPts)

    # Append the LOF scores as a new column in the data frame
    data_with_lof <- data.frame(ID = 1:nrow(data), data, LOF_Score = lof_scores)

    subset <- data_with_lof[data_with_lof$LOF_Score > 1, ]
    # Return the data frame with IDs, original data, and LOF scores
    class(subset) <- "multiOutliers"
    return(subset)
  }

  if(method=="mahalanobis"){
    library(dplyr)
    library(Routliers)

    #taking only numeric data
    numeric_data <-select_if(data, is.numeric)

    #make this into a matrix
    mat <- as.matrix(numeric_data)

    #run matrix on function and store results
    results <- outliers_mahalanobis(x=mat, alpha=alpha)
    index <- results$outliers_pos

    #isolate just rows with outliers
    if(!is.null(index) && length(index) > 0){
      subset <- data[index,]
      class(subset) <- "multiOutliers"
    } else {
      subset <- "No outliers dectected."
      class(subset) <- "multiOutliers"
    }
  }

  if (method == "kNN") {
    if (length(index)!=0) {
      data <- as.matrix(data)
    }

    # Calculate pairwise distances
    dist_matrix <- as.matrix(dist(data))

    # Get k-nearest neighbors for each point (excluding self-distance of 0)
    knn_scores <- apply(dist_matrix, 1, function(row) {
      sort(row, partial = k + 1)[2:(k + 1)]
    })

    # Calculate the average distance to the k-nearest neighbors
    avg_knn_distances <- rowMeans(knn_scores)

    # Determine the outliers based on the threshold
    cutoff <- quantile(avg_knn_distances, threshold)
    outlier_indices <- which(avg_knn_distances > cutoff)

    # Prepare results in the specified format
    results <- list(
      Method = "kNN",
      Variables = colnames(data),  # Get variable names from the data matrix
      Row = outlier_indices,       # Row number of each outlier in the original dataframe
      Score = avg_knn_distances[outlier_indices],  # Score of each outlier
      k = k                         # The value of k
    )

    class(results) <- "multiOutliers"
    return(results)
  }



  if(method=="iForest"){
    library(dplyr)
    library(outForest)

    if (!is.matrix(data) && !is.data.frame(data)) {
      stop("Data should be a matrix or data frame.")
    }

    #data needs to be numeric
    numeric_data <-select_if(data, is.numeric)

    ch <- outForest(numeric_data, replace = "no" )

    #actual outliers
    results <- outliers(ch)
    class(results) <- "multiOutliers"
    return(results)
  }
}


