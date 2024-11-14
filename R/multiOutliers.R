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
#'@import FNN
#'@examples
#'multiOutliers(mtcarsOutliers, method="mahalanobis", alpha=0.1)
#'multiOutliers(mtcarsOutliers, method="LoF", minPts=5)
#'multiOutliers(mtcarsOutliers, method="kNN", k=5, threshold=.95)
#'multiOutliers(mtcarsOutliers, method="iForest")


multiOutliers <- function(data, varlist = names(data), method, minPts = 10, k = 5, threshold = 0.95, alpha = 0.1, na.rm = TRUE, ...) {
  # Get the dataset name
  dataset_name <- deparse(substitute(data))

  #removing missing data
  if(na.rm) data <- na.omit(data[,varlist])

  # Standardize method argument
  method <- match.arg(method, c("kNN", "LoF", "mahalanobis", "iForest"))

  # LoF method
  if (method == "LoF") {
    # Check if data is a matrix or data frame and convert if necessary
    if (!is.matrix(data) && !is.data.frame(data)) {
      stop("Data should be a matrix or data frame.")
    }

    # Remove any non numeric data
    data <- data[sapply(data[,varlist], is.numeric)]

    # Ensure the number of points is greater than minPts
    if (nrow(data) <= minPts) {
      stop("Number of data points must be greater than minPts.")
    }

    # Normalize the data if not already scaled
    data_scaled <- scale(data)

    # Apply the LoF method from the dbscan package
    lof_scores <- dbscan::lof(as.matrix(data_scaled), minPts = minPts)

    # Identify outliers based on a threshold (LoF score > 1.5 for stronger outliers)
    outlier_indices <- which(lof_scores > 1.5)
    outlier_scores <- lof_scores[outlier_indices]

    # Prepare results
    results <- list(
      Method = "LoF",
      Data = dataset_name,
      Variables = colnames(data),
      Row = outlier_indices,
      Score = outlier_scores,
      Message = if (length(outlier_indices) == 0) "No outliers detected" else "Outliers detected",
      minPts = minPts
    )

    # Set class for consistency with other outlier detection methods
    class(results) <- "multiOutliers"
    return(results)
  }


  # Mahalanobis method
  if (method == "mahalanobis") {
    library(dplyr)
    library(Routliers)

    #take only numeric data
    numeric_data <- select_if(data[,varlist], is.numeric)

    #convert to matrix
    mat <- as.matrix(numeric_data)

    #run matrix on function and store results
    results <- outliers_mahalanobis(x=mat, alpha=alpha)
    index <- results$outliers_pos

    #extract the outlier indices and their Mahalanobis scores
    outlier_scores <- results$dist_from_center[index]  # Mahalanobis scores for outliers

    #prepare the result list
    output <- list(
      Method = "mahalanobis",
      Data = dataset_name,          # Store the dataset name
      Variables = colnames(numeric_data),  # Store column names of numeric data
      Row = index,        # Row numbers of detected outliers
      Score = outlier_scores,       # Mahalanobis scores of the detected outliers
      Message = if (length(index) == 0) "No outliers detected" else "Outliers detected",
      alpha = alpha
    )

    #assign class and return the result
    class(output) <- "multiOutliers"
    return(output)
  }


  # kNN method
  if (method == "kNN") {
    if (!is.data.frame(data) && !is.matrix(data)) {
      stop("Data must be a data frame or matrix.")
    }
    require(FNN)

    # Calculate kNN distances
    knn_distances <- knn.dist(data[,varlist], k = k)

    # Calculate the average kNN distance for each row
    avg_knn_distances <- rowMeans(knn_distances)

    # Define a threshold for detecting outliers
    # Outliers are rows with distances greater than a certain threshold
    threshold <- mean(avg_knn_distances) + 2 * sd(avg_knn_distances)  # Example: mean + 2 SD

    # Identify outliers based on the threshold
    outlier_indices <- which(avg_knn_distances > threshold)
    outlier_scores <- avg_knn_distances[outlier_indices]

    # Create the result list
    results <- list(
      Method = "kNN",
      Data = dataset_name,
      Variables = colnames(data),
      Row = outlier_indices,
      Score = outlier_scores,
      Message = if (length(outlier_indices) == 0) "No outliers detected" else "Outliers detected",
      k = k
    )

    class(results) <- "multiOutliers"
    return(results)
  }

  # Isolation Forest (iForest) method
  if (method == "iForest") {
    if (!is.matrix(data) && !is.data.frame(data)) {
      stop("Data should be a matrix or data frame.")
    }

    #changing to numeric data
    numeric_data <- data[sapply(data[,varlist], is.numeric)]

    #running iForest model
    isolation_forest_model <- outForest(numeric_data, replace = "no", verbose = 0)

    #extract row numbers and scores
    outlier_indices <- isolation_forest_model$outliers$row
    outlier_scores <- isolation_forest_model$outliers$score

    output <- list(
      Method = "iForest",
      Data = dataset_name,
      Variables = colnames(numeric_data),
      Row = outlier_indices,
      Score = outlier_scores,
      Message = if (length(outlier_indices) == 0) "No outliers detected" else "Outliers detected"
    )
    class(output) <- "multiOutliers"
    return(output)
  }
}

