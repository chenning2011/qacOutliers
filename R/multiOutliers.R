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


multiOutliers <- function(data, varlist = names(data), method, minPts = 10, k = 5, threshold = 0.95, alpha = 0.1, na.rm = TRUE, ...) {
  # Get the dataset name
  dataset_name <- deparse(substitute(data))

  # Remove missing values if na.rm = TRUE
  if (na.rm) {
    data <- na.omit(data[, varlist])
  }

  # Ensure only numeric variables
  data <- data[sapply(data, is.numeric)]

  # Standardize method argument
  method <- match.arg(method, c("kNN", "LoF", "mahalanobis", "iForest"))

  # LoF method
  if (method == "LoF") {
    # Check if data is a matrix or data frame and convert if necessary
    if (!is.matrix(data) && !is.data.frame(data)) {
      stop("Data should be a matrix or data frame.")
    }

    # Remove any non numeric data
    data <- data[sapply(data, is.numeric)]


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

    # Prepare results
    results <- list(
      Method = "LoF",
      Data = dataset_name,
      Variables = colnames(data),
      Row = outlier_indices,
      Score = if (length(outlier_indices) > 0) lof_scores[outlier_indices] else NULL,
      minPts = minPts,
      Message = if (length(outlier_indices) == 0) "No outliers detected" else NULL
    )

    # Set class for consistency with other outlier detection methods
    class(results) <- "multiOutliers"
    return(results)
  }


  # Mahalanobis method
  if (method == "mahalanobis") {
    library(dplyr)
    library(Routliers)

    # Take only numeric data
    numeric_data <- select_if(data, is.numeric)

    # Convert to matrix
    mat <- as.matrix(numeric_data)

    # Run Mahalanobis outlier detection and store results
    results <- outliers_mahalanobis(x = mat, alpha = alpha)

    #run matrix on function and store results
    #results <- outliers_mahalanobis(x=mat, alpha=alpha)
    # index <- results$outliers_pos
    #
    # #isolate just rows with outliers
    # if(!is.null(index) && length(index) > 0){
    #   subset <- data[index,]
    #   class(subset) <- "multiOutliers"
    # } else {
    #   subset <- "No outliers dectected."
    #   class(subset) <- "multiOutliers"
    # }

    # Extract the outlier indices and their Mahalanobis scores
    outlier_indices <- which(results$outliers == 1)  # Indices of detected outliers
    outlier_scores <- results$scores[outlier_indices]  # Mahalanobis scores for outliers

    # Prepare the result list
    output <- list(
      Method = "mahalanobis",
      Data = dataset_name,          # Store the dataset name
      Variables = colnames(numeric_data),  # Store column names of numeric data
      Row = outlier_indices,        # Row numbers of detected outliers
      Score = outlier_scores,       # Mahalanobis scores of the detected outliers
      alpha = alpha,                # Alpha value used for the detection
      Message = if (length(outlier_indices) == 0) "No outliers detected" else NULL
    )

    # Assign class and return the result
    class(output) <- "multiOutliers"
    return(output)
  }


  # kNN method
  if (method == "kNN") {
    data <- as.matrix(data)
    dist_matrix <- as.matrix(dist(data))

    # Get k-nearest neighbors for each point (excluding self-distance of 0)
    knn_scores <- apply(dist_matrix, 1, function(row) {
      sort(row, partial = k + 1)[2:(k + 1)]
    })

    avg_knn_distances <- rowMeans(knn_scores)
    cutoff <- quantile(avg_knn_distances, threshold)
    outlier_indices <- which(avg_knn_distances > cutoff)

    results <- list(
      Method = "kNN",
      Data = dataset_name,
      Variables = colnames(data),
      Row = outlier_indices,
      Score = if (length(outlier_indices) > 0) avg_knn_distances[outlier_indices] else NULL,
      k = k,
      Message = if (length(outlier_indices) == 0) "No outliers detected" else NULL
    )
    class(results) <- "multiOutliers"
    return(results)
  }

  # Isolation Forest (iForest) method
  if (method == "iForest") {
    if (!is.matrix(data) && !is.data.frame(data)) {
      stop("Data should be a matrix or data frame.")
    }

    numeric_data <- data[sapply(data, is.numeric)]
    isolation_forest_model <- outForest::outForest(numeric_data, replace = "no")
    outlier_indices <- which(outliers(isolation_forest_model) == 1)
    outlier_scores <- isolation_forest_model$outliers_scores[outlier_indices]

    output <- list(
      Method = "iForest",
      Data = dataset_name,
      Variables = colnames(numeric_data),
      Row = outlier_indices,
      Score = if (length(outlier_indices) > 0) outlier_scores else NULL,
      Message = if (length(outlier_indices) == 0) "No outliers detected" else NULL
    )
    class(output) <- "multiOutliers"
    return(output)
  }
}
