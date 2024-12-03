#'@title Multivariate Outlier Detection
#'@description Identifies multivariate outliers using four different methods.
#'@export
#'@param data a data frame
#'@param varlist a list of variables. The variables can be factor or numeric. Categorical variables are dropped when using the mahalanobis and kNN methods.
#'@param method character, supplies the method to be used for outlier detection. Methods are LoF, kNN, mahalanobis, and iForest.
#'@param minPts (optional) numeric, minimum points used for LoF outlier detection. Default value is 5
#'@param k (optional) a k value used for the kNN method of outlier detection. Default value is 5
#'@param threshold (optional) the threshold used for kNN outlier detection. Default value is 0.95
#'@param alpha (optional) the alpha used for mahalanobis distance outlier detection. Default value is 0.1
#'@param ntrees (optional) the number of trees used in iForest outlier detection. Default value is 100
#'@param n (optional) the number of points to take as outliers in iForest outlier detection. Default value is 5
#'@param na.rm (optional) logical, specifies whether to remove NA values. Defaults to TRUE. If the value is TRUE, removes missing data through listwise deletion.
#'@returns method used, dataset used, variables used for outliers detected, indices of any detected outliers, scores for the outliers, and values for optional parameters
#'@import ggplot2
#'@import Routliers
#'@import outliers
#'@import dplyr
#'@import dbscan
#'@import isotree
#'@import FNN
#'@importFrom ("utils", "head")
#'
#'@examples
#'multiOutliers(mtcarsOutliers, method="mahalanobis", alpha=0.1)
#'multiOutliers(mtcarsOutliers, method="LoF", minPts=5)
#'multiOutliers(mtcarsOutliers, method="kNN", k=5, threshold=.95)
#'multiOutliers(mtcarsOutliers, method="iForest", ntrees = 50)


#add in see also and link to other functions that we build our functions off of
multiOutliers <- function(data, varlist = names(data), method, minPts = 10, k = 5, threshold = 0.95, alpha = 0.1, ntrees = 100, n=5,na.rm = TRUE) {
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

    # Ensure the number of points is greater than minPts
    if (nrow(data) <= minPts) {
      stop("Number of data points must be greater than minPts.")
    }

    #check if data is categorical or numeric, if any variables are categorical then need to use gower distance instead of normalizing
    is_categorical <- any(sapply(data, function(x) is.factor(x) || is.character(x)))

    #if there are any categorical variables, do gower distance
    if(is_categorical){
      #taking gower distances
      data_stand <- cluster::daisy(data, metric="gower")

      #taking LoF scores
      lof_scores <- dbscan::lof(data_stand, minPts = minPts)

      #identify outliers based on the threshold
      outlier_indices <- which(lof_scores > 1)
      outlier_scores <- lof_scores[outlier_indices]

      #adding score column into the dataset
      data$scores <- lof_scores
    }

    #otherwise, use normalization of the data
    else{
      # Normalize the data if not already scaled
      data_stand <- scale(data)

      # Apply the LoF method from the dbscan package
      lof_scores <- dbscan::lof(as.matrix(data_stand), minPts = minPts)

      # Identify outliers based on a threshold (LoF score > 1 for stronger outliers)
      outlier_indices <- which(lof_scores > 1)
      outlier_scores <- lof_scores[outlier_indices]

      #adding score column into the dataset
      data$scores <- lof_scores
    }

    #list of results
    results <- list(
      Method = "LoF",
      Dataset = dataset_name,
      Variables = colnames(data),
      Row = outlier_indices,
      Score = outlier_scores,
      Message = if (length(outlier_indices) == 0) "No outliers detected" else "Outliers detected",
      Data = data,
      minPts = minPts
    )

    # Set class for consistency with other outlier detection methods
    class(results) <- "multiOutliers"
    return(results)
  }

  # Mahalanobis method
  if (method == "mahalanobis") {
    # Remove any non numeric data
    data <- data[sapply(data[,varlist], is.numeric)]

    #convert to matrix
    mat <- as.matrix(data)

    #run matrix on function and store results
    results <- Routliers::outliers_mahalanobis(x=mat, alpha=alpha)
    index <- results$outliers_pos

    #extract the outlier indices and their Mahalanobis scores
    outlier_scores <- results$dist_from_center[index]

    #adding scores back into dataset
    data$scores <- results$dist_from_center

    #prepare the result list
    output <- list(
      Method = "mahalanobis",
      Dataset = dataset_name,          # Store the dataset name
      Variables = colnames(data),  # Store column names of numeric data
      Row = index,        # Row numbers of detected outliers
      Score = outlier_scores,       # Mahalanobis scores of the detected outliers
      Message = if (length(index) == 0) "No outliers detected" else "Outliers detected",
      alpha = alpha,
      Data = data
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
    # Remove any non numeric data
    data <- data[sapply(data[,varlist], is.numeric)]

    # Calculate kNN distances
    knn_distances <- FNN::knn.dist(data, k = k)

    # Calculate the average kNN distance for each row
    avg_knn_distances <- rowMeans(knn_distances)

    # Define a threshold for detecting outliers
    # Outliers are rows with distances greater than a certain threshold
    threshold <- mean(avg_knn_distances) + 2 * sd(avg_knn_distances)  # Example: mean + 2 SD

    # Identify outliers based on the threshold
    outlier_indices <- which(avg_knn_distances > threshold)
    outlier_scores <- avg_knn_distances[outlier_indices]

    #adding the scores back into the dataset
    data$scores <- avg_knn_distances

    # Create the result list
    results <- list(
      Method = "kNN",
      Dataset = dataset_name,
      Variables = colnames(data),
      Row = outlier_indices,
      Score = outlier_scores,
      Message = if (length(outlier_indices) == 0) "No outliers detected" else "Outliers detected",
      k = k,
      Data = data
    )

    class(results) <- "multiOutliers"
    return(results)
  }

  # Isolation Forest (iForest) method
  if (method == "iForest") {
    if (!is.matrix(data) && !is.data.frame(data)) {
      stop("Data should be a matrix or data frame.")
    }

    #running iforest model
    isolation_forest_model <- isotree::isolation.forest(data, ntrees = ntrees)

    #getting isoscores
    data$scores <- predict(isolation_forest_model, data)

    #taking the top n points as outliers
    subset <- head(data[order(-data$scores),],n)

    #getting row numbers and scores for the outliers
    outlier_indices <- which(rownames(data) %in% rownames(subset))
    outlier_scores <- subset$scores

    #results
    output <- list(
      Method = "iForest",
      Dataset = dataset_name,
      Variables = colnames(data),
      Row = outlier_indices,
      Score = outlier_scores,
      Message = if (length(outlier_indices) == 0) "No outliers detected" else "Outliers detected",
      ntrees = ntrees,
      n = n,
      Data = data
    )
    class(output) <- "multiOutliers"
    return(output)
  }
}

