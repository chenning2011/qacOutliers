#' mtcars dataset from base R with outliers
#'
#' Modified version of mtcars with generated outliers.
#'
#' @format A data frame with 32 rows and 11 variables:
#' \describe{
#'   \item{\code{mpg}}{double. miles per gallon}
#'   \item{\code{cyl}}{double. number of cylinders}
#'   \item{\code{disp}}{double. displacement (cu.in.)}
#'   \item{\code{hp}}{double. gross horsepower}
#'   \item{\code{drat}}{double. rear axle ratio}
#'   \item{\code{wt}}{double. weight (1000 lbs)}
#'   \item{\code{qsec}}{double. 1/4 mile time}
#'   \item{\code{vs}}{double. engine (0 = v-shaped, 1 = straight)}
#'   \item{\code{am}}{double. transmission (0 = automatic, 1 = manual)}
#'   \item{\code{gear}}{double. number of forward gears}
#'   \item{\code{carb}}{double. number of carburetors}
#' }
#'
#' @source mtcars dataset comes from base R, and the outliers were generated using the `generateOutliers` function in the outForest package.
"mtcarsOutliers"
