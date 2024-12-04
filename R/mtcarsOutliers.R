#' mtcars dataset from base R with outliers
#'
#' Modified version of mtcars with generated outliers.
#'
#' @format A data frame with 32 rows and 11 variables:
#' \describe{
#'   \item{\code{mpg}}{double. miles per gallon}
#'   \item{\code{cyl}}{factor number of cylinders}
#'   \item{\code{disp}}{double. displacement (cu.in.)}
#'   \item{\code{hp}}{double. gross horsepower}
#'   \item{\code{drat}}{double. rear axle ratio}
#'   \item{\code{wt}}{double. weight (1000 lbs)}
#'   \item{\code{qsec}}{double. 1/4 mile time}
#'   \item{\code{vs}}{factor engine (0 = v-shaped, 1 = straight)}
#'   \item{\code{am}}{factor transmission (0 = automatic, 1 = manual)}
#'   \item{\code{gear}}{factor number of forward gears}
#'   \item{\code{carb}}{factor number of carburetors}
#' }
#'
#' @source mtcars dataset comes from base R. Outliers are generated using the [generateOutliers](https://www.rdocumentation.org/packages/outForest/versions/1.0.1/topics/generateOutliers) function from the outForest package.
"mtcarsOutliers"
