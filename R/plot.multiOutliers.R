#'@title Plotting Multivariate Outlier Detection
#'@description Plots identified multivariate outliers using either kNN, iForest, mahalanobis, LoF
#'@export
#'@param x the results from a multivariate outlier detection function (e.g., kNN, iForest, mahalanobis, LoF)
#'@param ... not used
#'@returns A formatted plot of the results of the multivariate outlier detection. Row indices are on the x-axis and ou
#'@import ggplot2
#'@import dplyr
#'@import gridExtra
#'@import grid
#'@import ggrepel
#'@examples
#'results <- multiOutliers(irisOutliers, method="iForest")
#'plot(results)
#'
#'results <- multiOutliers(irisOutliers, method = "kNN")
#'plot(results)
#'
#'results <- multiOutliers(irisOutliers, method = "LoF")
#'plot(results)
#'
#'results <- multiOutliers(irisOutliers, method = "mahalanobis")
#'plot(results)
#'
#'plot(multiOutliers(irisOutliers, method = "mahalanobis"))

plot.multiOutliers <- function(x, ...) {

  if(!inherits(x, "multiOutliers")){
    stop("This functon requires an object created by multiOutliers")
  }
  outlier_indices <- x$Row

  plot_data <- data.frame(
    Row = 1:nrow(x$Data),
    Value = x$Data$scores,
    Outlier = ifelse(1:nrow(x$Data) %in% outlier_indices, "Outlier", "Normal")
  )

  plot <- ggplot2::ggplot(plot_data, ggplot2::aes(x = Row, y = Value)) +
    ggplot2::geom_point(aes(color = Outlier), size = 3, alpha=.7) +
    ggrepel::geom_text_repel(
      data = plot_data[plot_data$Outlier == "Outlier",],
      aes(label = Row),
      vjust = -0.5,
      color = "black",
      size = 3
    ) +
    ggplot2::scale_color_manual(values = c("Normal" = "#666666", "Outlier" = "red")) +
    ggplot2::labs(title = paste("Outlier Detection using",x$Method, "Method"),
                  x = "Row Index", y = "Outlier Score") +
    ggplot2::theme_minimal()

  if(x$Method == "LoF"){
    plot <- plot + geom_hline(yintercept = 1, linetype = "dashed", alpha = 0.8)
  }

  text_grob <- grid::textGrob("Values below points are row numbers", gp = gpar(col = "black", fontsize = 15, fill = "lightblue"))

  # Use grid.arrange to combine the plot and the text outside it
  plot_w_text<- gridExtra::grid.arrange(
    plot,
    text_grob,
    ncol = 1,
    heights = c(5, 1)  # Adjust relative heights of plot and text
  )
}
