#'@title Plotting Multivariate Outlier Detection
#'@description Plots identified multivariate outliers using either kNN, iForest, mahalanobis, LoF
#'@export
#'@param x the results from a multivariate outlier detection function (e.g., kNN, iForest, mahalanobis, LoF)
#'@returns A formatted plot of the results of the multivariate outlier detection
#'@import ggplot2
#'@import dplyr
#'@import gridExtra
#'@import grid
#'@import ggrepel
#'@examples
#'data(iris)
#'results <- multiOutliers(iris, method="iForest")
#'plot(results)

plot.multiOutliers <- function(data, varlist = names(data), methods = c("kNN", "LoF", "mahalanobis", "iForest"),
                               minPts = 10, k = 5, threshold = 0.95, alpha = 0.1, na.rm = TRUE, ...) {
  # Check that the methods are valid
  if (any(!methods %in% c("kNN", "LoF", "mahalanobis", "iForest"))) {
    stop("Invalid method. Please choose from 'kNN', 'LoF', 'mahalanobis', or 'iForest'.")
  }

  plots <- list()

  # Loop
  for (method in methods) {
    # Call the multiOutliers function to get the outliers
    results <- multiOutliers(data, varlist, method, minPts, k, threshold, alpha, na.rm, ...)

    outlier_indices <- results$Row
    outlier_scores <- results$Score
    non_outliers <- setdiff(1:nrow(data), outlier_indices)

    plot_data <- data.frame(
      Row = 1:nrow(data),
      Value = data[, varlist[2]], # Using the first variable for plotting
      Outlier = ifelse(1:nrow(data) %in% outlier_indices, "Outlier", "Normal")
    )

    plot <- ggplot(plot_data, aes(x = Row, y = Value)) +
      geom_point(aes(color = Outlier), size = 3, alpha=.7) +
      geom_text_repel(
        data = plot_data[plot_data$Outlier == "Outlier",],
        aes(label = Row),
        vjust = -0.5,
        color = "black",
        size = 3
      ) +
      scale_color_manual(values = c("Normal" = "#666666", "Outlier" = "red")) +
      labs(title = paste("Outlier Detection using", method, "Method"),
           x = "Row Index", y = varlist[1]) +
      theme_minimal()

    plots[[method]] <- plot
  }
  # Create a text object
  text_grob <- textGrob("Values above Points are \n Row Numbers", gp = gpar(col = "black", fontsize = 15, fill = "lightblue"))

  # Use grid.arrange to combine the plot and the text outside it
  plot_w_text<- grid.arrange(
    plot,
    text_grob,
    ncol = 1,
    heights = c(5, 1)  # Adjust relative heights of plot and text
  )

  return(plots)
}
