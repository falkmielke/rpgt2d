#!/usr/bin/env Rscript

#' quickly get an overview of the points of interest
#'
#' @param pointset a n-by-2 matrix of n 2D points
#' @param reference_pointset optional n-by-2 matrix of n 2D points which
#'        are plotted as reference points
#' @param ... more parameters for `graphics::plot`
#'        (already in use: col, xlim, ylim, xlab, ylab)
#'
#' @export
#'
plot_points <- function(pointset, reference_pointset = NULL, ...) {
  ptmat <- as.matrix(pointset)

  all_points <- ptmat
  if (isFALSE(is.null(reference_pointset))) {
    reference_pointset <- as.matrix(reference_pointset)
    all_points <- rbind(all_points, reference_pointset)
  }

  passepartout <- 0.1 * stats::sd(all_points)

  graphics::par(pty="s")
  graphics::plot(
    ptmat,
    col = "black",
    xlim = c(
      min(all_points[, 1]) - passepartout,
      max(all_points[, 1]) + passepartout
    ),
    ylim = c(
      min(all_points[, 2]) - passepartout,
      max(all_points[, 2]) + passepartout
    ),
    xlab = "x", ylab = "y",
    ...
  )

  graphics::text(
    sweep(ptmat, 2, 0.02 * stats::sd(ptmat), FUN = "+"),
    labels = row.names(pointset),
    pos = 4
  )

  if (isFALSE(is.null(reference_pointset))) {
    rpx <- reference_pointset[, 1]
    rpy <- reference_pointset[, 2]
    graphics::points(rpx, rpy, col = "gray", cex = 1.2, pch = 4)
  }

} # /plot_points
