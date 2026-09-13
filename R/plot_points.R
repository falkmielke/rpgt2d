#!/usr/bin/env Rscript

#' quickly get an overview of the points of interest
#'
#' @export
#'
plot_points <- function(points, ref_points = NULL, ...) {
  ptmat <- as.matrix(points)

  all_points <- ptmat
  if (isFALSE(is.null(ref_points))) {
    ref_points <- as.matrix(ref_points)
    all_points <- rbind(all_points, ref_points)
  }

  passepartout <- 0.1 * sd(all_points)

  par(pty="s")
  plot(
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

  text(
    sweep(ptmat, 2, 0.02 * sd(ptmat), FUN = "+"),
    labels = row.names(points),
    pos = 4
  )

  if (isFALSE(is.null(ref_points))) {
    rpx <- ref_points[, 1]
    rpy <- ref_points[, 2]
    points(rpx, rpy, col = "gray", cex = 1.2, pch = 4)
  }

} # /plot_points
