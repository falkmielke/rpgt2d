#!/usr/bin/env Rscript

#' apply scaling for anisotropic correction (x-RCS to be the same as y-RCS)
#'
#' this does not literally create a square, but corrects the point sets aspect.
#'
#' @export
#'
make_square_cartesian <- function(pointset, aspect_ratio = NULL, axis = 1) {
  if (is.null(aspect_ratio)) {
    aspect_ratio <- rpgt2d::compute_xy_aspect_cartesian(pointset)
  }

  pointset[, axis] <- pointset[, axis] / aspect_ratio

  return(pointset)
} # /make_square_cartesian
