#!/usr/bin/env Rscript

#' apply scaling for anisotropic correction (x-RCS to be the same as y-RCS)
#'
#' this does not literally create a square, but corrects the point sets aspect.
#'
#' @param pointset a n-by-2 matrix of n 2D points
#' @param aspect_ratio the aspect ratio by which the
#'                     existing aspect is corrected (divided)
#' @param axis the coordinate axis along which to scale (anisotropic scaling)
#'
#' @returns a transformed pointset
#'
#' @export
#'
make_square_cartesian <- function(pointset, aspect_ratio = NULL, axis = 1) {
  if (is.null(aspect_ratio)) {
    aspect_ratio <- compute_xy_aspect_cartesian(pointset)
  }

  pointset[, axis] <- pointset[, axis] / aspect_ratio

  return(pointset)
} # /make_square_cartesian
