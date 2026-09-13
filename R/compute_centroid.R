#!/usr/bin/env Rscript


#' the centroid of a set of points
#'
#' @param pointset a n-by-2 matrix of n 2D points
#'
#' @export
#'
compute_centroid <- function(pointset) {
  return(as.numeric(colMeans(pointset)))
} # /compute_centroid
