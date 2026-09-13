#!/usr/bin/env Rscript


#' the centroid of a set of points
#'
#' @export
#'
compute_centroid <- function(pointset) {
  return(as.numeric(colMeans(pointset)))
} # /compute_centroid
