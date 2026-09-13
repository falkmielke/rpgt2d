#!/usr/bin/env Rscript

#' compute root centroid size (RCS), i.e. the scale of the data
#'
#' RCS is a scalar, defined as the root sum of
#' euclidean distances of all points from their centroid.
#' Optionally calculate size along only single axis.
#'
#' @export
#'
compute_rcs <- function(pointset, axis = NULL) {

  extent <- colSums(
    # move to origin for calculation
    rpgt2d::shift_to_point(pointset)^2
  )

  if (is.null(axis)) {
    centroid_size <- sum(extent)
  } else {
    centroid_size <- extent[[axis]]
  }
  
  return(sqrt(centroid_size))
} # /compute_rcs

