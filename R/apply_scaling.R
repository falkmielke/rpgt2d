#!/usr/bin/env Rscript


#' scale a set of point by a factor (without moving)
#'
#' scaling has to happen around the points' centroid
#' optional anisotropic scaling: choose one axis
#' optionally set a fixpoint (for scaling off-centroid)
#'
#' @param pointset a n-by-2 matrix of n 2D points
#' @param scaling a scalar by which to scale the points
#' @param axis the coordinate axis along which to scale (anisotropic scaling)
#' @param fixpoint an optional origin point (2D position vector)
#'                 from which to apply scaling
#'
#' @returns a transformed pointset
#'
#' @export
#'
apply_scaling <- function(pointset, scaling, axis = NULL, fixpoint = NULL) {

  # temporarily center, i.e. shift centroid to origin, or use fixpoint
  centroid <- compute_centroid(pointset)
  if (is.null(fixpoint)) {
    pointset <- shift_to_point(pointset, target = c(0., 0.))
    fixpoint <- centroid
  } else {
    pointset <- apply_translation(pointset, shift = -fixpoint)
  }

  # scale
  if (is.null(axis)) {
    pointset <- pointset * scaling
  } else {
    # scale only one axis
    pointset[, axis] <- pointset[, axis] * scaling
  }

  # reverse shift to center
  pointset <- apply_translation(pointset, shift = fixpoint)

  return(pointset)
} # /apply_scaling
