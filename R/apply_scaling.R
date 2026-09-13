#!/usr/bin/env Rscript


#' scale a set of point by a factor (without moving)
#'
#' scaling has to happen around the points' centroid
#' optional anisotropic scaling: choose one axis
#' optionally set a fixpoint (for scaling off-centroid)
#'
#' @export
#'
apply_scaling <- function(pointset, scaling, axis = NULL, fixpoint = NULL) {

  # temporarily center, i.e. shift centroid to origin, or use fixpoint
  centroid <- rpgt2d::compute_centroid(pointset)
  if (is.null(fixpoint)) {
    pointset <- rpgt2d::shift_to_point(pointset, target = c(0., 0.))
    fixpoint <- centroid
  } else {
    pointset <- rpgt2d::apply_translation(pointset, shift = -fixpoint)
  }

  # scale
  if (is.null(axis)) {
    pointset <- pointset * scaling
  } else {
    # scale only one axis
    pointset[, axis] <- pointset[, axis] * scaling
  }

  # reverse shift to center
  pointset <- rpgt2d::apply_translation(pointset, shift = fixpoint)

  return(pointset)
} # /apply_scaling
