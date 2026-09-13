#!/usr/bin/env Rscript

#' move pointset so that its centroid matches a target point
#'
#' Default target point is the coordinate origin
#'
#' @param pointset a n-by-2 matrix of n 2D points
#' @param target a 2D position vector to become the new centroid
#'
#' @returns a transformed pointset
#'
#' @export
#'
shift_to_point <- function(pointset, target = NULL) {

  # if no target is given, move points to the coordinate origin
  if (is.null(target)) {
    target <- c(0., 0.)
  }

  # starting point: the pointset centroid
  centroid <- compute_centroid(pointset)

  # move pointset with `apply_translation`
  return(
    apply_translation(
      pointset,
      shift = as.numeric(target) - as.numeric(centroid)
    )
  )
} # /shift_to_point
