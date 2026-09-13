#!/usr/bin/env Rscript

#' move pointset so that its centroid matches a target point
#'
#' Default target point is the coordinate origin
#'
#' @export
#'
shift_to_point <- function(pointset, target = NULL) {

  # if no target is given, move points to the coordinate origin
  if (is.null(target)) {
    target <- c(0., 0.)
  }

  # starting point: the pointset centroid
  centroid <- rpgt2d::compute_centroid(pointset)

  # move pointset with `apply_translation`
  return(
    rpgt2d::apply_translation(
      pointset,
      shift = as.numeric(target) - as.numeric(centroid)
    )
  )
} # /shift_to_point
