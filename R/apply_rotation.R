#!/usr/bin/env Rscript


#' apply a rotation matrix to a set of points 
#'
#' around a given center of rotation (default: centroid)
#'
#' @export
#'
apply_rotation <- function(pointset, rotation_matrix, center_of_action = NULL) {
  return(apply_matrix(pointset, rotation_matrix, center_of_action))
} # /apply_rotation


#' alias for `apply_rotation`
#'
#' @export
#'
rotate <- apply_rotation
