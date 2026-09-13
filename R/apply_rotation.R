#!/usr/bin/env Rscript


#' apply a rotation matrix to a set of points
#'
#' around a given center of rotation (default: centroid)
#'
#' @param pointset a n-by-2 matrix of n 2D points
#' @param rotation_matrix a 2-by-2 rotation matrix
#' @param center_of_action an optional origin point (2D position vector)
#'                 from which to apply rotation
#'
#' @returns a transformed pointset
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
