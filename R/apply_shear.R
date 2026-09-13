#!/usr/bin/env Rscript


#' apply a shear matrix to a set of points 
#'
#' around a given center of shearing (default: centroid)
#'
#' @param pointset a n-by-2 matrix of n 2D points
#' @param shear_matrix a 2-by-2 shear matrix matrix
#' @param center_of_action an optional origin point (2D position vector)
#'                 from which to apply the shear transformation
#'
#' @returns a transformed pointset
#'
#' @export
#'
apply_shear <- function(pointset, shear_matrix, center_of_action = NULL) {
  return(apply_matrix(pointset, shear_matrix, center_of_action))
} # /apply_shear


#' alias for `apply_shear`
#'
#' @inheritParams apply_shear
#'
#' @export
#'
shear <- apply_shear
