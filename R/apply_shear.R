#!/usr/bin/env Rscript


#' apply a shear matrix to a set of points 
#'
#' around a given center of shearing (default: centroid)
#'
#' @export
#'
apply_shear <- function(pointset, shear_matrix, center_of_action = NULL) {
  return(apply_matrix(pointset, shear_matrix, center_of_action))
} # /apply_shear


#' alias for `apply_shear`
#'
#' @export
#'
shear <- apply_shear
