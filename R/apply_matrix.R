#!/usr/bin/env Rscript


#' apply any matrix transformation to a set of points 
#'
#' optional: given center of action (default: centroid)
#'
#' @export
#'
apply_matrix <- function(pointset, transformation_matrix, center_of_action = NULL) {

  original_centroid <- rpgt2d::compute_centroid(pointset)

  # default: center of action is the centroid
  if (is.null(center_of_action)) {
    center_of_action <- original_centroid
  }

  pointset <- rpgt2d::apply_translation(pointset, -center_of_action)

  # apply rotation
  pointset <- as.matrix(pointset) %*% transformation_matrix


  # revert shift
  pointset <- rpgt2d::apply_translation(pointset, center_of_action)


  return(pointset)
} # /apply_matrix
