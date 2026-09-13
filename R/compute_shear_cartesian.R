#!/usr/bin/env Rscript


#' calculate the actual shear of a pointset relative to reference
#'
#' optionally fixing either direction
#'
#' @param pointset a n-by-2 matrix of n 2D points
#' @param reference_pointset another 2D pontset (same n-by-2 dimensions) 
#'        which serves as reference
#' @param fix_x do not shear along the first dimension
#' @param fix_y do not shear along the second dimension
#'
#' @export
#'
compute_shear_cartesian <- function(pointset, reference_pointset, fix_x = FALSE, fix_y = FALSE) {

  # TODO might be an option to have a neutral reference_pointset,
  #      e.g. the coordinate axes
  #      thereby "standardizing" the shear
  #      probably this is just the linar trend f in y = m x, eval at y=1
  #      SVD / eigenspace might help to generalize (scaling eigenvectors)
  #      https://stackoverflow.com/questions/59395218/pcl-scale-two-point-clouds-to-the-same-size

  x_pts <- pointset[, 1]
  y_pts <- pointset[, 2]
  x_ref <- reference_pointset[, 1]
  y_ref <- reference_pointset[, 2]
  s_x <- if (fix_y) 0 else sum((y_pts - y_ref) * x_ref) / sum(x_ref^2)
  s_y <- if (fix_x) 0 else sum((x_pts - x_ref) * y_ref) / sum(y_ref^2)

  # concatenate the shear matrix
  shear_matrix <- as.matrix(rbind(c(1, s_x), c(s_y, 1)))

  return(shear_matrix)
} # /compute_shear_cartesian
