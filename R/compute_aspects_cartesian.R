#!/usr/bin/env Rscript


#' compute the aspect ratios of a point set, relative to one axis
#'
#' compute ratios of the centroid size axes relative to one focal axis
#'
#' @export
#'
compute_aspects_cartesian <- function(pointset, axis = 1) {

  # center the set of points, i.e. move centroid to origin
  pointset <- rpgt2d::shift_to_point(pointset)

  # scale of the focal axis
  rcs_col <- sqrt(sum(pointset[, axis]^2))

  # scale of all axes
  rcs_all <- sqrt(colSums(pointset^2))

  return(rcs_col / rcs_all)
} # /compute_aspects_cartesian


#' specifically compute the x/y aspect along cartesian axes
#'
#' @export
#'
compute_xy_aspect_cartesian <- \(ps) rpgt2d::compute_aspects_cartesian(ps)[[2]]
