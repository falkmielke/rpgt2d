#!/usr/bin/env Rscript


#' Scale the points so that their RCS matches a target size
#'
#' By default, pointset is scaled to unit RCS.
#' Optionally scale only a single axis,
#' or relative to a fixpoint (which will change centroid position)
#'
#' @param pointset a n-by-2 matrix of n 2D points
#' @param target a scalar to become the new RCS
#' @param axis the coordinate axis along which to scale (anisotropic scaling)
#' @param fixpoint an optional origin point (2D position vector)
#'                 from which to apply scaling
#'
#' @returns a transformed pointset
#'
#' @export
#'
scale_to_size <- function(pointset, target = NULL, axis = NULL, fixpoint = NULL) {

  # if no target is given, scale to unit size
  if (is.null(target)) {
    target <- 1.
  }

  # original scale: the RCS
  rcs <- compute_rcs(pointset, axis = axis)

  # apply the target scaling to pointset
  return(
    apply_scaling(
      pointset,
      scaling = target / rcs,
      axis = axis,
      fixpoint = fixpoint
    )
  )

} # /scale_to_size
