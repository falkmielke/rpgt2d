#!/usr/bin/env Rscript


#' Scale the points so that their RCS matches a target size
#'
#' By default, pointset is scaled to unit RCS.
#' Optionally scale only a single axis,
#' or relative to a fixpoint (which will change centroid position)
#'
#' @export
#'
scale_to_size <- function(pointset, target = NULL, axis = NULL, fixpoint = NULL) {

  # if no target is given, scale to unit size
  if (is.null(target)) {
    target <- 1.
  }

  # original scale: the RCS
  rcs <- rpgt2d::compute_rcs(pointset, axis = axis)

  # apply the target scaling to pointset
  return(
    rpgt2d::apply_scaling(
      pointset,
      scaling = target / rcs,
      axis = axis,
      fixpoint = fixpoint
    )
  )

} # /scale_to_size
