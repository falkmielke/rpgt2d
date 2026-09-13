#!/usr/bin/env Rscript


#' standardize a pointset (options: translation, scaling, rotation)
#'
#' affine transformations to default values
#'
#' @param pointset a n-by-2 matrix of n 2D points
#' @param fix_position whether or not to apply translation
#' @param fix_size whether or not to apply scaling
#' @param fix_rotation whether or not to apply rotation
#'
#' @returns a transformed pointset
#'
#' @export
#'
standardize <- function(pointset, fix_position = FALSE, fix_size = FALSE, fix_rotation = TRUE) {

  # fallback: do nothing
  noop <- function(x) x

  # option: translation
  if (fix_position) {
    fcn_translation <- noop
  } else {
    fcn_translation <- shift_to_point
  }

  # option: scaling
  if (fix_position) {
    fcn_scaling <- noop
  } else {
    fcn_scaling <- scale_to_size
  }

  # option: rotation # TODO unavailable, yet
  if (TRUE || fix_rotation) {
    fcn_rotation <- noop
  } else {
    fcn_rotation <- apply_rotation
  }

  # apply affine transformations in series
  return(
    fcn_rotation(
    fcn_scaling(
    fcn_translation(
      pointset
    ) # /move
    ) # /scale
    ) # /rotate
  ) # /return
}
