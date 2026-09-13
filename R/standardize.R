#!/usr/bin/env Rscript


#' standardize a pointset (options: translation, scaling, rotation)
#'
#' affine transformations to default values
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
    fcn_translation <- rpgt2d::shift_to_point
  }

  # option: scaling
  if (fix_position) {
    fcn_scaling <- noop
  } else {
    fcn_scaling <- rpgt2d::scale_to_size
  }

  # option: rotation # TODO unavailable, yet
  if (TRUE || fix_rotation) {
    fcn_rotation <- noop
  } else {
    fcn_rotation <- rpgt2d::rotate_to_orientation
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
