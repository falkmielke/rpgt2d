#!/usr/bin/env Rscript


#' shift points along a vector
#'
#' @param pointset a n-by-2 matrix of n 2D points
#' @param shift a 2D vector (directed translation offset)
#'              along which points are moved
#'
#' @returns a transformed pointset
#'
#' @export
#'
apply_translation <- function(pointset, shift) {
  pointset_shifted <- sweep(
      pointset,
      MARGIN = 2,
      STATS = as.numeric(shift),
      FUN = "+"
    )
  return(pointset_shifted)
} # /apply_translation


#' alias for `apply_translation`
#'
#' @inheritParams apply_translation
#'
#' @export
#'
move <- apply_translation
